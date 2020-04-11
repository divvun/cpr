//! Drives the process of preprocessing and parsing C header and its dependencies.

mod expand;
pub mod grammar;
mod utils;

mod file_source_provider;
pub use file_source_provider::FileSourceProvider;

use expand::{ExpandError, Expandable};
use grammar::{Define, Directive, Expr, Include, IncludeDirective, Token, TokenSeq};
use thiserror::Error;

use indexmap::IndexSet;
use lang_c::{ast as c_ast, driver, env::Env};
use std::{
    collections::{HashMap, HashSet},
    fmt, io,
    path::PathBuf,
};

#[derive(Debug, Clone)]
pub struct Context {
    defines: HashMap<String, Vec<(Expr, Define)>>,
    unknowns: HashSet<String>,
}

#[derive(Debug)]
pub enum SymbolState<'a> {
    Unknown,
    Undefined,
    Defined((&'a Expr, &'a Define)),
    MultipleDefines(Vec<(&'a Expr, &'a Define)>),
}

impl Context {
    pub fn new() -> Self {
        let res = Context {
            defines: HashMap::new(),
            unknowns: HashSet::new(),
        };
        res
    }

    pub fn add_unknown(&mut self, unknown: &str) {
        self.unknowns.insert(unknown.into());
    }

    pub fn simple_define(&mut self, s: &str) {
        self.push(
            grammar::Expr::bool(true),
            grammar::Define::ObjectLike {
                name: s.to_string(),
                value: vec![grammar::Token::Int(1)].into(),
            },
        );
    }

    pub fn push(&mut self, expr: Expr, def: Define) {
        let name = def.name().to_string();
        let bucket = match self.defines.get_mut(&name) {
            Some(bucket) => bucket,
            None => {
                self.defines.insert(name.clone(), Vec::new());
                self.defines.get_mut(&name).unwrap()
            }
        };
        // TODO: change if we start supporting multiple defines again
        if !bucket.is_empty() {
            log::debug!("re-defining {:?}", name);
            bucket.clear();
        }
        bucket.push((expr, def));
    }

    pub fn pop(&mut self, name: &str) {
        self.defines.remove(name);
    }

    pub fn extend(&mut self, other: &Context) {
        for (_, bucket) in &other.defines {
            for (expr, def) in bucket {
                self.push(expr.clone(), def.clone());
            }
        }
    }

    pub fn lookup(&self, name: &str) -> SymbolState<'_> {
        if self.unknowns.contains(name) {
            return SymbolState::Undefined;
        }
        if let Some(defs) = self.defines.get(&*name) {
            // only one def...
            if let [(expr, def)] = &defs[..] {
                return SymbolState::Defined((&expr, &def));
            } else {
                log::debug!(
                    "Multiple defines are unsupported for now, returning last: {:?}",
                    defs
                );
                let (expr, def) = &defs[defs.len() - 1];
                return SymbolState::Defined((&expr, &def));
            }
        }
        SymbolState::Undefined
    }
}

impl Define {
    fn name(&self) -> &str {
        match self {
            Define::ObjectLike { name, .. } => name,
            Define::FunctionLike { name, .. } => name,
        }
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("invalid file")]
    InvalidFile,
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("utf-8 error: {0}")]
    Utf8(#[from] std::string::FromUtf8Error),
    #[error("include not found: {0:?}")]
    NotFound(Include),
    #[error("include could not be expanded: {0}")]
    IncludeNotExpandable(String),
    #[error("C syntax error: {0}")]
    Syntax(SyntaxError), // has custom From implementation
    #[error("C token expansion error: {0}")]
    Expand(#[from] ExpandError),
}

#[derive(Debug)]
pub struct SyntaxError(pub driver::SyntaxError);

impl From<driver::SyntaxError> for Error {
    fn from(e: driver::SyntaxError) -> Self {
        Self::Syntax(SyntaxError(e))
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq)]
pub struct SourceString(pub String);

impl fmt::Debug for SourceString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\n")?;
        for line in self.0.lines() {
            writeln!(f, "| {}", line)?;
        }
        Ok(())
    }
}

impl AsRef<str> for SourceString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl<T> From<T> for SourceString
where
    T: Into<String>,
{
    fn from(t: T) -> Self {
        Self(t.into())
    }
}

#[derive(Debug)]
pub struct Unit {
    pub path: PathBuf,
    pub dependencies: Vec<Include>,
    pub declarations: Vec<c_ast::ExternalDeclaration>,
}

pub struct Parser {
    provider: Box<dyn SourceProvider>,
    pub includes: IndexSet<Include>,
    pub units: HashMap<Include, Unit>,
    pub ctx: Context,
    pub env: Env,
}

pub trait SourceProvider {
    fn resolve(&self, include: &Include) -> Option<(PathBuf, String)>;
}

impl Parser {
    const MAX_AGGREGATE_LINES: usize = 150;

    /// Builds a new parser starting from the initial file,
    /// parses it and all its dependencies.
    pub fn new(provider: Box<dyn SourceProvider>, ctx: Context, env: Env) -> Self {
        Self {
            provider,
            ctx,
            env,
            includes: Default::default(),
            units: Default::default(),
        }
    }

    pub fn parse_path(&mut self, path: PathBuf) -> Result<(), Error> {
        self.parse_include(Include::Quoted(path))
    }

    pub fn parse_include(&mut self, inc: Include) -> Result<(), Error> {
        self.includes.insert(inc.clone());

        let (path, source) = self
            .provider
            .resolve(&inc)
            .ok_or_else(|| Error::NotFound(inc.clone()))?;

        let source = utils::process_line_continuations_and_comments(&source);
        let mut lines = source.lines().enumerate();
        let mut block: Vec<String> = Vec::new();

        let mut unit = Unit {
            dependencies: vec![],
            declarations: vec![],
            path,
        };

        let mut stack: Vec<(bool, TokenSeq)> = Vec::new();
        let mut if_stack: Vec<Vec<bool>> = Vec::new();

        fn path_taken(stack: &[(bool, TokenSeq)]) -> bool {
            stack.iter().all(|(b, _)| *b == true)
        }

        fn parse_expr(ctx: &Context, tokens: &TokenSeq) -> Expr {
            let expr_string = tokens
                .expand(ctx)
                .expect("all expressions should expand")
                .to_string();
            grammar::expr(&expr_string).unwrap_or_else(|e| {
                panic!(
                    "could not parse expression:\n\n{}\n\ngot error: {:?}",
                    expr_string, e
                )
            })
        }

        'each_line: loop {
            let (lineno, line) = match lines.next() {
                Some(line) => line,
                None => break 'each_line,
            };
            let line = line.trim();
            if line.is_empty() {
                continue 'each_line;
            }

            let taken = path_taken(&stack);

            log::trace!("====================================");
            log::trace!("{:?}:{} | {}", inc, lineno, line);
            let dir = grammar::directive(line).unwrap_or_else(|e| {
                panic!("could not parse directive `{}`\n\ngot error: {:?}", line, e)
            });
            match dir {
                Some(dir) => match dir {
                    Directive::Include(dep) => {
                        if taken {
                            let dep = match dep {
                                IncludeDirective::Complete(dep) => dep,
                                IncludeDirective::Raw(tokens) => {
                                    let expanded = tokens.expand(&self.ctx)?;
                                    let source = expanded.to_string();
                                    match grammar::include(&source)
                                        .expect("includes should all parse or tokenize")
                                    {
                                        IncludeDirective::Complete(dep) => dep,
                                        IncludeDirective::Raw(tokens) => {
                                            return Err(Error::IncludeNotExpandable(
                                                tokens.to_string(),
                                            ));
                                        }
                                    }
                                }
                            };
                            log::info!(
                                "{}:{} including {:?}, stack = {:?}",
                                inc,
                                lineno,
                                dep,
                                stack
                            );
                            self.parse_include(dep)?;
                        } else {
                            log::debug!("path not taken, not including");
                        }
                    }
                    Directive::Define(def) => {
                        if taken {
                            log::debug!("{}:{} defining {}", inc, lineno, def.name());
                            match &def {
                                Define::ObjectLike { value, .. } => {
                                    log::debug!("...to: {}", value);
                                }
                                _ => {}
                            }
                            self.ctx.push(Expr::bool(true), def);
                        } else {
                            log::debug!("{}:{} not defining {}", inc, lineno, def.name());
                        }
                    }
                    Directive::Undefine(name) => {
                        if taken {
                            log::debug!("undefining {}", name);
                            self.ctx.pop(&name);
                        } else {
                            log::debug!("path not taken, not undefining");
                        }
                    }
                    Directive::If(tokens) => {
                        let expr = parse_expr(&self.ctx, &tokens);
                        let truthy = expr.truthy();
                        if_stack.push(vec![truthy]);

                        let tup = (expr.truthy(), tokens);
                        log::debug!("{}:{} if | {} {}", inc, lineno, tup.0, tup.1);
                        stack.push(tup)
                    }
                    Directive::Else => {
                        stack.pop().expect("else without if");
                        let mut v = if_stack.pop().expect("else without if");
                        let branch_taken = v.iter().copied().all(|x| x == false);
                        v.push(branch_taken);

                        let tup = (branch_taken, vec![].into());
                        log::debug!("{}:{} else | {} {}", inc, lineno, tup.0, tup.1);
                        if_stack.push(v);
                        stack.push(tup);
                    }
                    Directive::ElseIf(tokens) => {
                        stack.pop().expect("elseif without if");
                        let mut v = if_stack.pop().expect("elseif without if");
                        let expr = parse_expr(&self.ctx, &tokens);
                        let truthy = expr.truthy();
                        let branch_taken = v.iter().copied().all(|x| x == false) && truthy;
                        v.push(truthy);

                        let tup = (branch_taken, tokens);
                        log::debug!("{}:{} elseif | {} {}", inc, lineno, tup.0, tup.1);
                        if_stack.push(v);
                        stack.push(tup);
                    }
                    Directive::EndIf => {
                        stack.pop().expect("endif without if");
                        if_stack.pop().expect("endif without if");
                        log::debug!("endif");
                    }
                    Directive::Pragma(s) => {
                        log::debug!("{}:{} ignoring pragma: {}", inc, lineno, s);
                    }
                    Directive::Error(s) => {
                        if taken {
                            panic!("{}:{} pragma error: {}", inc, lineno, s);
                        }
                    }
                    Directive::Unknown(a, b) => {
                        log::warn!(
                            "{}:{} ignoring unknown directive: {} {}\n",
                            inc,
                            lineno,
                            a,
                            b
                        );
                    }
                },
                None => {
                    if !taken {
                        log::debug!("{}:{} not taken | {}", inc, lineno, line);
                        continue 'each_line;
                    }

                    let mut tokens =
                        grammar::token_stream(line).expect("should tokenize everything");

                    let mut expanded = tokens.expand(&self.ctx);
                    'aggregate: loop {
                        match expanded {
                            Ok(_) => break 'aggregate,
                            Err(e) if e.needs_more() => {
                                let (_, next_line) =
                                    lines.next().expect("ran out of lines while aggregating");
                                let mut next_tokens = grammar::token_stream(next_line)
                                    .expect("should tokenize everything");
                                tokens.0.push(Token::WS);
                                tokens.0.append(&mut next_tokens.0);
                                expanded = tokens.expand(&self.ctx);
                            }
                            Err(e) => panic!(
                                "while expanding non-directive line\n\n{}\n\ngot error: {}",
                                tokens, e
                            ),
                        }
                    }

                    let line = expanded.unwrap().to_string();
                    log::debug!("expanded line | {}", line);

                    block.push(line);
                    let block_str = block.join("\n");

                    if block_str.trim() == ";" {
                        log::debug!(
                            "{}:{} is a single semi-colon (sloppy macro invocation), ignoring...",
                            inc,
                            lineno
                        );
                        block.clear();
                        continue 'each_line;
                    }

                    if block.len() > Self::MAX_AGGREGATE_LINES {
                        panic!(
                            "suspiciously long block ({} lines):\n\n{}",
                            block.len(),
                            block_str
                        );
                    }

                    match grammar::pragma(&block_str) {
                        Ok(p) => {
                            log::debug!("skipping pragma:\n__pragma{}", p);
                            block.clear();
                            continue 'each_line;
                        }
                        Err(_) => {
                            // continue
                        }
                    }

                    match lang_c::parser::translation_unit(&block_str, &mut self.env) {
                        Ok(mut node) => {
                            unit.declarations
                                .extend(node.0.drain(..).map(|node| node.node));

                            log::debug!("{}:{} parsed C:\n{}", inc, lineno, block_str);
                            block.clear();
                            continue 'each_line;
                        }
                        Err(e) => {
                            log::trace!("parse error (probably incomplete block): {:?}", e);
                        }
                    }
                }
            }
        }

        let unprocessed_lines: Vec<_> = block
            .iter()
            .map(|x| x.trim())
            .filter(|x| !x.is_empty())
            .collect();

        if !unprocessed_lines.is_empty() {
            panic!(
                "Unprocessed lines: {:#?}\n\nParse result: {:#?}",
                unprocessed_lines,
                lang_c::parser::translation_unit(&unprocessed_lines.join("\n"), &mut self.env)
            );
        }

        log::debug!("=== {:?} (end) ===", inc);
        // TODO: merge declarations if it was included several times?
        // this seems *really* unlikely but ohwell
        if self.units.contains_key(&inc) {
            log::debug!("included several times: {:?}", inc);
        } else {
            self.units.insert(inc, unit);
        }
        Ok(())
    }
}

#[cfg(test)]
mod test_expr;
