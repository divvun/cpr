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
use lang_c::{ast as c_ast, driver, env::Env, span::Node};
use std::{collections::HashMap, fmt, io, path::PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LineNo(pub u64);

impl fmt::Display for LineNo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct Location {
    id: FileId,
    lineno: LineNo,
}

impl Location {
    fn display<'a>(&'a self, provider: &'a dyn SourceProvider) -> LocationDisplay<'a> {
        return LocationDisplay {
            loc: self,
            provider,
        };
    }
}

struct LocationDisplay<'a> {
    loc: &'a Location,
    provider: &'a dyn SourceProvider,
}

impl<'a> fmt::Display for LocationDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let info = self.provider.info(self.loc.id).unwrap();
        write!(f, "{}:{}", info.path, self.loc.lineno)
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    defines: HashMap<String, Define>,
}

#[derive(Debug)]
pub enum SymbolState<'a> {
    Undefined,
    Defined(&'a Define),
}

impl Context {
    pub fn new() -> Self {
        let res = Context {
            defines: HashMap::new(),
        };
        res
    }

    pub fn simple_define(&mut self, s: &str) {
        self.push(grammar::Define::ObjectLike {
            name: s.to_string(),
            value: vec![grammar::Token::Int(1)].into(),
        });
    }

    pub fn push(&mut self, def: Define) {
        self.defines.insert(def.name().to_string(), def);
    }

    pub fn pop(&mut self, name: &str) {
        self.defines.remove(name);
    }

    pub fn extend(&mut self, other: &Context) {
        for (_, def) in &other.defines {
            self.push(def.clone());
        }
    }

    pub fn lookup(&self, name: &str) -> SymbolState<'_> {
        if let Some(def) = self.defines.get(name) {
            return SymbolState::Defined(def);
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
    #[error("unknown file ID (internal error)")]
    UnknownFileId,
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
    pub id: FileId,
    pub dependencies: Vec<Include>,
    pub declarations: Vec<UnitDeclaration>,
}

#[derive(Debug)]
pub enum UnitDeclaration {
    External(c_ast::ExternalDeclaration),
    Constant(UnitConstant),
}

#[derive(Debug)]
pub struct UnitConstant {
    pub name: String,
    pub value: c_ast::Constant,
    pub negated: bool,
}

impl From<c_ast::ExternalDeclaration> for UnitDeclaration {
    fn from(v: c_ast::ExternalDeclaration) -> Self {
        UnitDeclaration::External(v)
    }
}

pub struct Parser {
    pub provider: Box<dyn SourceProvider>,
    pub ordered_files: IndexSet<FileId>,
    pub units: HashMap<FileId, Unit>,
    pub ctx: Context,
    pub env: Env,
    pub idgen: IdGenerator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u64);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileInfo {
    /// Unique identifier for file
    pub id: FileId,
    /// Path relative to `dir`
    pub path: FilePath,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FilePath {
    /// Source directory this file was read from
    pub dir: SourceDir,
    /// Path relative from `dir`
    pub rel_path: PathBuf,
}

impl fmt::Display for FilePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.dir.pkg, self.rel_path.display())
    }
}

impl FilePath {
    /// Returns path of source file on disk
    pub fn source_path(&self) -> PathBuf {
        self.dir.path.join(&self.rel_path)
    }

    /// Returns Rust package components, like ["um", "WinTrust.h"]
    pub fn pkg_components(&self) -> Vec<String> {
        let mut res = vec![];
        res.push(self.dir.pkg.to_string());
        for comp in self.rel_path.components() {
            match comp {
                std::path::Component::Prefix(_) => {}
                std::path::Component::RootDir => {}
                std::path::Component::CurDir => {}
                std::path::Component::ParentDir => {}
                std::path::Component::Normal(comp) => {
                    res.push(comp.to_string_lossy().to_string());
                }
            }
        }
        res
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceDir {
    /// Name of top-level rust package for crate
    pub pkg: String,
    /// Absolute filesystem path for source dir
    pub path: PathBuf,
}

pub struct IdGenerator {
    file_id_seed: u64,
}

impl IdGenerator {
    fn new() -> Self {
        Self { file_id_seed: 1 }
    }

    pub fn generate_id(&mut self) -> FileId {
        let res = FileId(self.file_id_seed);
        self.file_id_seed += 1;
        res
    }
}

#[derive(Debug)]
struct Block {
    lines: Vec<(LineNo, TokenSeq)>,
}

impl Block {
    fn new() -> Self {
        Self {
            lines: Default::default(),
        }
    }

    fn clear(&mut self) {
        self.lines.clear();
    }

    fn tokens(&self) -> impl Iterator<Item = &Token> {
        self.lines.iter().flat_map(|(_, ts)| ts.0.iter())
    }

    fn is_balanced(&self) -> bool {
        let mut count = 0;
        for tok in self.tokens() {
            match tok {
                Token::Pun('{') => {
                    count += 1;
                }
                Token::Pun('}') => {
                    count -= 1;
                }
                _ => {}
            }
        }
        count == 0
    }

    fn is_degenerate_macro_invocation(&self) -> bool {
        for tok in self.tokens() {
            match tok {
                Token::Pun(';') => {}
                Token::WS => {}
                _ => return false,
            }
        }
        true
    }

    /// Returns true if only whitespace
    fn is_empty(&self) -> bool {
        !self.tokens().any(|t| !matches!(t, Token::WS))
    }

    fn len(&self) -> usize {
        self.lines.len()
    }

    fn start_line(&self) -> LineNo {
        self.lines
            .first()
            .map(|(l, _t)| *l)
            .unwrap_or_else(|| LineNo(1))
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        for (i, (_, ts)) in self.lines.iter().enumerate() {
            use std::fmt::Write;
            write!(&mut out, "{}", ts).unwrap();
            if i + 1 < self.lines.len() {
                write!(&mut out, "\n").unwrap();
            }
        }
        out
    }
}

pub trait SourceProvider {
    fn resolve(
        &mut self,
        idgen: &mut IdGenerator,
        working_dir: &SourceDir,
        include: &Include,
    ) -> Result<FileId, Error>;
    fn info(&self, id: FileId) -> Option<&FileInfo>;
    fn read(&self, id: FileId) -> Result<String, Error>;
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
            ordered_files: Default::default(),
            units: Default::default(),
            idgen: IdGenerator::new(),
        }
    }

    pub fn parse_file(&mut self, file_id: FileId) -> Result<(), Error> {
        let file_info = self
            .provider
            .info(file_id)
            .ok_or(Error::UnknownFileId)?
            .clone();
        self.ordered_files.insert(file_id.clone());
        let path = &file_info.path;

        let source = self.provider.read(file_id)?;
        let lines = utils::process_line_continuations_and_comments(&source);
        let mut lines = lines.iter();
        let mut block = Block::new();

        let mut unit = Unit {
            id: file_id,
            dependencies: vec![],
            declarations: vec![],
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
            let lineno = *lineno;
            let loc = Location {
                id: file_id,
                lineno,
            };

            macro_rules! loc {
                () => {
                    loc.display(self.provider.as_ref())
                };
            }

            let line = line.trim();
            if line.is_empty() {
                continue 'each_line;
            }

            let taken = path_taken(&stack);

            log::trace!("====================================");
            log::trace!("{} | {}", loc!(), line);
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
                                "{} including {:?} | {}",
                                loc!(),
                                dep,
                                stack
                                    .iter()
                                    .map(|(b, ts)| format!("({})={}", ts, b))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            );

                            // FIXME: working dir is wrong here
                            let dep_id = self.provider.resolve(
                                &mut self.idgen,
                                &file_info.path.dir,
                                &dep,
                            )?;
                            self.parse_file(dep_id)?;
                        } else {
                            log::debug!("path not taken, not including");
                        }
                    }
                    Directive::Define(def) => {
                        if taken {
                            match &def {
                                Define::ObjectLike { value, .. } => {
                                    log::debug!(
                                        "{}:{} defining {} to {}",
                                        path,
                                        lineno,
                                        def.name(),
                                        value
                                    );

                                    let s = value.expand(&self.ctx)?.to_string();
                                    match lang_c::parser::constant_expression(&s, &mut self.env) {
                                        Ok(node) => {
                                            let expr = &node.node;
                                            if let c_ast::Expression::Constant(c) = expr {
                                                unit.declarations.push(UnitDeclaration::Constant(
                                                    UnitConstant {
                                                        name: def.name().to_string(),
                                                        value: c.node.clone(),
                                                        negated: false,
                                                    },
                                                ));
                                            } else if let c_ast::Expression::UnaryOperator(un) =
                                                expr
                                            {
                                                if let c_ast::UnaryOperatorExpression {
                                                    operator:
                                                        Node {
                                                            node: c_ast::UnaryOperator::Minus,
                                                            ..
                                                        },
                                                    operand,
                                                } = &un.node
                                                {
                                                    if let c_ast::Expression::Constant(c) =
                                                        &operand.node
                                                    {
                                                        unit.declarations.push(
                                                            UnitDeclaration::Constant(
                                                                UnitConstant {
                                                                    name: def.name().to_string(),
                                                                    value: c.node.clone(),
                                                                    negated: true,
                                                                },
                                                            ),
                                                        )
                                                    }
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {
                                    log::debug!(
                                        "{}:{} defining {} (function-like)",
                                        path,
                                        lineno,
                                        def.name()
                                    );
                                }
                            }
                            self.ctx.push(def);
                        } else {
                            log::debug!("{}:{} not defining {}", path, lineno, def.name());
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
                        log::debug!("{}:{} if | {} {}", path, lineno, tup.0, tup.1);
                        stack.push(tup)
                    }
                    Directive::Else => {
                        stack.pop().expect("else without if");
                        let mut v = if_stack.pop().expect("else without if");
                        let branch_taken = v.iter().copied().all(|x| x == false);
                        v.push(branch_taken);

                        let tup = (branch_taken, vec![].into());
                        log::debug!("{}:{} else | {} {}", path, lineno, tup.0, tup.1);
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
                        log::debug!("{} elseif | {} {}", loc!(), tup.0, tup.1);
                        if_stack.push(v);
                        stack.push(tup);
                    }
                    Directive::EndIf => {
                        stack.pop().expect("endif without if");
                        if_stack.pop().expect("endif without if");
                        log::debug!("endif");
                    }
                    Directive::Pragma(s) => {
                        log::debug!("{} ignoring pragma: {}", loc!(), s);
                    }
                    Directive::Error(s) => {
                        if taken {
                            panic!("{} pragma error: {}", loc!(), s);
                        }
                    }
                    Directive::Unknown(a, b) => {
                        log::warn!("{} ignoring unknown directive: {} {}\n", loc!(), a, b);
                    }
                },
                None => {
                    if !taken {
                        log::debug!("{} not taken | {}", loc!(), line);
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
                                    .unwrap_or_else(|e| {
                                        log::error!("Could not tokenize input line:");
                                        log::error!("| {:?}", next_line);
                                        log::error!("  {}^", " ".repeat(e.location.offset));
                                        panic!("Failed to tokenize");
                                    });
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

                    block.lines.push((lineno, expanded?));

                    if block.is_degenerate_macro_invocation() {
                        log::debug!(
                            "{} is a single semi-colon (sloppy macro invocation), ignoring...",
                            loc!(),
                        );
                        block.clear();
                        continue 'each_line;
                    }

                    if block.len() > Self::MAX_AGGREGATE_LINES {
                        log::error!("Suspiciously long block ({} lines), aborting", block.len());
                        break 'each_line;
                    }

                    if block.is_balanced() {
                        let block_str = block.as_string();
                        match grammar::pragma(&block_str) {
                            Ok(p) => {
                                log::debug!("skipping pragma: __pragma{}", p);
                                block.clear();
                                continue 'each_line;
                            }
                            Err(_e) => {
                                // log::debug!("pragma error: {:?}", e);
                            }
                        }

                        match lang_c::c_parser::translation_unit(
                            &block_str,
                            &mut self.env.for_parser(),
                        ) {
                            Ok(mut node) => {
                                unit.declarations
                                    .extend(node.0.drain(..).map(|node| node.node.into()));

                                log::debug!("{} parsed C:\n{}", loc!(), block_str);
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
        }

        if !block.is_empty() {
            log::error!("In {}:", file_info.path);
            log::trace!("Full tokens: {:?}", block.tokens().collect::<Vec<_>>());
            let input = block.as_string();
            let err = lang_c::parser::translation_unit(&input, &mut self.env)
                .err()
                .unwrap();

            let padding = 8 + 3;
            for (logical_lineno, (lineno, s)) in block.lines.iter().enumerate() {
                log::error!("{:>8} | {}", lineno.0, s);
                if logical_lineno + 1 == err.line {
                    log::error!(
                        "{}^ expected {:?}",
                        " ".repeat(err.column - 1 + padding),
                        err.expected
                    );
                }
            }
            panic!("Some lines couldn't be processed.");
        }

        log::debug!("=== {} (end) ===", file_info.path);
        // TODO: merge declarations if it was included several times?
        // this seems *really* unlikely but ohwell
        if self.units.contains_key(&file_id) {
            log::debug!("included several times: {:?}", file_info.path);
        } else {
            self.units.insert(file_id, unit);
        }
        Ok(())
    }
}

#[cfg(test)]
mod test_expr;
