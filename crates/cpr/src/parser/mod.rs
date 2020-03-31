mod directive;
mod expr;
mod rangeset;
mod utils;

use directive::Directive;
use rangeset::RangeSet;
use thiserror::Error;

use custom_debug_derive::CustomDebug;
use expr::{Expr, TokenStream};
use lang_c::{driver, env::Env};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt, io,
    path::{Path, PathBuf},
};

/// A C token
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Token {
    /// `##`
    Paste,
    /// `#@`
    Charize,
    /// `#`
    Stringize,
    Defined,
    /// whitespace
    WS,
    /// punctuation
    Pun(char),
    Name(String),
    /// integer
    Int(i64),
    /// string
    Str(String),
}

impl From<char> for Token {
    fn from(c: char) -> Self {
        Self::Pun(c)
    }
}

impl Token {
    fn name(s: &str) -> Self {
        Self::Name(s.to_string())
    }

    fn int(i: i64) -> Self {
        Self::Int(i)
    }

    fn bool(b: bool) -> Self {
        Self::int(if b { 1 } else { 0 })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Paste => f.write_str("##"),
            Token::Charize => f.write_str("#@"),
            Token::Stringize => f.write_str("#"),
            Token::Defined => f.write_str("defined"),
            Token::WS => f.write_str(" "),
            Token::Pun(c) => write!(f, "{}", c),
            Token::Name(n) => write!(f, "{}", n),
            Token::Int(i) => write!(f, "{}", i),
            Token::Str(s) => write!(f, "{:?}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroParams {
    names: Vec<String>,
    has_trailing: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Define {
    ObjectLike {
        name: String,
        value: TokenStream,
    },
    FunctionLike {
        name: String,
        params: MacroParams,
        value: TokenStream,
    },
}

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Include {
    System(PathBuf),
    Quoted(PathBuf),
    TokenStream(TokenStream),
}

impl fmt::Display for Include {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Include::System(p) => write!(f, "<{}>", p.to_string_lossy()),
            Include::Quoted(p) => write!(f, r#""{}""#, p.to_string_lossy()),
            Include::TokenStream(ts) => write!(f, "{}", ts),
        }
    }
}

impl Include {
    #[inline]
    fn resolve_system(candidate: &Path, system_paths: &[PathBuf]) -> Option<PathBuf> {
        for path in system_paths {
            let merged = path.join(candidate);
            if merged.exists() {
                return Some(merged);
            }
        }
        None
    }

    #[inline]
    fn resolve_quoted(
        candidate: &Path,
        quoted_paths: &[PathBuf],
        working_path: &Path,
    ) -> Option<PathBuf> {
        // Check local path first
        let merged = working_path.join(candidate);
        if merged.exists() {
            return Some(merged);
        }

        // Check quoted paths
        for path in quoted_paths {
            let merged = path.join(candidate);
            if merged.exists() {
                return Some(merged);
            }
        }
        None
    }

    fn resolve(
        &self,
        system_paths: &[PathBuf],
        quoted_paths: &[PathBuf],
        working_path: &Path,
    ) -> Option<PathBuf> {
        match self {
            Include::System(path) => Self::resolve_system(path, system_paths),
            Include::Quoted(path) => {
                // Fallback to system lookup
                Self::resolve_quoted(path, quoted_paths, working_path)
                    // Fallback to system lookup
                    .or_else(|| Self::resolve_system(path, system_paths))
            }
            Include::TokenStream(tokens) => {
                unimplemented!("tokens: {:?}", tokens);
            }
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
    #[error("C syntax error: {0}")]
    Syntax(SyntaxError),
    #[error("Could not knit atoms together. Source = \n{0}")]
    CouldNotKnit(String),
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

/// One (1) C header, split into define-dependent ranges.
#[derive(Debug)]
pub struct ParsedUnit {
    pub source: String,
    pub def_ranges: RangeSet<TokenStream>,
    pub dependencies: HashMap<Include, TokenStream>,
}

#[derive(CustomDebug)]
pub struct Chunk {
    pub expr: Expr,
    pub source: SourceString,
    #[debug(skip)]
    pub unit: lang_c::ast::TranslationUnit,
}

impl Chunk {
    fn new(parse: driver::Parse, expr: Expr) -> Self {
        Chunk {
            source: SourceString(parse.source),
            unit: parse.unit,
            expr,
        }
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

pub struct ChunkedUnit {
    pub chunks: Vec<Chunk>,
    pub ctx: Context,
    pub typenames: HashSet<String>,
}

impl ParsedUnit {
    /// Go through each line of a source file, handling preprocessor directives
    /// like #if, #ifdef, #include, etc.
    fn parse(source: &str) -> Result<ParsedUnit, Error> {
        let source = utils::process_line_continuations_and_comments(source);

        let mut dependencies = HashMap::new();
        let mut def_ranges = RangeSet::<TokenStream>::new(vec![Token::bool(true)].into());
        let mut n = 0usize;
        let mut last_if: Option<TokenStream> = None;

        for line in source.lines() {
            log::debug!("| {}", line);
            let res = directive::parser::directive(line);
            if let Some(directive) = res.expect("should be able to parse all directives") {
                log::debug!("{}", line);
                log::debug!("{:?}", &directive);

                match directive {
                    Directive::Include(include) => {
                        dependencies.insert(include, def_ranges.last().1.clone());
                    }
                    Directive::If(pred) => {
                        last_if = Some(pred.clone());
                        def_ranges.push((n, pred));
                    }
                    Directive::ElseIf(pred) => {
                        def_ranges.pop(n);
                        let pred = !last_if.clone().expect("elif without last_if") & pred;
                        last_if = Some(pred.clone());
                        def_ranges.push((n, pred));
                    }
                    Directive::Else => {
                        def_ranges.pop(n);
                        let pred = !last_if.clone().expect("else without last_if");
                        def_ranges.push((n, pred));
                    }
                    Directive::EndIf => {
                        def_ranges.pop(n);
                        last_if = None;
                    }
                    Directive::Define(_)
                    | Directive::Undefine(_)
                    | Directive::Error(_)
                    | Directive::Pragma(_)
                    | Directive::Unknown(_, _) => {
                        // leave as-is
                    }
                }
                log::trace!("STACK: {:?}", def_ranges.last());
            }
            n += 1;
        }

        def_ranges.pop(n);

        Ok(ParsedUnit {
            source,
            def_ranges,
            dependencies,
        })
    }
}

#[derive(Debug)]
pub struct Parser {
    system_paths: Vec<PathBuf>,
    quoted_paths: Vec<PathBuf>,
    working_path: PathBuf,
    root: Include,
    ordered_includes: Vec<Include>,
    sources: HashMap<Include, ParsedUnit>,
}

impl Parser {
    /// Builds a new parser starting from the initial file,
    /// parses it and all its dependencies.
    pub fn new(
        initial_file: PathBuf,
        system_paths: Vec<PathBuf>,
        quoted_paths: Vec<PathBuf>,
    ) -> Result<Parser, Error> {
        let file_name = initial_file.file_name().ok_or(Error::InvalidFile)?;
        let working_path = initial_file
            .parent()
            .ok_or(Error::InvalidFile)?
            .to_path_buf();

        let root = Include::Quoted(file_name.into());
        let mut parser = Parser {
            system_paths,
            quoted_paths,
            working_path,
            sources: HashMap::new(),
            ordered_includes: vec![root.clone()],
            root,
        };
        parser.parse_all_2()?;

        Ok(parser)
    }

    /// Find a file on disk corresponding to an `Include`, read it
    fn read_include(&self, include: &Include) -> Result<String, Error> {
        let path =
            match include.resolve(&*self.system_paths, &self.quoted_paths, &self.working_path) {
                Some(v) => v,
                None => return Err(Error::NotFound(include.clone())),
            };
        log::info!("Reading {:?} ===", path);

        Ok(std::fs::read_to_string(&path)?)
    }

    fn parse_all_2(&mut self) -> Result<(), Error> {
        let mut env = Env::with_msvc();
        let mut ctx = Context::new();
        self.parse_2(&mut ctx, &mut env, self.root.clone())?;
        Ok(())
    }

    fn parse_2(&mut self, ctx: &mut Context, env: &mut Env, incl: Include) -> Result<(), Error> {
        let source = self.read_include(&incl)?;
        let source = utils::process_line_continuations_and_comments(&source);
        let mut lines = source.lines().enumerate();
        let mut block: Vec<String> = Vec::new();

        let mut stack: Vec<(bool, Expr)> = Vec::new();
        fn path_taken(stack: &[(bool, Expr)]) -> bool {
            stack.iter().all(|(b, _)| *b == true)
        }

        fn parse_expr(ctx: &Context, tokens: TokenStream) -> Expr {
            let expr_string = tokens.must_expand_single(ctx).to_string();
            directive::parser::expr(&expr_string).expect("all expressions should parse")
        }

        'each_line: loop {
            let (line_number, line) = match lines.next() {
                Some(line) => line,
                None => break 'each_line,
            };
            let line = line.trim();
            if line.is_empty() {
                continue 'each_line;
            }

            let taken = path_taken(&stack);

            log::debug!("====================================");
            log::debug!("{:?}:{} | {}", incl, line_number, line);
            let dir = directive::parser::directive(line).expect("should parse all directives");
            match dir {
                Some(dir) => {
                    log::debug!("directive | {:?}", dir);
                    match dir {
                        Directive::Include(dep) => {
                            if taken {
                                log::info!(
                                    "{}:{} including {:?}, stack = {:?}",
                                    incl,
                                    line_number,
                                    dep,
                                    stack
                                );
                                self.parse_2(ctx, env, dep)?;
                            } else {
                                log::debug!("path not taken, not including");
                            }
                        }
                        Directive::Define(def) => {
                            if taken {
                                log::info!("{}:{} defining {}", incl, line_number, def.name());
                                match &def {
                                    Define::ObjectLike { value, .. } => {
                                        log::debug!("...to {:?}", value);
                                    }
                                    _ => {}
                                }
                                ctx.push(Expr::bool(true), def);
                            } else {
                                log::debug!("path not taken, not defining");
                            }
                        }
                        Directive::Undefine(name) => {
                            if taken {
                                log::debug!("undefining {}", name);
                                ctx.pop(&name);
                            } else {
                                log::debug!("path not taken, not undefining");
                            }
                        }
                        Directive::If(tokens) => {
                            let expr = parse_expr(ctx, tokens);
                            let tup = (expr.truthy(), expr);
                            log::debug!("if | {:?}", tup);
                            stack.push(tup)
                        }
                        Directive::Else => {
                            let mut tup = stack.pop().expect("else without if");
                            tup.0 = !tup.0;
                            log::debug!("else | {:?}", tup);
                            stack.push(tup);
                        }
                        Directive::EndIf => {
                            stack.pop().expect("endif without if");
                        }
                        _ => {
                            log::debug!("todo: handle that directive");
                        }
                    }
                }
                None => {
                    if !taken {
                        log::debug!("not taken | {}", line);
                        continue 'each_line;
                    }

                    log::debug!("not a directive");
                    let tokens =
                        directive::parser::token_stream(line).expect("should tokenize everything");
                    log::debug!("tokens = {:?}", tokens);
                    let line = tokens.must_expand_single(ctx).to_string();
                    log::debug!("expanded line | {}", line);

                    block.push(line);
                    let block_str = block.join("\n");
                    match lang_c::parser::declaration(&block_str, env) {
                        Ok(node) => {
                            log::debug!("parse result (input len={}) | {:?}", block.len(), node);
                            block.clear();
                        }
                        Err(e) => {
                            log::debug!("parse error: {:?}", e);
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
            panic!("Unprocessed lines: {:#?}", unprocessed_lines);
        }

        log::debug!("=== {:?} (end) ===", incl);
        Ok(())
    }

    /// Parse the roots and all its included dependencies,
    /// breadth-first.
    fn parse_all(&mut self) -> Result<(), Error> {
        let mut unit_queue = VecDeque::new();
        unit_queue.push_back(self.root.clone());

        while let Some(work_unit) = unit_queue.pop_front() {
            log::debug!("## WORK UNIT: {:?}", &work_unit);

            if self.sources.contains_key(&work_unit) {
                continue;
            }

            let source = self.read_include(&work_unit)?;
            let parsed_unit = ParsedUnit::parse(&source[..])?;

            log::trace!("{:?}", &parsed_unit);

            for include in parsed_unit.dependencies.keys() {
                self.ordered_includes.push(include.clone());
                unit_queue.push_back(include.clone());
            }

            self.sources.insert(work_unit, parsed_unit);
        }

        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Include, &ParsedUnit)> {
        // TODO: that's a hack, find something better.
        self.ordered_includes
            .iter()
            .map(move |inc| (inc, self.sources.get(inc).unwrap()))
    }
}

#[cfg(test)]
mod test_expr;
