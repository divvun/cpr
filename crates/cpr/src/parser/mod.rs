mod directive;
mod expr;
mod rangeset;
mod strand;
mod utils;

use directive::Directive;
use rangeset::RangeSet;
use thiserror::Error;

use custom_debug_derive::CustomDebug;
use expr::{Expr, TokenStream};
use lang_c::{ast::Expression, driver, env::Env};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt, io,
    path::{Path, PathBuf},
};
use strand::{Atom, Strand};

/// A C token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Keyword(String),
    Identifier(String),
    Punctuator(Punctuator),
    Integer(i64),
    StringLiteral(String),
    Whitespace,
}

impl From<Punctuator> for Token {
    fn from(p: Punctuator) -> Self {
        Self::Punctuator(p)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Keyword(s) | Self::Identifier(s) => f.write_str(s)?,
            Self::Punctuator(p) => write!(f, "{}", (*p as u8) as char)?,
            Self::Integer(i) => write!(f, "{}", i)?,
            Self::StringLiteral(s) => write!(f, "{:?}", s)?,
            Self::Whitespace => f.write_str(" ")?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Punctuator {
    Bang = b'!',
    Percent = b'%',
    Circumflex = b'^',
    Ampersand = b'&',
    Star = b'*',
    ParenOpen = b'(',
    ParenClose = b')',
    Minus = b'-',
    Plus = b'+',
    Equal = b'=',
    CurlyOpen = b'{',
    CurlyClose = b'}',
    Pipe = b'|',
    Tilde = b'~',
    SquareOpen = b'[',
    SquareClose = b']',
    Backslash = b'\\',
    Semicolon = b';',
    SingleQuote = b'\'',
    Colon = b':',
    DoubleQuote = b'"',
    AngleOpen = b'<',
    AngleClose = b'>',
    Question = b'?',
    Comma = b',',
    Dot = b'.',
    Slash = b'/',
    Hash = b'#',
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefineArguments {
    names: Vec<String>,
    has_trailing: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Define {
    Value {
        name: String,
        value: TokenStream,
    },
    Replacement {
        name: String,
        args: DefineArguments,
        value: TokenStream,
    },
}

#[derive(Clone)]
pub struct Context {
    defines: HashMap<String, Vec<(Expr, Define)>>,
    blacklist: HashSet<String>,
}

pub enum SymbolState<'a> {
    Unknown,
    Blacklisted,
    Unconditional(&'a Define),
}

impl Context {
    pub fn new() -> Self {
        let mut blacklist = HashSet::new();
        blacklist.insert("__cplusplus".to_string());

        let res = Context {
            defines: HashMap::new(),
            blacklist,
        };
        res
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
        if self.blacklist.contains(name) {
            return SymbolState::Blacklisted;
        }
        if let Some(defs) = self.defines.get(&*name) {
            // only one def...
            if let [(expr, def)] = &defs[..] {
                // and it's unconditional...
                if matches!(expr, Expr::True) {
                    return SymbolState::Unconditional(&def);
                }
            }
        }
        SymbolState::Unknown
    }
}

impl Define {
    fn name(&self) -> &str {
        match self {
            Define::Value { name, .. } => name,
            Define::Replacement { name, .. } => name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Include {
    System(PathBuf),
    Quoted(PathBuf),
    Expression(Expression),
}

impl PartialOrd for Include {
    fn partial_cmp(&self, other: &Include) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Include::System(ref x), Include::System(ref y)) => x.partial_cmp(y),
            (Include::Quoted(ref x), Include::Quoted(ref y)) => x.partial_cmp(y),
            (Include::System(_), Include::Quoted(_)) => Some(std::cmp::Ordering::Greater),
            (Include::Quoted(_), Include::System(_)) => Some(std::cmp::Ordering::Less),
            (Include::Expression(_), Include::Expression(_)) => Some(std::cmp::Ordering::Equal),
            (_, Include::Expression(_)) => Some(std::cmp::Ordering::Greater),
            (Include::Expression(_), _) => Some(std::cmp::Ordering::Less),
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
            Include::System(ref path) => Self::resolve_system(path, system_paths),
            Include::Quoted(ref path) => {
                // Fallback to system lookup
                Self::resolve_quoted(path, quoted_paths, working_path)
                    // Fallback to system lookup
                    .or_else(|| Self::resolve_system(path, system_paths))
            }
            Include::Expression(ref expr) => {
                unimplemented!("Expression: {:?}", expr);
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
        let mut def_ranges =
            RangeSet::<TokenStream>::new(vec![Token::Keyword("true".into())].into());
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

    fn atoms(&self, ctx: &Context) -> VecDeque<Atom<'_>> {
        let lines = self.source.lines().collect::<Vec<&str>>();

        self.def_ranges
            .iter()
            .filter_map(|(range, expr)| {
                let expr = expr.parse().constant_fold(ctx).simplify();

                if matches!(expr, Expr::False) {
                    log::debug!("Eliminating range {:?} (always-false)", range);
                    return None;
                }

                let mut region_lines = Vec::new();
                'each_line: for &line in &lines[range] {
                    let line = line.trim();
                    if line.is_empty() {
                        continue;
                    }
                    let res =
                        directive::parser::directive(line).expect("should parse all directives");
                    if let Some(directive) = res {
                        match directive {
                            Directive::Define(_) | Directive::Undefine(_) => {
                                // leave them in!
                                log::debug!("In atoms, found directive: {:?}", directive);
                            }
                            _ => {
                                // skip'em
                                continue 'each_line;
                            }
                        }
                    }
                    region_lines.push(line);
                }

                if region_lines.is_empty() {
                    return None;
                } else {
                    return Some(Atom {
                        lines: region_lines,
                        expr: expr.clone(),
                    });
                }
            })
            .collect()
    }

    pub fn chunkify(
        &self,
        deps: &[&ChunkedUnit],
        init_ctx: &Context,
    ) -> Result<ChunkedUnit, Error> {
        let mut init_ctx = init_ctx.clone();

        let mut env = Env::with_msvc();
        for dep in deps {
            for typename in &dep.typenames {
                env.add_typename(typename)
            }
            init_ctx.extend(&dep.ctx);
        }

        let mut atom_queue = self.atoms(&init_ctx);
        let mut strands: Vec<Strand> = Vec::new();
        let mut strand = Strand::new();

        'knit: loop {
            log::debug!("Knitting...");

            let mut must_finish = false;
            match atom_queue.pop_front() {
                Some(atom) => strand.push(atom),
                None => must_finish = true,
            }

            if strand.len() > 4 {
                panic!(
                    "Trying to knit too deep, current strand =\n{:#?}",
                    strand.expand_atoms(&init_ctx, &mut strand.all_atoms())
                );
            }

            if strand.is_empty() {
                if must_finish {
                    log::debug!("Finished with empty strand");
                    break 'knit;
                }
                log::debug!("Strand empty so far, continuing");
                continue;
            }

            // rebuild context up to this point
            let mut ctx = init_ctx.clone();
            for strand in &strands {
                ctx = strand.chunks(&mut env, &ctx).1;
            }

            if strand.has_complete_variants(&mut env, &ctx) {
                log::debug!("Strand complete (len {})", strand.len());
                strands.push(strand);

                log::debug!("Resetting strand..");
                strand = Strand::new();
            } else {
                if must_finish {
                    log::debug!(
                        "Ran out of atoms while trying to make a complete strand (got {} so far)",
                        strand.len()
                    );

                    match strands.pop() {
                        Some(prev) => {
                            log::debug!(
                                "Trying to backtrack by extending previous strand (len {})",
                                prev.len()
                            );
                            atom_queue.extend(strand.into_atoms());
                            strand = prev;
                            continue 'knit;
                        }
                        None => {
                            log::debug!("No previous strand, can't backtrack");
                            return Err(Error::CouldNotKnit(
                                strand
                                    .all_atoms()
                                    .map(|atom| {
                                        format!("// ~> {:?}\n{}", atom.expr, atom.lines.join("\n"))
                                    })
                                    .collect::<Vec<_>>()
                                    .join("\n"),
                            ));
                        }
                    }
                }
            }
        }

        let mut ctx = init_ctx.clone();
        let mut chunks = Vec::new();
        for strand in &strands {
            let (chunks2, ctx2) = strand.chunks(&mut env, &ctx);
            chunks.extend(chunks2);
            ctx = ctx2;
        }

        let unit = ChunkedUnit {
            chunks,
            ctx,
            typenames: env.typenames.drain(..).next().unwrap(),
        };
        Ok(unit)
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
        parser.parse_all()?;

        Ok(parser)
    }

    /// Find a file on disk corresponding to an `Include`, read it
    fn read_include(&self, include: &Include) -> Result<String, Error> {
        log::debug!("=== {:?} ===", include);

        let path =
            match include.resolve(&*self.system_paths, &self.quoted_paths, &self.working_path) {
                Some(v) => v,
                None => return Err(Error::NotFound(include.clone())),
            };

        log::debug!("=== {:?} ===", &path);

        Ok(std::fs::read_to_string(&path)?)
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
mod test_parse;

#[cfg(test)]
mod test_chunks;

#[cfg(test)]
mod test_expr;
