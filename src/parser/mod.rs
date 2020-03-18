mod directive;
mod rangeset;
mod utils;

use directive::{Directive, PreprocessorIdent};
use quine_mc_cluskey as qmc;
use rangeset::RangeSet;
use thiserror::Error;

use custom_debug_derive::CustomDebug;
use hashbrown::HashMap;
use lang_c::ast::Expression;
use regex::Regex;
use std::{
    cmp::Ordering,
    collections::VecDeque,
    fmt, io,
    ops::{BitAnd, BitOr, Not},
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum Define {
    Value {
        name: String,
        value: Option<String>,
    },
    Replacement {
        name: String,
        args: Vec<String>,
        value: String,
    },
}

impl Define {
    fn name(&self) -> &str {
        match self {
            Define::Value { name, .. } => &*name,
            Define::Replacement { name, .. } => &*name,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    True,
    False,
    Value(String),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),
}

pub struct Terms {
    map: HashMap<Expr, u8>,
}

impl Terms {
    const MAX_TERMS: usize = 12;

    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn add(&mut self, e: &Expr) -> qmc::Bool {
        use Expr::*;

        qmc::Bool::Term(match e {
            Value(_) => match self.map.get(e) {
                Some(&t) => t,
                None => {
                    let next_term = self.map.len();
                    if next_term >= Self::MAX_TERMS {
                        panic!("refusing to add more than {} terms", Self::MAX_TERMS);
                    }
                    let next_term = next_term as u8;
                    self.map.insert(e.clone(), next_term as u8);
                    next_term
                }
            },
            _ => panic!("can't add expr to terms: {:#?}", e),
        })
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;

        match self {
            True => write!(f, "true"),
            False => write!(f, "false"),
            Value(s) => write!(f, "{}", s),
            And(c) => {
                write!(f, "(")?;
                for (i, v) in c.iter().enumerate() {
                    match i {
                        0 => write!(f, "{:?}", v),
                        _ => write!(f, " & {:?}", v),
                    }?;
                }
                write!(f, ")")
            }
            Or(c) => {
                write!(f, "(")?;
                for (i, v) in c.iter().enumerate() {
                    match i {
                        0 => write!(f, "{:?}", v),
                        _ => write!(f, " | {:?}", v),
                    }?;
                }
                write!(f, ")")
            }
            Not(v) => write!(f, "(!{:?})", v),
        }
    }
}

impl PreprocessorIdent for Expr {
    fn ident(&self) -> Vec<String> {
        use Expr::*;

        match self {
            Value(x) => vec![x.clone()],
            And(c) | Or(c) => {
                let mut res = Vec::new();
                for v in c {
                    res.append(&mut v.ident());
                }
                res
            }
            _ => vec![],
        }
    }
}

impl Ord for Expr {
    fn cmp(&self, other: &Self) -> Ordering {
        use Expr::*;

        match self {
            True => match other {
                True => Ordering::Equal,
                _ => Ordering::Less,
            },
            False => match other {
                True => Ordering::Greater,
                False => Ordering::Equal,
                _ => Ordering::Less,
            },
            Value(v1) => match other {
                True | False => Ordering::Greater,
                Value(v2) => v1.cmp(v2),
                _ => Ordering::Less,
            },
            Not(v1) => match other {
                True | False | Value(_) => Ordering::Greater,
                Not(v2) => v1.cmp(v2),
                _ => Ordering::Less,
            },
            And(c) => match other {
                True | False | Value(_) | Not(_) => Ordering::Greater,
                And(c2) => c.cmp(c2),
                _ => Ordering::Less,
            },
            Or(c) => match other {
                True | False | Value(_) | Not(_) | And(_) => Ordering::Greater,
                Or(c2) => c.cmp(c2),
            },
        }
    }
}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Expr {
    fn combine(vec: &[Expr]) -> Expr {
        use Expr::*;
        vec.iter().fold(True, |acc, cur| acc & cur.clone())
    }

    fn satisfies(&self, defines: &[Define]) -> bool {
        use Expr::*;
        match self {
            True => true,
            False => false,
            Value(v) => defines.iter().map(|x| x.name()).any(|n| n == v),
            And(c) => c.iter().all(|v| v.satisfies(defines)),
            Or(c) => c.iter().any(|v| v.satisfies(defines)),
            Not(v) => !v.satisfies(defines),
        }
    }

    fn reduce(&self) -> Expr {
        let mut terms = Terms::new();
        let input = self.as_bool(&mut terms);
        let mut output = input.simplify();
        assert_eq!(output.len(), 1);
        let output = output
            .pop()
            .expect("simplification should yield at least one term");
        Self::from_bool(output, &terms)
    }

    fn as_bool(&self, terms: &mut Terms) -> qmc::Bool {
        use qmc::Bool;
        use Expr::*;

        match self {
            True => Bool::True,
            False => Bool::False,
            And(v) => Bool::And(v.into_iter().map(|v| v.as_bool(terms)).collect()),
            Or(v) => Bool::Or(v.into_iter().map(|v| v.as_bool(terms)).collect()),
            Not(c) => Bool::Not(Box::new(c.as_bool(terms))),
            Value(_) => terms.add(self),
        }
    }

    fn from_bool(v: qmc::Bool, terms: &Terms) -> Expr {
        use qmc::Bool;
        use Expr::*;

        match v {
            Bool::True => True,
            Bool::False => False,
            Bool::And(c) => And(c.into_iter().map(|v| Self::from_bool(v, terms)).collect()),
            Bool::Or(c) => Or(c.into_iter().map(|v| Self::from_bool(v, terms)).collect()),
            Bool::Not(v) => Not(Box::new(Self::from_bool(*v, terms))),
            Bool::Term(t) => {
                // todo: reverse terms once
                for (k, &v) in &terms.map {
                    if v == t {
                        return k.clone();
                    }
                }
                panic!("unknown term: {}", t)
            }
        }
    }

    fn sort(&self) -> Expr {
        use Expr::*;

        match self {
            And(c) => {
                let mut c: Vec<_> = c.iter().map(|e| e.sort()).collect();
                c.sort();
                And(c)
            }
            Or(c) => {
                let mut c: Vec<_> = c.iter().map(|e| e.sort()).collect();
                c.sort();
                Or(c)
            }
            Not(v) => !v.sort(),
            v => v.clone(),
        }
    }
}

impl<T> From<T> for Expr
where
    T: AsRef<str>,
{
    fn from(v: T) -> Self {
        Expr::Value(v.as_ref().into())
    }
}

impl Default for Expr {
    fn default() -> Expr {
        Expr::True
    }
}

impl BitAnd for Expr {
    type Output = Expr;

    fn bitand(self, rhs: Expr) -> Self::Output {
        use std::iter::once;
        use Expr::*;

        match (self, rhs) {
            (_, False) | (False, _) => False,
            (v, True) | (True, v) => v,
            (And(l), And(r)) => And(l.into_iter().chain(r.into_iter()).collect()),
            (And(c), v) | (v, And(c)) => And(c.into_iter().chain(once(v)).collect()),
            (l, r) => And(vec![l, r]),
        }
    }
}

impl BitOr for Expr {
    type Output = Expr;

    fn bitor(self, rhs: Expr) -> Self::Output {
        use std::iter::once;
        use Expr::*;

        match (self, rhs) {
            (_, True) | (True, _) => True,
            (v, False) | (False, v) => v,
            (Or(l), Or(r)) => Or(l.into_iter().chain(r.into_iter()).collect()),
            (Or(c), v) | (v, Or(c)) => Or(c.into_iter().chain(once(v)).collect()),
            (l, r) => Or(vec![l, r]),
        }
    }
}

impl Not for Expr {
    type Output = Expr;

    fn not(self) -> Self::Output {
        use Expr::*;

        match self {
            Not(v) => *v,
            v => Not(Box::new(v)),
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
}

#[derive(Debug)]
pub struct SyntaxError(pub lang_c::driver::SyntaxError);

impl From<lang_c::driver::SyntaxError> for Error {
    fn from(e: lang_c::driver::SyntaxError) -> Self {
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
    source: String,
    def_ranges: RangeSet<Expr>,
    dependencies: HashMap<Include, Expr>,
}

#[derive(CustomDebug)]
pub struct Chunk {
    pub expr: Expr,
    pub source: SourceString,
    #[debug(skip)]
    pub unit: lang_c::ast::TranslationUnit,
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

impl From<&str> for SourceString {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl ParsedUnit {
    /// Go through each line of a source file, handling preprocessor directives
    /// like #if, #ifdef, #include, etc.
    fn parse(source: &str) -> Result<ParsedUnit, Error> {
        let source = utils::process_line_continuations_and_comments(source);

        let mut dependencies = HashMap::new();
        let mut def_ranges = RangeSet::<Expr>::new();
        let mut n = 0usize;
        let mut last_if: Option<Expr> = None;

        for line in source.lines() {
            if let Some(directive) = directive::parse_directive(line) {
                log::debug!("{}", line);
                log::debug!("{:?}", &directive);

                match directive {
                    Directive::Include(include) => {
                        dependencies.insert(include, def_ranges.last().1.clone());
                    }
                    Directive::If(expr) => {
                        panic!("CExpr not supported: {:?}", expr);
                        // let pred = Expr::CExpr(expr);
                        // last_if = Some(pred.clone());
                        // def_ranges.push((n, pred));
                    }
                    Directive::IfDefined(name) => {
                        let pred = Expr::Value(name);
                        last_if = Some(pred.clone());
                        def_ranges.push((n, pred));
                    }
                    Directive::IfNotDefined(name) => {
                        let pred = !Expr::Value(name);
                        last_if = Some(pred.clone());
                        def_ranges.push((n, pred));
                    }
                    Directive::ElseIf(value) => {
                        panic!("CExpr not supported {:?}", value);
                        // def_ranges.pop(n);
                        // let pred = Expr::CExpr(value);
                        // last_if = Some(pred.clone());
                        // def_ranges.push((n, pred));
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

    pub fn chunks(&self) -> Result<Vec<Chunk>, Error> {
        let lines = self.source.lines().collect::<Vec<&str>>();
        let directive_pattern = Regex::new(r"^\s*#").unwrap();

        let config = lang_c::driver::Config {
            cpp_command: "".into(),
            cpp_options: Vec::new(),
            flavor: lang_c::driver::Flavor::MsvcC11,
        };

        let mut res = Vec::new();

        for (range, expr) in self.def_ranges.iter() {
            let mut chunk_lines = Vec::new();
            for &line in &lines[range] {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                if directive_pattern.is_match(line) {
                    continue;
                }
                chunk_lines.push(line);
            }

            if chunk_lines.is_empty() {
                continue;
            }

            let chunk_source = chunk_lines.join("\n");
            let lang_c::driver::Parse { source, unit } =
                lang_c::driver::parse_preprocessed(&config, chunk_source)?;

            res.push(Chunk {
                source: SourceString(source),
                unit,
                expr: expr.clone(),
            });
        }

        Ok(res)
    }
}

#[derive(Debug)]
pub struct Parser {
    system_paths: Vec<PathBuf>,
    quoted_paths: Vec<PathBuf>,
    working_path: PathBuf,
    root: Include,
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
                unit_queue.push_back(include.clone());
            }

            self.sources.insert(work_unit, parsed_unit);
        }

        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Include, &ParsedUnit)> {
        self.sources.iter()
    }
}

#[cfg(test)]
mod test_parse;

#[cfg(test)]
mod test_chunks;

#[cfg(test)]
mod test_expr;
