mod directive;
mod rangeset;
mod utils;

use directive::{Directive, PreprocessorIdent};
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
    CExpr(Expression),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;

        match self {
            True => write!(f, "true"),
            False => write!(f, "false"),
            Value(s) => write!(f, "{}", s),
            And(l, r) => write!(f, "({:?} & {:?})", l, r),
            Or(l, r) => write!(f, "({:?} | {:?})", l, r),
            Not(v) => write!(f, "(!{:?})", v),
            CExpr(e) => write!(f, "(expr({:?}))", e),
        }
    }
}

impl PreprocessorIdent for Expr {
    fn ident(&self) -> Vec<String> {
        use Expr::*;

        match self {
            Value(x) => vec![x.clone()],
            CExpr(x) => x.ident(),
            And(x, y) | Or(x, y) => {
                let mut x = x.ident();
                x.append(&mut y.ident());
                x
            }
            _ => vec![],
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Assumption {
    True,
    False,
    Unknowable,
}

impl Not for Assumption {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Self::True => Self::False,
            Self::False => Self::True,
            Self::Unknowable => Self::Unknowable,
        }
    }
}

impl BitAnd for Assumption {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        match self {
            Self::True => match rhs {
                Self::True => Self::True,
                _ => Self::Unknowable,
            },
            Self::False => match rhs {
                Self::False => Self::False,
                _ => Self::Unknowable,
            },
            _ => Self::Unknowable,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssumptionSet(HashMap<String, Assumption>);

impl AssumptionSet {
    fn from<S: Into<String>>(s: S) -> Self {
        let mut m = HashMap::new();
        m.insert(s.into(), Assumption::True);
        Self(m)
    }

    fn reduce(&self, expr: &Expr) -> Expr {
        match expr {
            Expr::Value(v) => match self.0.get(v) {
                Some(ass) => match ass {
                    Assumption::True => Expr::True,
                    Assumption::False => Expr::False,
                    Assumption::Unknowable => Expr::Value(v.clone()),
                },
                None => Expr::Value(v.clone()),
            },
            Expr::Not(b) => match b.as_ref() {
                Expr::Not(b) => self.reduce(b.as_ref()),
                Expr::True => Expr::False,
                Expr::False => Expr::True,
                b => Expr::Not(Box::new(self.reduce(&b))),
            },
            Expr::And(a, b) => {
                let a = self.reduce(a);
                let b = self.reduce(b);
                // (false & x) = false
                if a == Expr::False {
                    return Expr::False;
                }
                // (x & false) = false
                if b == Expr::False {
                    return Expr::False;
                }
                // (true & x) = x
                if a == Expr::True {
                    return b;
                }
                // (x & true) = x
                if b == Expr::True {
                    return a;
                }
                // (!x & x) = false
                if (!a.clone()).reduce() == b {
                    return Expr::False;
                }
                // if b implies a, (a & b) = b
                if Expr::True == b.assumptions().reduce(&a) {
                    return b;
                }
                // if a implies b, (a & b) = a
                if Expr::True == a.assumptions().reduce(&b) {
                    return a;
                }
                // both terms are needed
                a & b
            }
            Expr::Or(a, b) => {
                let a = self.reduce(a);
                let b = self.reduce(b);

                // (x | false) = x
                if a == Expr::False {
                    return b;
                }
                // (false | x) = x
                if b == Expr::False {
                    return a;
                }
                // (!x | x) = true
                if (!a.clone()).reduce() == b {
                    return Expr::True;
                }
                // if b implies a, (a | b) = b
                if Expr::True == b.assumptions().reduce(&a) {
                    return b;
                }
                // if a implies b, (a | b) = a
                if Expr::True == a.assumptions().reduce(&b) {
                    return a;
                }
                // both terms are needed
                a | b
            }
            x => x.clone(),
        }
        .sort()
    }
}

impl Default for AssumptionSet {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl BitAnd for AssumptionSet {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        let mut m = HashMap::new();
        for (k, v) in self.0.into_iter().chain(rhs.0.into_iter()) {
            let v = match m.remove(&k) {
                Some(pv) => pv & v,
                None => v,
            };
            m.insert(k, v);
        }
        Self(m)
    }
}

impl Not for AssumptionSet {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self(self.0.into_iter().map(|(k, v)| (k, !v)).collect())
    }
}

impl Ord for Expr {
    fn cmp(&self, other: &Self) -> Ordering {
        match self {
            Expr::True => match other {
                Expr::True => Ordering::Equal,
                _ => Ordering::Less,
            },
            Expr::False => match other {
                Expr::True => Ordering::Greater,
                Expr::False => Ordering::Equal,
                _ => Ordering::Less,
            },
            Expr::Value(v1) => match other {
                Expr::True | Expr::False => Ordering::Greater,
                Expr::Value(v2) => v1.cmp(v2),
                _ => Ordering::Less,
            },
            Expr::Not(v1) => match other {
                Expr::True | Expr::False | Expr::Value(_) => Ordering::Greater,
                Expr::Not(v2) => v1.cmp(v2),
                _ => Ordering::Less,
            },
            Expr::And(l, r) => match other {
                Expr::True | Expr::False | Expr::Value(_) | Expr::Not(_) => Ordering::Greater,
                Expr::And(l2, r2) => match l.cmp(l2) {
                    Ordering::Equal => r.cmp(r2),
                    o => o,
                },
                _ => Ordering::Less,
            },
            Expr::Or(l, r) => match other {
                Expr::True | Expr::False | Expr::Value(_) | Expr::Not(_) | Expr::And(_, _) => {
                    Ordering::Greater
                }
                Expr::Or(l2, r2) => match l.cmp(l2) {
                    Ordering::Equal => r.cmp(r2),
                    o => o,
                },
                _ => Ordering::Less,
            },
            Expr::CExpr(_e) => unimplemented!(),
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
        vec.iter().fold(Expr::True, |acc, cur| acc & cur.clone())
    }

    fn satisfies(&self, defines: &[Define]) -> bool {
        match self {
            Expr::True => true,
            Expr::False => false,
            Expr::Value(v) => defines.iter().map(|x| x.name()).any(|n| n == v),
            Expr::And(l, r) => l.satisfies(defines) && r.satisfies(defines),
            Expr::Or(l, r) => l.satisfies(defines) || r.satisfies(defines),
            Expr::Not(v) => !v.satisfies(defines),
            Expr::CExpr(_e) => unimplemented!(),
        }
    }

    fn reduce(&self) -> Expr {
        AssumptionSet::default().reduce(self)
    }

    fn sort(&self) -> Expr {
        match self {
            Expr::And(l, r) => {
                let l = l.sort();
                let r = r.sort();

                match &l {
                    Expr::And(l2, r2) => {
                        // (l2 & r2) & r
                        let mut elms = [*l2.clone(), *r2.clone(), r];
                        elms.sort_unstable();
                        let [a, b, c] = elms;
                        return a & (b & c).sort();
                    }
                    _ => {}
                }
                match &r {
                    Expr::And(l2, r2) => {
                        // l & (l2 & r2)
                        let mut elms = [l, *l2.clone(), *r2.clone()];
                        elms.sort_unstable();
                        let [a, b, c] = elms;
                        return a & (b & c).sort();
                    }
                    _ => {}
                }

                if l > r {
                    Expr::And(Box::new(r), Box::new(l))
                } else {
                    Expr::And(Box::new(l), Box::new(r))
                }
            }
            Expr::Or(l, r) => {
                let l = l.sort();
                let r = r.sort();

                match &l {
                    Expr::Or(l2, r2) => {
                        // (l2 & r2) & r
                        let mut elms = [*l2.clone(), *r2.clone(), r];
                        elms.sort_unstable();
                        let [a, b, c] = elms;
                        return a | (b | c).sort();
                    }
                    _ => {}
                }
                match &r {
                    Expr::Or(l2, r2) => {
                        // l & (l2 & r2)
                        let mut elms = [l, *l2.clone(), *r2.clone()];
                        elms.sort_unstable();
                        let [a, b, c] = elms;
                        return a | (b | c).sort();
                    }
                    _ => {}
                }

                if l > r {
                    Expr::Or(Box::new(r), Box::new(l))
                } else {
                    Expr::Or(Box::new(l), Box::new(r))
                }
            }
            v => v.clone(),
        }
    }

    fn permute(&self, f: &mut dyn FnMut(Expr)) {
        match self {
            Expr::And(l, r) => {
                l.permute(&mut |l| {
                    r.permute(&mut |r| {
                        f(l.clone() & r.clone());
                        f(r.clone() & l.clone());
                    });
                });
                match l.as_ref() {
                    Expr::And(l2, r2) => {
                        l2.permute(&mut |l2| {
                            let other = *r2.clone() & *r.clone();
                            other.permute(&mut |other| {
                                f(l2.clone() & other.clone());
                                f(other.clone() & l2.clone());
                            })
                        });
                    }
                    _ => {}
                }
                match r.as_ref() {
                    Expr::And(l2, r2) => {
                        r2.permute(&mut |r2| {
                            let other = *l.clone() & *l2.clone();
                            other.permute(&mut |other| {
                                f(r2.clone() & other.clone());
                                f(other.clone() & r2.clone());
                            })
                        });
                    }
                    _ => {}
                }
            }
            Expr::Or(l, r) => {
                l.permute(&mut |l| {
                    r.permute(&mut |r| {
                        f(Expr::Or(Box::new(l.clone()), Box::new(r.clone())));
                        f(Expr::Or(Box::new(r), Box::new(l.clone())));
                    })
                });
                match l.as_ref() {
                    Expr::Or(l2, r2) => {
                        l2.permute(&mut |l2| {
                            let other = *r2.clone() | *r.clone();
                            other.permute(&mut |other| {
                                f(l2.clone() | other.clone());
                                f(other.clone() | l2.clone());
                            })
                        });
                    }
                    _ => {}
                }
                match r.as_ref() {
                    Expr::Or(l2, r2) => {
                        r2.permute(&mut |r2| {
                            let other = *l.clone() & *l2.clone();
                            other.permute(&mut |other| {
                                f(r2.clone() | other.clone());
                                f(other.clone() | r2.clone());
                            })
                        });
                    }
                    _ => {}
                }
            }
            Expr::Not(x) => x.permute(&mut |x| f(Expr::Not(Box::new(x)))),
            x => f(x.clone()),
        }
    }

    fn assumptions(&self) -> AssumptionSet {
        match self {
            Expr::Value(s) => AssumptionSet::from(s),
            Expr::Not(b) => !b.assumptions(),
            Expr::And(a, b) => a.assumptions() & b.assumptions(),
            _ => Default::default(),
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
        match (self, rhs) {
            (lhs, Expr::True) => lhs,
            (Expr::True, rhs) => rhs,
            (lhs, rhs) => Expr::And(Box::new(lhs), Box::new(rhs)),
        }
    }
}

impl BitOr for Expr {
    type Output = Expr;

    fn bitor(self, rhs: Expr) -> Self::Output {
        match (self, rhs) {
            (lhs, Expr::True) => lhs,
            (Expr::True, rhs) => rhs,
            (lhs, rhs) => Expr::Or(Box::new(lhs), Box::new(rhs)),
        }
    }
}

impl Not for Expr {
    type Output = Expr;

    fn not(self) -> Self::Output {
        match self {
            Expr::Not(v) => *v,
            v => Expr::Not(Box::new(v)),
        }
    }
}

impl Not for Box<Expr> {
    type Output = Box<Expr>;

    fn not(self) -> Self::Output {
        Box::new(!*self)
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
                        let pred = Expr::CExpr(expr);
                        last_if = Some(pred.clone());
                        def_ranges.push((n, pred));
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
                        def_ranges.pop(n);
                        let pred = Expr::CExpr(value);
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
