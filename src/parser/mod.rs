mod directive;
mod rangeset;
mod utils;

use directive::{Directive, PreprocessorIdent};
use rangeset::RangeSet;

use hashbrown::HashMap;
use lang_c::ast::Expression;
use regex::Regex;
use std::{
    collections::VecDeque,
    fmt,
    fs::File,
    io::{self, BufReader},
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
            Value(s) => write!(f, "defined({})", s),
            CExpr(e) => write!(f, "(expr({:?}))", e),
            And(l, r) => write!(f, "({:?} && {:?})", l, r),
            Or(l, r) => write!(f, "({:?} || {:?})", l, r),
            Not(v) => write!(f, "(!{:?})", v),
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

impl Expr {
    fn combine(vec: &[Expr]) -> Expr {
        vec.iter().fold(Expr::True, |acc, cur| acc & cur.clone())
    }

    fn satisfies(&self, defines: &[Define]) -> bool {
        match self {
            Expr::True => true,
            Expr::Value(v) => defines.iter().map(|x| x.name()).any(|n| n == v),
            Expr::And(a, b) => a.satisfies(defines) && b.satisfies(defines),
            Expr::Or(a, b) => a.satisfies(defines) || b.satisfies(defines),
            Expr::Not(a) => !a.satisfies(defines),
            Expr::CExpr(_e) => unimplemented!(),
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

#[derive(Debug)]
pub enum Error {
    InvalidFile,
    Io(io::Error),
    NotFound(Include),
}

/// One (1) C header, split into define-dependent ranges.
#[derive(Debug)]
pub struct ParsedUnit {
    source: String,
    def_ranges: RangeSet<Expr>,
    dependencies: HashMap<Include, Expr>,
}

#[derive(Debug)]
pub struct Chunk {
    pub expr: Expr,
}

impl ParsedUnit {
    /// Go through each line of a source file, handling preprocessor directives
    /// like #if, #ifdef, #include, etc.
    fn parse(source: String) -> Result<ParsedUnit, Error> {
        let mut dependencies = HashMap::new();
        let mut def_ranges = RangeSet::<Expr>::new();
        let mut n = 0usize;
        let mut last_if: Option<Expr> = None;

        for line in source.lines() {
            if let Some(directive) = directive::parse_directive(line) {
                log::debug!("{:?}", &directive);

                match directive {
                    Directive::Include(include) => {
                        dependencies.insert(include, def_ranges.last().1.clone());
                    }
                    Directive::If(expr) => {
                        let pred = Expr::CExpr(expr);
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
                    _ => unimplemented!(),
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

    // Iterate over valid chunks of C code
    pub fn chunks<F, T, E>(_f: F)
    where
        F: FnMut(Chunk) -> Result<T, E>,
    {
        unimplemented!()
    }

    pub fn source(&self, defines: &[Define]) -> String {
        // log::trace!("SOURCE: {:?}", rules);

        let lines = self.source.lines().collect::<Vec<&str>>();
        let directive_pattern = Regex::new(r"^\s*#").unwrap();
        let comment_pattern = Regex::new(r"^\s*//").unwrap();

        let mut out = String::new();
        let mut last_was_whitespace = false;
        let mut write = |s: &str| {
            let is_whitespace = s.chars().all(char::is_whitespace);
            if is_whitespace && last_was_whitespace {
                // don't write anything
            } else {
                out.push_str(s);
                out.push('\n');
            }
            last_was_whitespace = is_whitespace;
        };

        for (range, key) in self.def_ranges.iter() {
            if !key.satisfies(defines) {
                log::debug!("Skipping key {:?}", key);
                continue;
            }

            for line in &lines[range] {
                if directive_pattern.is_match(line) {
                    continue;
                }
                if comment_pattern.is_match(line) {
                    continue;
                }
                write(line);
            }
        }
        out
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
    /// to String, strip escaped newlines.
    fn read_include(&self, include: &Include) -> Result<String, Error> {
        log::debug!("=== {:?} ===", include);

        let path =
            match include.resolve(&*self.system_paths, &self.quoted_paths, &self.working_path) {
                Some(v) => v,
                None => return Err(Error::NotFound(include.clone())),
            };

        log::debug!("=== {:?} ===", &path);

        let file = File::open(path).map_err(Error::Io)?;
        let file = BufReader::new(file);
        let file = utils::strip_all_escaped_newlines(file);
        Ok(file)
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
            let parsed_unit = ParsedUnit::parse(source)?;

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
mod tests;
