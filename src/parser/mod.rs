mod directive;
mod emit;

use directive::PreprocessorIdent;
use lang_c::driver;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::ops::{BitAnd, BitOr, Not, Range};
use std::{
    fmt,
    path::{Path, PathBuf},
};

use hashbrown::{HashMap, HashSet};
use lang_c::ast::Expression;

use directive::Directive;
use regex::Regex;

fn strip_all_escaped_newlines<R: BufRead>(reader: R) -> String {
    reader
        .lines()
        .filter_map(Result::ok)
        .map(|line| {
            if line.ends_with(r"\") {
                format!(" {}", line.trim_matches('\\').trim())
            } else {
                format!("{}\n", line.trim())
            }
        })
        .collect::<Vec<_>>()
        .join("")
}

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
enum Defined {
    True,
    Value(String),
    Expr(Expression),
    And(Box<Defined>, Box<Defined>),
    Or(Box<Defined>, Box<Defined>),
    Not(Box<Defined>),
}

impl fmt::Debug for Defined {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Defined::*;

        match self {
            True => write!(f, "true"),
            Value(s) => write!(f, "defined({})", s),
            Expr(e) => write!(f, "(expr({:?}))", e),
            And(l, r) => write!(f, "({:?} && {:?})", l, r),
            Or(l, r) => write!(f, "({:?} || {:?})", l, r),
            Not(v) => write!(f, "(!{:?})", v),
        }
    }
}

impl PreprocessorIdent for Defined {
    fn ident(&self) -> Vec<String> {
        use Defined::*;

        match self {
            Value(x) => vec![x.clone()],
            Expr(x) => x.ident(),
            And(x, y) | Or(x, y) => {
                let mut x = x.ident();
                x.append(&mut y.ident());
                x
            }
            _ => vec![],
        }
    }
}

impl Defined {
    fn combine(vec: &[Defined]) -> Defined {
        vec.iter().fold(Defined::True, |acc, cur| acc & cur.clone())
    }

    fn satisfies(&self, defines: &[Define]) -> bool {
        match self {
            Defined::True => true,
            Defined::Value(v) => defines.iter().map(|x| x.name()).any(|n| n == v),
            Defined::And(a, b) => a.satisfies(defines) && b.satisfies(defines),
            Defined::Or(a, b) => a.satisfies(defines) || b.satisfies(defines),
            Defined::Not(a) => !a.satisfies(defines),
            Defined::Expr(_e) => unimplemented!(),
        }
    }
}

impl<T> From<T> for Defined
where
    T: AsRef<str>,
{
    fn from(v: T) -> Self {
        Defined::Value(v.as_ref().into())
    }
}

impl Default for Defined {
    fn default() -> Defined {
        Defined::True
    }
}

impl BitAnd for Defined {
    type Output = Defined;

    fn bitand(self, rhs: Defined) -> Self::Output {
        match (self, rhs) {
            (lhs, Defined::True) => lhs,
            (Defined::True, rhs) => rhs,
            (lhs, rhs) => Defined::And(Box::new(lhs), Box::new(rhs)),
        }
    }
}

impl BitOr for Defined {
    type Output = Defined;

    fn bitor(self, rhs: Defined) -> Self::Output {
        match (self, rhs) {
            (lhs, Defined::True) => lhs,
            (Defined::True, rhs) => rhs,
            (lhs, rhs) => Defined::Or(Box::new(lhs), Box::new(rhs)),
        }
    }
}

impl Not for Defined {
    type Output = Defined;

    fn not(self) -> Self::Output {
        match self {
            Defined::Not(v) => *v,
            v => Defined::Not(Box::new(v)),
        }
    }
}

impl Not for Box<Defined> {
    type Output = Box<Defined>;

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

#[derive(Debug)]
pub struct ParsedUnit {
    source: String,
    def_ranges: RangeSet<Defined>,
    dependencies: HashMap<Include, Defined>,
}

#[derive(Debug)]
pub struct RangeSet<K: Default> {
    stack: Vec<K>,
    vec: BTreeMap<usize, K>,
}

impl<K> RangeSet<K>
where
    K: Default + Clone + PartialEq + Eq + std::hash::Hash + BitAnd<Output = K> + BitOr<Output = K>,
{
    fn new() -> RangeSet<K> {
        let mut set = RangeSet {
            stack: vec![],
            vec: BTreeMap::new(),
        };
        set.vec.insert(0, K::default());
        set
    }

    fn keys(&self) -> HashSet<&K> {
        let mut set = HashSet::new();
        for v in self.vec.values() {
            set.insert(v);
        }
        set
    }

    fn push(&mut self, index: usize, key: K) {
        assert!(index == 0 || *self.vec.keys().last().unwrap() <= index);

        let value = self.vec.values().last().cloned().unwrap();
        let new_value = key & value;

        self.vec.insert(index, new_value.clone());
        self.stack.push(new_value);
    }

    fn pop(&mut self, index: usize) {
        assert!(!self.vec.keys().last().is_none() && *self.vec.keys().last().unwrap() <= index);

        self.stack.pop();
        let prev_key = self.stack.last().cloned().unwrap_or_else(|| K::default());
        self.vec.insert(index, prev_key);
    }

    fn last(&self) -> (usize, &K) {
        self.vec.iter().last().map(|(a, b)| (*a, b)).unwrap()
    }

    fn iter<'a>(&'a self) -> Iter<'a, K> {
        Iter {
            range_set: self,
            keys: self.vec.keys().copied().collect::<VecDeque<_>>(),
        }
    }
}

struct Iter<'a, K: Default> {
    range_set: &'a RangeSet<K>,
    keys: VecDeque<usize>,
}

impl<'a, K: Default> Iterator for Iter<'a, K> {
    type Item = (Range<usize>, &'a K);

    fn next(&mut self) -> Option<Self::Item> {
        // log::trace!("KEYS: {:?}", &self.keys);
        if self.keys.len() <= 1 {
            return None;
        }

        let first = self.keys.pop_front().unwrap();
        let second = self.keys[0];
        Some((first..second, self.range_set.vec.get(&first).unwrap()))
    }
}

impl ParsedUnit {
    /// Go through each line of a source file, handling preprocessor directives
    /// like #if, #ifdef, #include, etc.
    fn parse(source: String) -> Result<ParsedUnit, Error> {
        let mut dependencies = HashMap::new();
        let mut def_ranges = RangeSet::<Defined>::new();
        let mut n = 0usize;
        let mut last_if: Option<Defined> = None;

        for line in source.lines() {
            if let Some(directive) = directive::parse_directive(line) {
                log::debug!("{:?}", &directive);

                match directive {
                    Directive::Include(include) => {
                        dependencies.insert(include, def_ranges.last().1.clone());
                    }
                    Directive::If(expr) => {
                        let pred = Defined::Expr(expr);
                        def_ranges.push(n, pred);
                    }
                    Directive::IfDefined(name) => {
                        let pred = Defined::Value(name);
                        last_if = Some(pred.clone());
                        def_ranges.push(n, pred);
                    }
                    Directive::IfNotDefined(name) => {
                        let pred = !Defined::Value(name);
                        last_if = Some(pred.clone());
                        def_ranges.push(n, pred);
                    }
                    Directive::ElseIf(value) => {
                        def_ranges.pop(n);
                        let pred = Defined::Expr(value);
                        last_if = Some(pred.clone());
                        def_ranges.push(n, pred);
                    }
                    Directive::Else => {
                        def_ranges.pop(n);
                        def_ranges.push(n, !last_if.clone().expect("else without last_if"));
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

    fn source(&self, defines: &[Define]) -> String {
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
        let file = strip_all_escaped_newlines(file);
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

    /// Emit a single-file header, starting from the root, depth-first,
    /// emitting the leaf-most units first.
    pub fn emit_header(&self, defines: &[Define]) {
        let mut set = Vec::new();

        fn visit(
            sources: &HashMap<Include, ParsedUnit>,
            set: &mut Vec<Include>,
            defines: &[Define],
            k: &Include,
        ) {
            let mm = sources.get(k).unwrap();
            log::trace!("[>] Visiting {:?}", k);
            for (kk, cons) in &mm.dependencies {
                if cons.satisfies(defines) {
                    log::trace!("[v] Descending into {:?} ({:?})", kk, cons);
                    visit(sources, set, defines, kk)
                } else {
                    log::trace!("[x] Skipping {:?} (cons {:?})", kk, cons);
                }
            }

            // need an OrderedSet here really
            if !set.iter().any(|x| x == k) {
                set.push(k.clone());
            }
        }
        log::trace!("=========== emit graph traversal start =============");
        visit(&self.sources, &mut set, defines, &self.root);
        log::trace!("=========== emit graph traversal end ===============");

        for k in &set {
            log::debug!("Processing {:?}", k);

            let mm = self.sources.get(k).unwrap();

            let source = mm.source(defines);
            println!("{}", source);

            match driver::parse_preprocessed(
                &driver::Config {
                    cpp_command: "<none>".into(),
                    cpp_options: Default::default(),
                    flavor: driver::Flavor::MsvcC11,
                },
                source.clone(),
            ) {
                Ok(res) => {
                    let unit = res.unit;
                    println!("\n====== traversing AST");

                    let unit_out = emit::translate_unit(&unit);

                    let code = format!("{}\n{}", emit::prelude(), unit_out);

                    println!("========= Rust code ===========");
                    println!("{}", code);

                    std::fs::create_dir_all("out").unwrap();
                    let path = "out/out.rs";
                    std::fs::write(path, code).unwrap();
                    println!("(Also written to {:?})", path);
                }
                Err(e) => println!("Failed: {:#?}", e),
            }
        }
    }
}

#[cfg(test)]
mod tests;
