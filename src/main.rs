mod directive;

use crate::directive::PreprocessorIdent;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::ops::{BitAnd, BitOr, Deref, Not, Range};
use std::path::{Path, PathBuf};

use hashbrown::{HashSet};
use lang_c::ast::Expression;
use regex::Regex;

use directive::Directive;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Defined {
    None,
    Value(String),
    NotValue(String),
    Expr(Expression),
    NotExpr(Expression),
    Any(Vec<Defined>),
    NotAny(Vec<Defined>),
    And(Box<Defined>, Box<Defined>),
    Or(Box<Defined>, Box<Defined>),
}

impl PreprocessorIdent for Defined {
    fn ident(&self) -> Vec<String> {
        use Defined::*;

        match self {
            Value(x) | NotValue(x) => vec![x.clone()],
            Expr(x) | NotExpr(x) => x.ident(),
            Any(x) | NotAny(x) => x.iter().map(|x| x.ident()).flatten().collect(),
            And(x, y) | Or(x, y) => {
                let mut x = x.ident();
                x.append(&mut y.ident());
                x
            }
            _ => vec![]
        }
    }
}

impl Defined {
    fn combine(vec: &[Defined]) -> Defined {
        vec.iter().fold(Defined::None, |acc, cur| acc & cur.clone())
    }

    fn satisfies(&self, defines: &[Define]) -> bool {
        match self {
            Defined::None => true,
            Defined::Any(v) => v.iter().any(|d| d.satisfies(defines)),
            Defined::NotAny(v) => v.iter().all(|d| !d.satisfies(defines)),
            Defined::Value(v) => defines.iter().map(|x| x.name()).any(|n| n == v),
            Defined::NotValue(v) => !defines.iter().map(|x| x.name()).any(|n| n == v),
            Defined::Expr(e) => {
                false // TODO!
            }
            Defined::NotExpr(e) => false,
            Defined::And(a, b) => a.satisfies(defines) && b.satisfies(defines),
            Defined::Or(a, b) => a.satisfies(defines) || b.satisfies(defines),
        }
    }
}

impl Default for Defined {
    fn default() -> Defined {
        Defined::None
    }
}

impl BitAnd for Defined {
    type Output = Defined;

    fn bitand(self, rhs: Defined) -> Self::Output {
        match (self, rhs) {
            (lhs, Defined::None) => lhs,
            (Defined::None, rhs) => rhs,
            (lhs, rhs) => Defined::And(Box::new(lhs), Box::new(rhs)),
        }
    }
}

impl BitOr for Defined {
    type Output = Defined;

    fn bitor(self, rhs: Defined) -> Self::Output {
        match (self, rhs) {
            (lhs, Defined::None) => lhs,
            (Defined::None, rhs) => rhs,
            (lhs, rhs) => Defined::Or(Box::new(lhs), Box::new(rhs)),
        }
    }
}

impl Not for Defined {
    type Output = Defined;

    fn not(self) -> Self::Output {
        match self {
            Defined::None => Defined::None,
            Defined::Any(v) => Defined::NotAny(v),
            Defined::NotAny(v) => Defined::Any(v),
            Defined::Value(v) => Defined::NotValue(v),
            Defined::NotValue(v) => Defined::Value(v),
            Defined::Expr(v) => Defined::NotExpr(v),
            Defined::NotExpr(v) => Defined::Expr(v),
            Defined::And(a, b) => Defined::Or(!a, !b),
            Defined::Or(a, b) => Defined::And(!a, !b),
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
        Some(self.cmp(other))
    }
}

impl Ord for Include {
    fn cmp(&self, other: &Include) -> std::cmp::Ordering {
        match (self, other) {
            (Include::System(ref x), Include::System(ref y)) => x.cmp(y),
            (Include::Quoted(ref x), Include::Quoted(ref y)) => x.cmp(y),
            (Include::System(_), Include::Quoted(_)) => std::cmp::Ordering::Greater,
            (Include::Quoted(_), Include::System(_)) => std::cmp::Ordering::Less,
            (Include::Expression(_), Include::Expression(_)) => std::cmp::Ordering::Equal,
            (_, Include::Expression(_)) => std::cmp::Ordering::Greater,
            (Include::Expression(_), _) => std::cmp::Ordering::Less,
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

    pub fn resolve(
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

    pub fn path(&self) -> Option<&Path> {
        match self {
            Include::System(p) | Include::Quoted(p) => Some(&p),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    NotFound(Include),
}

#[derive(Debug)]
pub struct ParsedUnit {
    source: String,
    flattened_source: String,
    def_ranges: RangeSet<Defined>,
    dependencies: BTreeMap<Include, Vec<Defined>>,
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
    fn source(&self, defines: &[Define]) -> String {
        // log::trace!("SOURCE: {:?}", rules);

        let mut out: Vec<String> = vec![];
        for (range, key) in self.def_ranges.iter() {
            if !key.satisfies(defines) {
                continue;
            }

            out.push(
                self.source
                    .lines()
                    .skip(range.start + 1)
                    .take(range.end - range.start - 1)
                    .filter(|x| !x.trim().starts_with("#"))
                    .collect::<Vec<_>>()
                    .join("\n"),
            );
        }

        return out.join("\n");
    }
}

#[derive(Debug)]
pub struct Parser {
    system_paths: Vec<PathBuf>,
    quoted_paths: Vec<PathBuf>,
    working_path: PathBuf,
    sources: BTreeMap<Include, ParsedUnit>,
    unit_queue: VecDeque<Include>,
}

#[derive(Debug)]
pub enum ParserError {
    InvalidFile,
}

impl Parser {
    pub fn new(
        initial_file: PathBuf,
        system_paths: Vec<PathBuf>,
        quoted_paths: Vec<PathBuf>,
    ) -> Result<Parser, ParserError> {
        let file_name = initial_file.file_name().ok_or(ParserError::InvalidFile)?;
        let working_path = initial_file
            .parent()
            .ok_or(ParserError::InvalidFile)?
            .to_path_buf();

        let mut parser = Parser {
            system_paths,
            quoted_paths,
            working_path,
            sources: BTreeMap::new(),
            unit_queue: VecDeque::new(),
        };

        parser
            .unit_queue
            .push_back(Include::Quoted(file_name.into()));

        Ok(parser)
    }

    fn parse_work_unit(&mut self, include: &Include, source: String) -> Result<ParsedUnit, Error> {
        // let mut defines = HashSet::new();
        let mut dependencies = BTreeMap::new();
        let mut def_ranges = RangeSet::<Defined>::new();
        let mut n = 0usize;
        let mut last_ifs = vec![];

        // First pass: collect all the context
        for line in source.lines() {
            if let Some(directive) = directive::parse_directive(line) {
                // self.handle_directive(directive)
                log::debug!("{:?}", &directive);

                match directive {
                    Directive::Include(include) => {
                        dependencies.insert(
                            include,
                            def_ranges.iter().map(|x| x.1.clone()).collect::<Vec<_>>(),
                        );
                    }
                    Directive::If(expr) => {
                        def_ranges.push(n, Defined::Expr(expr.clone()));
                    }
                    Directive::IfDefined(name) => {
                        def_ranges.push(n, Defined::Value(name.clone()));
                        last_ifs = vec![Defined::Value(name)];
                    }
                    Directive::IfNotDefined(name) => {
                        def_ranges.push(n, Defined::NotValue(name.clone()));
                        last_ifs = vec![Defined::NotValue(name)];
                    }
                    Directive::ElseIf(value) => {
                        def_ranges.pop(n);
                        def_ranges.push(n, Defined::Expr(value.clone()));
                        last_ifs.push(Defined::Expr(value));
                    }
                    Directive::Else => {
                        def_ranges.pop(n);
                        def_ranges.push(n, Defined::NotAny(last_ifs.clone()));
                    }
                    Directive::EndIf => {
                        def_ranges.pop(n);
                        last_ifs = vec![];
                    }
                    _ => {}
                }
                log::trace!("STACK: {:?}", def_ranges.last());
            }
            n += 1;
        }

        def_ranges.pop(n);

        // Second pass: generate a not-quite-minimum set of rules for handling the entire file

        // - Catch <, >, =>, <=, and other operator conditionals
        // - Catch dumb checks for existence

        let keys = def_ranges.keys();

        use std::fmt::Write;

        // Final pass: flatten the document
        let mut f = String::with_capacity(source.len());
        writeln!(&mut f, "# rust mod {:?}", self.resolve_include(include));

        for expr in keys {
            writeln!(&mut f, "# rust features {:?}", expr).unwrap();
            writeln!(&mut f, )
        }

        Ok(ParsedUnit {
            source,
            flattened_source: f,
            def_ranges,
            dependencies,
        })
    }

    fn parse_include(&self, include: &Include) -> Result<String, Error> {
        log::trace!("=== {:?} ===", include);

        let path =
            match include.resolve(&*self.system_paths, &self.quoted_paths, &self.working_path) {
                Some(v) => v,
                None => return Err(Error::NotFound(include.clone())),
            };

        log::trace!("=== {:?} ===", &path);

        let file = File::open(path).map_err(Error::Io)?;
        let file = BufReader::new(file);
        let file = strip_all_escaped_newlines(file);
        
        Ok(file)
    }

    fn recurse_includes(&mut self) -> Result<(), Error> {
        while let Some(work_unit) = self.unit_queue.pop_front() {
            log::debug!("## WORK UNIT: {:?}", &work_unit);

            if self.sources.contains_key(&work_unit) {
                continue;
            }

            let source = self.parse_include(&work_unit)?;
            let parsed_unit = self.parse_work_unit(&work_unit, source)?;

            // log::trace!("==============");
            // log::trace!("RANGES:");
            // for key in parsed_unit.def_ranges.keys() {
            //     log::trace!("{:?}: {:?}", key, parsed_unit.def_ranges.ranges(key).collect::<Vec<_>>());
            // }
            // log::trace!("==============");

            // for key in parsed_unit.def_ranges.keys() {
            // log::trace!("Nothing => ```{}```", parsed_unit.source(&[]));
            // log::trace!("With FOO => ```{}```", parsed_unit.source(&[Define::Value { name: "FOO".into(), value: None }]));
            // log::trace!("With BAR => ```{}```", parsed_unit.source(&[Define::Value { name: "BAR".into(), value: None }]));
            // }

            log::trace!("{:?}", &parsed_unit);

            for include in parsed_unit.dependencies.keys() {
                self.unit_queue.push_back(include.clone());
            }

            self.sources.insert(work_unit, parsed_unit);
        }

        Ok(())
    }

    fn ohno(&self) -> HashSet<String> {
        let mut set = HashSet::new();

        for (k, mm) in self.sources.iter() {
            println!("{:?}: {:?}", k, mm.def_ranges.iter()
                .map(|x| x.1.ident())
                .flatten()
                .collect::<HashSet<_>>());
        }

        set
    }

    fn print_graphviz(&self, font_name: &str) {
        println!(r#"digraph {{
            node [shape=box, fontname="{}"]
            graph [pad="0.5", nodesep="0.5", ranksep="0.5"];
        "#, font_name);

        const COLORS: [&'static str; 5] = ["red", "blue", "darkgreen", "purple", "orange"];
        let mut i = 5usize;

        for (include, unit) in self.sources.iter() {
            if let Some(path) = include.path() {
                i = (i + 1) % 5;
                println!("edge [color={}]", COLORS[i]);
                println!("  \"{}\" -> {{", path.display());

                for x in unit.dependencies.keys() {
                    if let Some(path) = x.path() {
                        println!("    \"{}\"", path.display())
                    }
                }

                println!("  }};");
            }

        }

        println!("}}");
    }

    #[inline]
    fn resolve_include(&self, include: &Include) -> Option<PathBuf> {
        include.resolve(&*self.system_paths, &self.quoted_paths, &self.working_path)
    }

    fn generate_output(&self) {
        for (include, unit) in self.sources.iter() {
            if let Some(path) = self.resolve_include(include) {
                // TODO: includes should include a "resolve mod" for Rust
                println!("# rust mod {}", path.display());

                println!("  \"{}\" -> {{", path.display());

                for x in unit.dependencies.keys() {
                    if let Some(path) = x.path() {
                        println!("    \"{}\"", path.display())
                    }
                }

                println!("  }};");
            }

        }
    }

    pub fn parse(mut self) -> Result<String, Error> {
        self.recurse_includes()?;

        // self.ohno();

        // println!("{:#?}", &self.sources.keys());
        // self.print_graphviz("JetBrains Mono");

        Ok("TODO".into())
    }
}

fn main() {
    env_logger::init();
    let kits = PathBuf::from("./winsdk"); //PathBuf::from(r"C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0");

    Parser::new(PathBuf::from("./test.h"), vec![
        kits.join("ucrt"),
        kits.join("shared"),
        kits.join("um"),
        // kits.join("km"),
        // PathBuf::from(r"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.24.28314\include"),
        PathBuf::from("./msvc"),
        PathBuf::from("./stubs")
    ], vec![])
        .unwrap()
        .parse()
        .unwrap();
}
