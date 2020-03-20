mod directive;
mod rangeset;
mod utils;

use directive::{Directive, PreprocessorIdent};
use quine_mc_cluskey as qmc;
use rangeset::RangeSet;
use thiserror::Error;

use custom_debug_derive::CustomDebug;
use hashbrown::HashMap;
use lang_c::{ast::Expression, driver, env::Env};
use regex::Regex;
use std::{
    collections::{HashSet, VecDeque},
    fmt, io,
    ops::{BitAnd, BitOr, Not},
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Define {
    Blacklist {
        name: String,
    },
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

#[derive(Clone)]
pub struct Context {
    defines: HashMap<String, Define>,
}

impl Context {
    pub fn new() -> Self {
        let mut res = Context {
            defines: HashMap::new(),
        };
        res.push(Define::Blacklist {
            name: "__cplusplus".into(),
        });
        res
    }

    pub fn push(&mut self, d: Define) {
        self.defines.insert(d.name().to_string(), d);
    }

    pub fn pop(&mut self, name: &str) {
        self.defines.remove(name);
    }
}

impl Define {
    fn name(&self) -> &str {
        match self {
            Define::Blacklist { name, .. } => name,
            Define::Value { name, .. } => name,
            Define::Replacement { name, .. } => name,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    True,
    False,
    Defined(String),
    Symbol(String),
    Call(Box<Expr>, Vec<Expr>),
    Binary(BinaryOperator, Box<Expr>, Box<Expr>),
    Integer(i64),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equals,
    NotEquals,
    BitwiseOr,
}

impl BinaryOperator {
    fn sign(&self) -> &'static str {
        use BinaryOperator::*;
        match self {
            Less => "<",
            LessOrEqual => "<=",
            Greater => ">",
            GreaterOrEqual => ">=",
            Equals => "==",
            NotEquals => "!=",
            BitwiseOr => "|",
        }
    }
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
        qmc::Bool::Term(match self.map.get(e) {
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
        })
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;

        match self {
            True => write!(f, "true"),
            False => write!(f, "false"),
            Integer(i) => write!(f, "{}", i),
            Binary(op, l, r) => write!(f, "({:?} {} {:?})", l, op.sign(), r),
            Call(callee, args) => {
                write!(f, "({:?}(", callee)?;
                for (i, arg) in args.iter().enumerate() {
                    match i {
                        0 => write!(f, "{:?}", arg),
                        _ => write!(f, ", {:?}", arg),
                    }?;
                }
                write!(f, "))")
            }
            Symbol(s) => write!(f, "{}", s),
            Defined(s) => write!(f, "defined({})", s),
            And(c) => {
                write!(f, "(")?;
                for (i, v) in c.iter().enumerate() {
                    match i {
                        0 => write!(f, "{:?}", v),
                        _ => write!(f, " && {:?}", v),
                    }?;
                }
                write!(f, ")")
            }
            Or(c) => {
                write!(f, "(")?;
                for (i, v) in c.iter().enumerate() {
                    match i {
                        0 => write!(f, "{:?}", v),
                        _ => write!(f, " || {:?}", v),
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
            Defined(x) => vec![x.clone()],
            Symbol(x) => vec![x.clone()],
            Integer(_) => vec![],
            Call(callee, args) => {
                let mut res = callee.ident();
                for v in args {
                    res.append(&mut v.ident());
                }
                res
            }
            Binary(_op, l, r) => {
                let mut res = vec![];
                res.append(&mut l.ident());
                res.append(&mut r.ident());
                res
            }
            And(c) | Or(c) => {
                let mut res = Vec::new();
                for v in c {
                    res.append(&mut v.ident());
                }
                res
            }
            Not(c) => c.ident(),
            True | False => vec![],
        }
    }
}

impl Expr {
    fn from_cexpr(expr: Expression) -> Self {
        use lang_c::ast::{self, Expression as CE};
        use Expr::*;

        match expr {
            CE::Constant(n) => match n.node {
                ast::Constant::Integer(il) => match il.base {
                    ast::IntegerBase::Hexademical /* sic. */ => Integer(i64::from_str_radix(il.number.as_ref(), 16).unwrap()),
                    _ => todo!(),
                },
                _ => unimplemented!(),
            },
            CE::Call(n) => {
                let ast::CallExpression {
                    callee, arguments, ..
                } = n.node;
                Call(
                    Box::new(Self::from_cexpr(callee.node)),
                    arguments
                        .into_iter()
                        .map(|n| Self::from_cexpr(n.node))
                        .collect(),
                )
            }
            CE::Identifier(n) => Symbol(n.node.name),
            CE::BinaryOperator(n) => {
                let ast::BinaryOperatorExpression { operator, lhs, rhs } = n.node;
                use ast::BinaryOperator as CBO;
                use BinaryOperator as BO;

                Binary(
                    match operator.node {
                        CBO::Greater => BO::Greater,
                        CBO::GreaterOrEqual => BO::GreaterOrEqual,
                        CBO::Less => BO::Less,
                        CBO::LessOrEqual => BO::LessOrEqual,
                        CBO::Equals => BO::Equals,
                        CBO::NotEquals => BO::NotEquals,
                        CBO::BitwiseOr => BO::BitwiseOr,
                        _ => {
                            panic!(
                                "unsupported operator in preprocessor expression: {:?}",
                                operator.node
                            );
                        }
                    },
                    Box::new(Self::from_cexpr(lhs.node)),
                    Box::new(Self::from_cexpr(rhs.node)),
                )
            }
            _ => {
                log::debug!("Got CExpr: {:#?}", expr);
                unimplemented!();
            }
        }
    }

    fn combine(vec: &[Expr]) -> Expr {
        use Expr::*;
        vec.iter().fold(True, |acc, cur| acc & cur.clone())
    }

    fn satisfies(&self, defines: &[Define]) -> bool {
        use Expr::*;
        match self {
            True => true,
            False => false,
            Defined(v) => defines.iter().map(|x| x.name()).any(|n| n == v),
            And(c) => c.iter().all(|v| v.satisfies(defines)),
            Or(c) => c.iter().any(|v| v.satisfies(defines)),
            Not(v) => !v.satisfies(defines),
            _ => unimplemented!(),
        }
    }

    fn eval(&self, ctx: &Context) -> Expr {
        use Expr::*;

        match self {
            True | False => self.clone(),
            And(c) => And(c.iter().map(|v| v.eval(ctx)).collect()),
            Or(c) => Or(c.iter().map(|v| v.eval(ctx)).collect()),
            Not(v) => !v.eval(ctx),
            Defined(name) => match ctx.defines.get(name) {
                Some(def) => match def {
                    Define::Blacklist { .. } => {
                        // explicitly blacklisted
                        False
                    }
                    Define::Value { .. } | Define::Replacement { .. } => {
                        // explicitly defined
                        True
                    }
                },
                None => {
                    // not explicitly defined, it's impossible to tell if it's good or not
                    self.clone()
                }
            },
            Symbol(_name) => {
                // TODO: we can do better - by using lang-c to parse `Define::Value`
                // note: we could be doing that earlier
                self.clone()
            }
            Call(callee, args) => {
                // TODO: we can do better - by invoking the macro,
                // which really should be defined at this point.
                Call(
                    Box::new(callee.eval(ctx)),
                    args.iter().map(|arg| arg.eval(ctx)).collect(),
                )
            }
            Binary(op, l, r) => {
                // TODO: again, we can probably do some maths here, if l and r are constants.
                Binary(*op, Box::new(l.eval(ctx)), Box::new(r.eval(ctx)))
            }
            Integer(_) => self.clone(),
        }
    }

    fn simplify(&self) -> Expr {
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
            And(c) => Bool::And(c.into_iter().map(|v| v.as_bool(terms)).collect()),
            Or(c) => Bool::Or(c.into_iter().map(|v| v.as_bool(terms)).collect()),
            Not(v) => Bool::Not(Box::new(v.as_bool(terms))),
            Defined(_) | Symbol(_) | Call(_, _) | Binary(_, _, _) | Integer(_) => terms.add(self),
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

struct Atom<'a> {
    expr: Expr,
    lines: Vec<&'a str>,
}

struct Strand<'a> {
    atoms: Vec<Atom<'a>>,
}

impl<'a> Strand<'a> {
    fn new() -> Self {
        Self {
            atoms: Default::default(),
        }
    }

    /// Returns true if strand has zero atoms
    fn is_empty(&self) -> bool {
        self.atoms.is_empty()
    }

    /// Add a new atom to the list
    fn push(&mut self, atom: Atom<'a>) {
        self.atoms.push(atom)
    }

    /// Return the number of atoms in this strand
    fn len(&self) -> usize {
        self.atoms.len()
    }

    /// Returns true if all atoms put together parse as a series of C external
    /// declarations
    fn has_complete_variants(&self, env: &mut Env, ctx: &Context) -> bool {
        !self.chunks(env, ctx).is_empty()
    }

    /// Returns a single String for the source of all given atoms put together
    fn source(&self, iter: &mut dyn Iterator<Item = &Atom>) -> String {
        iter.map(|r| r.lines.iter().copied())
            .flatten()
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Parses C code
    fn parse(source: String, env: &mut Env) -> Result<driver::Parse, driver::SyntaxError> {
        match lang_c::parser::translation_unit(&source, env) {
            Ok(unit) => Ok(driver::Parse { source, unit }),
            Err(err) => Err(driver::SyntaxError {
                source: source,
                line: err.line,
                column: err.column,
                offset: err.offset,
                expected: err.expected,
            }),
        }
    }

    fn chunks(&self, env: &mut Env, ctx: &Context) -> Vec<Chunk> {
        let mut chunks = Vec::new();
        variations(self.len(), &mut |toggles| {
            let pairs: Vec<_> = self.atoms.iter().zip(toggles).collect();
            if let Some((_, false)) = pairs.get(0) {
                // skip first-false
                return;
            }

            let base_expr = pairs.iter().fold(Expr::True, |acc, (atom, on)| {
                acc & if *on {
                    atom.expr.clone()
                } else {
                    !atom.expr.clone()
                }
            });
            let expr = base_expr.eval(ctx).simplify();
            log::debug!(
                "[{}] {:?} => {:?}",
                pairs
                    .iter()
                    .map(|(_, on)| if *on { "1" } else { "0" })
                    .collect::<Vec<_>>()
                    .join(""),
                base_expr,
                expr
            );
            if matches!(expr, Expr::False) {
                log::debug!("(!) Always-false condition");
                return;
            }

            let atoms: Vec<_> = pairs
                .iter()
                .copied()
                .filter_map(|(atom, on)| if on { Some(atom) } else { None })
                .collect();
            if atoms.is_empty() {
                log::debug!("(!) Empty strand");
                return;
            }

            let source = self.source(&mut atoms.iter().copied());
            log::debug!("Parsing source:\n{:?}", SourceString(source.clone()));
            match Self::parse(source, env) {
                Ok(driver::Parse { source, unit }) => {
                    log::debug!("(✔) Valid chunk");
                    chunks.push(Chunk {
                        source: SourceString(source),
                        unit,
                        expr,
                    })
                }
                Err(e) => {
                    log::debug!("(!) Incomplete strand: {:?}", e);
                }
            }
        });
        if !chunks.is_empty() {
            log::debug!("Found {} complete chunks", chunks.len());
        }
        chunks
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
    pub def_ranges: RangeSet<Expr>,
    pub dependencies: HashMap<Include, Expr>,
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
    pub typenames: HashSet<String>,
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
            log::debug!("| {}", line);
            if let Some(directive) = directive::parse_directive(line) {
                // log::debug!("{}", line);
                log::debug!("{:?}", &directive);

                match directive {
                    Directive::Include(include) => {
                        dependencies.insert(include, def_ranges.last().1.clone());
                    }
                    Directive::If(expr) => {
                        let pred = Expr::from_cexpr(expr);
                        last_if = Some(pred.clone());
                        def_ranges.push((n, pred));
                    }
                    Directive::IfDefined(name) => {
                        let pred = Expr::Defined(name);
                        last_if = Some(pred.clone());
                        def_ranges.push((n, pred));
                    }
                    Directive::IfNotDefined(name) => {
                        let pred = !Expr::Defined(name);
                        last_if = Some(pred.clone());
                        def_ranges.push((n, pred));
                    }
                    Directive::ElseIf(value) => {
                        def_ranges.pop(n);
                        let pred = Expr::from_cexpr(value);
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
                let expr = expr.eval(ctx).simplify();
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
                    if let Some(directive) = directive::parse_directive(line) {
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
                    None
                } else {
                    Some(Atom {
                        lines: region_lines,
                        expr: expr.clone(),
                    })
                }
            })
            .collect()
    }

    pub fn chunkify(&self, deps: &[&ChunkedUnit], ctx: &Context) -> Result<ChunkedUnit, Error> {
        let mut atom_queue = self.atoms(ctx);
        let mut env = Env::with_msvc();
        for dep in deps {
            for typename in &dep.typenames {
                env.add_typename(typename)
            }
        }

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
                    "Trying to knit too deep, current strand =\n{}",
                    strand.source(&mut strand.atoms.iter())
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

            if strand.has_complete_variants(&mut env, ctx) {
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
                            atom_queue.extend(strand.atoms.drain(..));
                            strand = prev;
                            log::debug!(
                                "Extended strand (len {}) now has source:\n{}",
                                strand.len(),
                                strand.source(&mut strand.atoms.iter())
                            );
                            continue 'knit;
                        }
                        None => {
                            log::debug!("No previous strand, can't backtrack");
                            return Err(Error::CouldNotKnit(
                                strand
                                    .atoms
                                    .iter()
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

        let chunks = strands
            .iter()
            .map(|strand| strand.chunks(&mut env, ctx))
            .flatten()
            .collect();

        let chunks = ChunkedUnit {
            chunks,
            typenames: env.typenames.drain(..).next().unwrap(),
        };
        Ok(chunks)
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

pub fn variations(len: usize, f: &mut dyn FnMut(Box<dyn Iterator<Item = bool>>)) {
    use std::iter::{empty, once};

    match len {
        0 => f(Box::new(empty())),
        n => {
            for &b in &[true, false] {
                variations(n - 1, &mut |it| {
                    f(Box::new(once(b).chain(it)));
                });
            }
        }
    }
}

#[cfg(test)]
#[test]
fn test_variations() {
    fn test(n: usize, expected: &[&[bool]]) {
        let mut vecs = Vec::<Vec<_>>::new();
        variations(n, &mut |v| vecs.push(v.collect()));
        assert_eq!(&vecs[..], expected);
    }

    test(1, &[&[true], &[false]]);
    test(
        2,
        &[
            &[true, true],
            &[true, false],
            &[false, true],
            &[false, false],
        ],
    );
    test(
        3,
        &[
            &[true, true, true],
            &[true, true, false],
            &[true, false, true],
            &[true, false, false],
            &[false, true, true],
            &[false, true, false],
            &[false, false, true],
            &[false, false, false],
        ],
    );
    test(
        4,
        &[
            &[true, true, true, true],
            &[true, true, true, false],
            &[true, true, false, true],
            &[true, true, false, false],
            &[true, false, true, true],
            &[true, false, true, false],
            &[true, false, false, true],
            &[true, false, false, false],
            &[false, true, true, true],
            &[false, true, true, false],
            &[false, true, false, true],
            &[false, true, false, false],
            &[false, false, true, true],
            &[false, false, true, false],
            &[false, false, false, true],
            &[false, false, false, false],
        ],
    );
}

#[cfg(test)]
mod test_parse;

#[cfg(test)]
mod test_chunks;

#[cfg(test)]
mod test_expr;
