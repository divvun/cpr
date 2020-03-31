//! C lexer (parses into token) and constant expression parser (for #if, #elseif, etc.)

mod qmc_conversion;
pub use qmc_conversion::*;

use peg::ParseLiteral;
use std::{
    fmt,
    ops::{Add, BitAnd, BitOr, Not},
    path::{Path, PathBuf},
};

peg::parser! { pub(crate) grammar rules() for str {
    /// 0+ whitespace
    rule _()
        = quiet!{[' ' | '\t']*}

    /// 1+ whitespace
    rule __()
        = quiet!{[' ' | '\t']+}

    /// matches end of file (end of input)
    rule eof()
        = _ ![_] // 0+ whitespace then eof
        / expected!("eof")

    /// matches name literally
    rule N(name: &str)
        = ##parse_string_literal(name)

    /// Parses line (no CR/LF, no comments, no line continuations) as directive or None
    pub rule directive() -> Option<Directive>
        = _ d:directive0() { d }

    rule directive0() -> Option<Directive>
        = "#" _ d:directive1() { Some(d) }
        / eof() { None } // empty line
        / !"#" [_]* eof() { None } // non-directive line

    rule directive1() -> Directive
        = N("include") __ i:include() { Directive::Include(i) }
        / N("if") __ t:token_stream() { Directive::If(t) }
        / N("elif") __ t:token_stream() { Directive::ElseIf(t) }
        / N("ifdef") __ i:identifier() { Directive::If(
            vec![
                Token::Defined,
                '('.into(),
                Token::Name(i),
                ')'.into(),
            ].into()
        ) }
        / N("ifndef") __ i:identifier() { Directive::If(
            vec![
                Token::Pun('!'),
                Token::Defined,
                '('.into(),
                Token::Name(i),
                ')'.into(),
            ].into()
        ) }
        / N("else") eof() { Directive::Else }
        / N("endif") eof() { Directive::EndIf }
        / N("define") __ d:define() { Directive::Define(d) }
        / N("undef") __ n:undef() { Directive::Undefine(n) }
        / N("error") __ s:$([_]*) { Directive::Error(s.into()) }
        / N("pragma") __ s:$([_]*) { Directive::Pragma(s.into()) }
        / l:$(![' '][_]+) __ r:$([_]*)  { Directive::Unknown(l.into(), r.into()) }
        / expected!("directive name")

    rule undef() -> String
        = n:identifier() eof() { n }

    rule include() -> Include
        = t:include_token() eof() { t }

    rule include_token() -> Include
        = "<" p:$((!['>'][_])+) ">" { Include::System(p.into()) }
        / "\"" p:$((!['"'][_])+) "\"" { Include::Quoted(p.into()) }
        / t:token_stream() { Include::TokenSeq(t) }

    rule define() -> Define
        = define_function_like()
        / define_object_like()

    rule define_function_like() -> Define
        = name:identifier() "(" _ params:macro_params() _ ")" value:spaced_token_stream()? {
            Define::FunctionLike {
                name,
                params,
                value: value.unwrap_or(vec![].into()),
            }
        }

    rule define_object_like() -> Define
        = name:identifier() value:spaced_token_stream()? {
            Define::ObjectLike {
                name,
                value: value.unwrap_or(vec![].into()),
            }
        }

    // 1+ whitespace, then a token stream
    rule spaced_token_stream() -> TokenSeq
        = __ value:token_stream() { value }

    // valid C identifier, also valid macro name
    rule identifier() -> String
        // note: '$' is accepted by MSVC, Clang and GCC in identifiers, see C99 Standard, Annex J, J.5.2
        = n:$(['_' | '$' | 'a'..='z' | 'A'..='Z'] ['_' | '$' | 'a'..='z' | 'A'..='Z' | '0'..='9']*) {
            n.into()
        }

    // parses (x, y, z), (a, b, ...), (...)
    rule macro_params() -> MacroParams
        = _ "..." {
            MacroParams {
                names: vec![],
                has_trailing: true,
            }
        }
        / names:identifier() ** (_ "," _) _ e:("," _ "...")? {
            MacroParams {
                names,
                has_trailing: e.is_some(),
            }
        }

    pub rule token_stream() -> TokenSeq
        = t:token()* eof() { t.into() }

    pub rule token() -> Token
        = __                 { Token::WS }
        / k:tok_operator()   { k }
        / k:tok_string()     { k }
        / k:tok_punctuator() { k }
        / k:identifier()     { Token::Name(k) }
        / k:tok_integer()    { Token::Int(k) }
        / expected!("token")

    rule tok_string() -> Token
        = "\"" n:string_tok()* string_end() { Token::Str(n.join("")) }

    rule string_tok() -> &'input str
        = n:$("\"" _ "\"") { "" }
        / n:$("\\\"")      { n }
        / n:$(!['"'][_])   { n }

    rule string_end()
        = "\""
        / expected!("end of string")

    rule tok_integer() -> i64
        = "0x" s:$(tok_hex_integer()+) int_suffix() {? i64::from_str_radix(&s, 16).map_err(|_| "bad hex constant") }
        / "0" s:$(tok_oct_integer()+) int_suffix() {? i64::from_str_radix(&s, 8).map_err(|_| "bad oct constant") }
        / s:$(tok_dec_integer()+) int_suffix() {? i64::from_str_radix(&s, 10).map_err(|_| "bad decimal constant") }

    rule int_suffix()
        = ['u' | 'U']? ['l' | 'L']? ['l' | 'L']?

    rule tok_hex_integer() -> String
        = e:$(['0'..='9' | 'A'..='F' | 'a'..='f']) { e.into() }

    rule tok_oct_integer() -> String
        = e:$(['0'..='7']) { e.into() }

    rule tok_dec_integer() -> String
        = e:$(['0'..='9']) { e.into() }

    rule tok_operator() -> Token
        = "##"      { Token::Paste }
        / "#@"      { Token::Charize }
        / "#"       { Token::Stringize }
        / "defined" { Token::Defined }

    rule tok_punctuator() -> Token
        = s:$([
            '!' | '%' | '^' | '&' | '*' | '(' | ')' | '-' | '+' | '=' |
            '{' | '}' | '|' | '~' | '[' | ']' | '\\'| ';' | ':' | '\''|
            '"' | '<' | '>' | '?' | ',' | '.' | '/' | '@'
          ]) { s.chars().next().unwrap().into() }

    pub rule expr() -> Expr
        = e:expr0() eof() { e }

    pub rule expr0() -> Expr = precedence!{
        // precedence 15 (lowest)
        l:(@) _ "||" _ r:@ { l | r }
        --
        // precedence 14
        l:(@) _ "&&" _ r:@ { l & r }
        --
        // precedence 13
        l:(@) _ "|"  _ r:@ { BinaryOperator::BitwiseOr.build(l, r) }
        --
        // precedence 12
        l:(@) _ "^"  _ r:@ { BinaryOperator::BitwiseXor.build(l, r) }
        --
        // precedence 11
        l:(@) _ "&"  _ r:@ { BinaryOperator::BitwiseAnd.build(l, r)  }
        --
        // precedence 10
        l:(@) _ "==" _ r:@ { BinaryOperator::Equals.build(l, r)  }
        l:(@) _ "!=" _ r:@ { BinaryOperator::NotEquals.build(l, r)  }
        --
        // precedence 9
        l:(@) _ ">"  _ r:@ { BinaryOperator::Greater.build(l, r)  }
        l:(@) _ ">=" _ r:@ { BinaryOperator::GreaterOrEqual.build(l, r)  }
        l:(@) _ "<"  _ r:@ { BinaryOperator::Less.build(l, r)  }
        l:(@) _ "<=" _ r:@ { BinaryOperator::LessOrEqual.build(l, r)  }
        --
        // precedence 7
        l:(@) _ "<<" _ r:@ { BinaryOperator::LeftShift.build(l, r) }
        l:(@) _ ">>" _ r:@ { BinaryOperator::RightShift.build(l, r) }
        --
        // precedence 6
        l:(@) _ "+"  _ r:@ { BinaryOperator::Add.build(l, r) }
        l:(@) _ "-"  _ r:@ { BinaryOperator::Subtract.build(l, r) }
        --
        // precedence 5
        l:(@) _ "*"  _ r:@ { BinaryOperator::Multiply.build(l, r) }
        l:(@) _ "/"  _ r:@ { BinaryOperator::Divide.build(l, r) }
        l:(@) _ "*"  _ r:@ { BinaryOperator::Modulo.build(l, r) }
        --
        // precendence 3
        "!" _ x:@ { Expr::Not(Box::new(x)) }
        --
        "defined" _ name:identifier() { Expr::Defined(name) }
        "defined" _ "(" _ name:identifier() _ ")" { Expr::Defined(name) }
        --
        i:tok_integer() { Expr::Integer(i) }
        callee:identifier() _ "(" args:expr0() ** (_ "," _) ")" _ { Expr::Call(callee, args) }
        --
        name:identifier() { Expr::Symbol(name) }
        "(" _ e:expr0() _ ")" { e }
        // highest precedence
    }

    pub rule pragma() -> TokenSeq
        = "__pragma" t:token_stream() { t }
}}

pub use rules::*;

/// A single C token
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
    /// Builds a Token::Name
    pub fn name(s: &str) -> Self {
        Self::Name(s.to_string())
    }

    /// Builds a Token::Int
    pub fn int(i: i64) -> Self {
        Self::Int(i)
    }

    /// Builds a Token::Int(i) where i is 1 if b is true, 0 otherwise
    pub fn bool(b: bool) -> Self {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenSeq(pub Vec<Token>);

impl From<Vec<Token>> for TokenSeq {
    fn from(tokens: Vec<Token>) -> Self {
        Self(tokens)
    }
}

impl fmt::Display for TokenSeq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for tok in &self.0 {
            write!(f, "{}", tok)?;
        }
        Ok(())
    }
}

impl Add for TokenSeq {
    type Output = TokenSeq;
    fn add(mut self, mut rhs: TokenSeq) -> Self::Output {
        self.0.append(&mut rhs.0);
        self
    }
}

impl Not for TokenSeq {
    type Output = TokenSeq;
    fn not(self) -> Self::Output {
        let mut out = Self::new();
        out.0.push('!'.into());
        out.0.push('('.into());
        out.0.extend(self.0);
        out.0.push(')'.into());
        out
    }
}

impl BitAnd for TokenSeq {
    type Output = TokenSeq;
    fn bitand(self, rhs: TokenSeq) -> TokenSeq {
        let mut out = Self::new();
        out.0.push('('.into());
        out.0.push('('.into());
        out.0.extend(self.0);
        out.0.push(')'.into());
        out.0.push('&'.into());
        out.0.push('&'.into());
        out.0.push(')'.into());
        out.0.extend(rhs.0);
        out.0.push(')'.into());
        out.0.push(')'.into());
        out
    }
}

impl TokenSeq {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn parse_as_expr(&self) -> Expr {
        let source = self.to_string();
        let res = rules::expr(&source).expect("all exprs should parse");
        res
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroParams {
    pub names: Vec<String>,
    pub has_trailing: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Define {
    ObjectLike {
        name: String,
        value: TokenSeq,
    },
    FunctionLike {
        name: String,
        params: MacroParams,
        value: TokenSeq,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    If(TokenSeq),
    Else,
    ElseIf(TokenSeq),
    EndIf,
    Include(Include),
    Define(Define),
    Undefine(String),
    Error(String),
    Pragma(String),
    Unknown(String, String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Include {
    System(PathBuf),
    Quoted(PathBuf),
    TokenSeq(TokenSeq),
}

impl fmt::Display for Include {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Include::System(p) => write!(f, "<{}>", p.to_string_lossy()),
            Include::Quoted(p) => write!(f, r#""{}""#, p.to_string_lossy()),
            Include::TokenSeq(ts) => write!(f, "{}", ts),
        }
    }
}

impl Include {
    fn resolve_system(candidate: &Path, system_paths: &[PathBuf]) -> Option<PathBuf> {
        for path in system_paths {
            let merged = path.join(candidate);
            if merged.exists() {
                return Some(merged);
            }
        }
        None
    }

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
            Include::System(path) => Self::resolve_system(path, system_paths),
            Include::Quoted(path) => {
                // Fallback to system lookup
                Self::resolve_quoted(path, quoted_paths, working_path)
                    // Fallback to system lookup
                    .or_else(|| Self::resolve_system(path, system_paths))
            }
            Include::TokenSeq(tokens) => {
                unimplemented!("tokens: {:?}", tokens);
            }
        }
    }
}

/// Any preprocessor expression, used in `#if` and `#elif`.
/// Essentially a subset of valid C expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Defined(String),
    Symbol(String),
    Call(String, Vec<Expr>),
    Binary(BinaryOperator, Box<Expr>, Box<Expr>),
    Integer(i64),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    /// <
    Less,
    /// <=
    LessOrEqual,
    /// >
    Greater,
    /// >=
    GreaterOrEqual,
    /// ==
    Equals,
    /// !=
    NotEquals,
    /// |
    BitwiseOr,
    /// &
    BitwiseAnd,
    /// ^
    BitwiseXor,
    /// +
    Add,
    /// -
    Subtract,
    /// *
    Multiply,
    /// /
    Divide,
    /// %
    Modulo,
    /// <<
    LeftShift,
    /// >>
    RightShift,
}

impl BinaryOperator {
    pub fn build(self, l: Expr, r: Expr) -> Expr {
        Expr::Binary(self, Box::new(l), Box::new(r))
    }

    pub fn eval(&self, l: i64, r: i64) -> i64 {
        use BinaryOperator::*;
        fn bool(b: bool) -> i64 {
            if b {
                1
            } else {
                0
            }
        }

        match self {
            Add => l + r,
            Subtract => l - r,
            Multiply => l * r,
            Divide => l / r,
            Modulo => l % r,
            BitwiseOr => l | r,
            BitwiseAnd => l & r,
            BitwiseXor => l ^ r,
            LeftShift => l << r,
            RightShift => l >> r,
            Greater => bool(l > r),
            GreaterOrEqual => bool(l >= r),
            Less => bool(l < r),
            LessOrEqual => bool(l <= r),
            Equals => bool(l == r),
            NotEquals => bool(l != r),
        }
    }
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
            BitwiseAnd => "&",
            BitwiseXor => "^",
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
            Modulo => "%",
            LeftShift => "<<",
            RightShift => ">>",
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;

        match self {
            Integer(i) => write!(f, "{}", i),
            Binary(op, l, r) => write!(f, "({} {} {})", l, op.sign(), r),
            Call(callee, args) => {
                write!(f, "({}(", callee)?;
                for (i, arg) in args.iter().enumerate() {
                    match i {
                        0 => write!(f, "{}", arg),
                        _ => write!(f, ", {}", arg),
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
                        0 => write!(f, "{}", v),
                        _ => write!(f, " && {}", v),
                    }?;
                }
                write!(f, ")")
            }
            Or(c) => {
                write!(f, "(")?;
                for (i, v) in c.iter().enumerate() {
                    match i {
                        0 => write!(f, "{}", v),
                        _ => write!(f, " || {}", v),
                    }?;
                }
                write!(f, ")")
            }
            Not(v) => write!(f, "(!{})", v),
        }
    }
}

impl Default for Expr {
    fn default() -> Expr {
        Expr::Integer(1)
    }
}

impl BitAnd for Expr {
    type Output = Expr;

    fn bitand(self, rhs: Expr) -> Self::Output {
        use std::iter::once;
        use Expr::*;

        match (self, rhs) {
            (_, Integer(0)) | (Integer(0), _) => Expr::bool(false),
            (v, Integer(_)) | (Integer(_), v) => v,
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
            (v, Integer(0)) | (Integer(0), v) => v,
            (_, Integer(_)) | (Integer(_), _) => Expr::bool(true),
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

impl Expr {
    pub fn bool(b: bool) -> Self {
        Self::Integer(if b { 1 } else { 0 })
    }

    // Fold (2 + 2) to 4, etc.
    pub fn constant_fold(&self) -> Expr {
        use Expr::*;

        match self {
            Symbol(_name) => self.clone(),
            Defined(_name) => self.clone(),
            Call(callee, args) => Call(
                callee.clone(),
                args.iter().map(|arg| arg.constant_fold()).collect(),
            ),
            And(c) => And(c.iter().map(|v| v.constant_fold()).collect()),
            Or(c) => Or(c.iter().map(|v| v.constant_fold()).collect()),
            Not(v) => match v.constant_fold() {
                Integer(i) => Integer(!i),
                Not(v) => *v,
                v => !v,
            },
            Binary(op, l, r) => match (l.constant_fold(), r.constant_fold()) {
                (Integer(l), Integer(r)) => Integer(op.eval(l, r)),
                (l, r) => op.build(l, r),
            },
            Integer(_) => self.clone(),
        }
    }

    // Fold expression to a single i64 value, assuming any symbols, calls, etc. are undefined
    pub fn assume_undefined(&self) -> i64 {
        use Expr::*;
        fn bool(b: bool) -> i64 {
            if b {
                1
            } else {
                0
            }
        }

        match self {
            Defined(_) => 0,
            Symbol(_) => 0,
            Call(_, _) => 0,
            Binary(op, l, r) => op.eval(l.assume_undefined(), r.assume_undefined()),
            Integer(i) => *i,
            And(c) => bool(c.iter().all(|v| v.assume_undefined() != 0)),
            Or(c) => bool(c.iter().any(|v| v.assume_undefined() != 0)),
            Not(v) => bool(v.assume_undefined() == 0),
        }
    }

    /// Return truthiness of expression, assuming
    pub fn truthy(&self) -> bool {
        self.assume_undefined() != 0
    }

    /// Simplify "logical and" and "logical or" expressions using
    /// Quine-McCluskey. For example, simplifies (a && !(a && b)) to (a && !b)
    pub fn simplify(&self) -> Expr {
        let mut terms = Terms::new();
        let input = self.as_bool(&mut terms);
        let mut output = input.simplify();
        assert_eq!(output.len(), 1);
        let output = output
            .pop()
            .expect("simplification should yield at least one term");
        Self::from_bool(output, &terms)
    }
}

#[cfg(test)]
mod test_lexer;

#[cfg(test)]
mod test_directive_parser;

#[cfg(test)]
mod test_expr_parser;

#[cfg(test)]
mod test_truthy;

#[cfg(test)]
mod test_constant_fold;
