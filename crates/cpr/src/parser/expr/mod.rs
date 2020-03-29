use super::{
    directive::{self, PreprocessorIdent},
    Context, Define, Punctuator, SymbolState, Token,
};
use qmc_conversion::*;
use std::{
    fmt,
    ops::{Add, BitAnd, BitOr, Not},
};

pub mod qmc_conversion;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenStream(pub Vec<Token>);

impl From<Vec<Token>> for TokenStream {
    fn from(tokens: Vec<Token>) -> Self {
        Self(tokens)
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for tok in &self.0 {
            write!(f, "{}", tok)?;
        }
        Ok(())
    }
}

impl Add for TokenStream {
    type Output = TokenStream;
    fn add(mut self, mut rhs: TokenStream) -> Self::Output {
        self.0.append(&mut rhs.0);
        self
    }
}

impl TokenStream {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn must_expand_single(&self, ctx: &Context) -> Self {
        let mut expansions = self.expand(ctx);
        assert_eq!(
            expansions.len(),
            1,
            "more than one expansion: not supported for now. expansions = {:?}",
            expansions,
        );
        assert_eq!(expansions[0].0, Expr::True);
        let tokens = expansions.pop().unwrap().1;
        log::debug!("expanded tokens = {:?}", tokens);
        tokens
    }

    pub fn expand(&self, ctx: &Context) -> Vec<(Expr, Self)> {
        let mut output = vec![(Expr::True, Self::new())];
        let mut slice = &self.0[..];

        fn push(output: &mut Vec<(Expr, TokenStream)>, token: Token) {
            for (_expr, stream) in output.iter_mut() {
                stream.0.push(token.clone());
            }
        }

        struct ParseResult<'a, T> {
            rest: &'a [Token],
            data: T,
        }

        fn skip_ws(mut input: &[Token]) -> &[Token] {
            loop {
                match input {
                    [Token::Whitespace, rest @ ..] => {
                        input = rest;
                    }
                    rest => return rest,
                }
            }
        }

        fn parse_defined(mut input: &[Token]) -> Option<ParseResult<&String>> {
            match input {
                [Token::Keyword(kw), rest @ ..] if kw == "defined" => {
                    input = skip_ws(rest);
                }
                _ => return None,
            }

            match input {
                [Token::Punctuator(Punctuator::ParenOpen), rest @ ..] => match skip_ws(rest) {
                    [Token::Identifier(id), rest @ ..] => match skip_ws(rest) {
                        [Token::Punctuator(Punctuator::ParenClose), rest @ ..] => {
                            return Some(ParseResult { rest, data: id });
                        }
                        _ => None,
                    },
                    _ => None,
                },
                [Token::Identifier(id), rest @ ..] => return Some(ParseResult { rest, data: id }),
                _ => None,
            }
        }

        'outer: loop {
            if let Some(res) = parse_defined(slice) {
                log::debug!("expanding defined({:?})", res.data);
                match ctx.lookup(res.data) {
                    SymbolState::Unknown => {
                        // don't replace anything
                    }
                    SymbolState::Undefined => {
                        slice = res.rest;
                        push(&mut output, Token::bool(false));
                        continue 'outer;
                    }
                    SymbolState::Defined(_) | SymbolState::MultipleDefines(_) => {
                        slice = res.rest;
                        push(&mut output, Token::bool(true));
                        continue 'outer;
                    }
                };
            }

            match slice {
                [] => break 'outer,
                [Token::Identifier(id), rest @ ..] => {
                    slice = rest;

                    match ctx.defines.get(id) {
                        None => {} // can't replace,
                        Some(defs) => {
                            let mut combined_output = vec![];
                            for (l_expr, l_stream) in output {
                                for (r_expr, r_def) in defs {
                                    match r_def {
                                        Define::Value {
                                            value: r_stream, ..
                                        } => {
                                            combined_output.push((
                                                l_expr.clone() & r_expr.clone(),
                                                l_stream.clone() + r_stream.clone(),
                                            ));
                                        }
                                        Define::Replacement { .. } => todo!(),
                                    }
                                }
                            }
                            output = combined_output;
                            continue 'outer;
                        }
                    };

                    push(&mut output, Token::Identifier(id.clone()));
                }
                [token, rest @ ..] => {
                    slice = rest;
                    push(&mut output, token.clone());
                }
            }
        }
        output
    }

    pub fn parse(&self) -> Expr {
        log::debug!("self = {:?}", self);
        let source = self.to_string();
        log::debug!("source = {:?}", source);
        let res = directive::parser::expr(&source).expect("all exprs should parse");
        res
    }
}

impl Not for TokenStream {
    type Output = TokenStream;
    fn not(self) -> Self::Output {
        let mut out = Self::new();
        out.0.push(Punctuator::Bang.into());
        out.0.push(Punctuator::ParenOpen.into());
        out.0.extend(self.0);
        out.0.push(Punctuator::ParenClose.into());
        out
    }
}

impl BitAnd for TokenStream {
    type Output = TokenStream;
    fn bitand(self, rhs: TokenStream) -> TokenStream {
        let mut out = Self::new();
        out.0.push(Punctuator::ParenOpen.into());
        out.0.push(Punctuator::ParenOpen.into());
        out.0.extend(self.0);
        out.0.push(Punctuator::ParenClose.into());
        out.0.push(Punctuator::Ampersand.into());
        out.0.push(Punctuator::Ampersand.into());
        out.0.push(Punctuator::ParenOpen.into());
        out.0.extend(rhs.0);
        out.0.push(Punctuator::ParenClose.into());
        out.0.push(Punctuator::ParenClose.into());
        out
    }
}

/// Any preprocessor expression, used in `#if` and `#elif`.
/// Essentially a subset of valid C expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    True,
    False,
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
            True => write!(f, "true"),
            False => write!(f, "false"),
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

impl PreprocessorIdent for Expr {
    fn ident(&self) -> Vec<String> {
        use Expr::*;

        match self {
            Defined(x) => vec![x.clone()],
            Symbol(x) => vec![x.clone()],
            Integer(_) => vec![],
            Call(callee, args) => {
                let mut res = vec![callee.clone()];
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

impl Expr {
    pub fn bool(b: bool) -> Self {
        if b {
            Self::True
        } else {
            Self::False
        }
    }

    // Fold (2 + 2) to 4, etc.
    pub fn constant_fold(&self) -> Expr {
        use BinaryOperator as BO;
        use Expr::*;

        match self {
            Symbol(_name) => {
                // anything that's not defined is zero
                Integer(0)
            }
            Defined(_name) => {
                // TODO
                self.clone()
            }
            Call(callee, args) => Call(
                callee.clone(),
                args.iter().map(|arg| arg.constant_fold()).collect(),
            ),
            True | False => self.clone(),
            And(c) => And(c.iter().map(|v| v.constant_fold()).collect()),
            Or(c) => Or(c.iter().map(|v| v.constant_fold()).collect()),
            Not(v) => match v.constant_fold() {
                True => False,
                False => True,
                Integer(i) => Integer(!i),
                Not(v) => *v,
                v => !v,
            },
            Binary(op, l, r) => match (l.constant_fold(), r.constant_fold()) {
                (Integer(l), Integer(r)) => match op {
                    BO::Add => Integer(l + r),
                    BO::Subtract => Integer(l - r),
                    BO::Multiply => Integer(l * r),
                    BO::Divide => Integer(l / r),
                    BO::Modulo => Integer(l % r),
                    BO::BitwiseOr => Integer(l | r),
                    BO::BitwiseAnd => Integer(l & r),
                    BO::BitwiseXor => Integer(l ^ r),
                    BO::LeftShift => Integer(l << r),
                    BO::RightShift => Integer(l >> r),
                    BO::Greater => Self::bool(l > r),
                    BO::GreaterOrEqual => Self::bool(l >= r),
                    BO::Less => Self::bool(l < r),
                    BO::LessOrEqual => Self::bool(l <= r),
                    BO::Equals => Self::bool(l == r),
                    BO::NotEquals => Self::bool(l != r),
                },
                (l, r) => op.build(l, r),
            },
            Integer(_) => self.clone(),
        }
    }

    /// Determine expression truthiness - true is truthy, non-zero integers
    /// are truthy, false is falsy, zero is falsy, BUT anything with
    /// an unresolved symbol, a call, whatever - that's neither, ie. None.
    pub fn truthiness(&self) -> Option<bool> {
        use Expr::*;
        match self {
            True => Some(true),
            False => Some(false),
            Integer(i) => Some(*i != 0),
            Symbol(_) => {
                // if we still have a symbol that wasn't expanded, it's 0
                Some(false)
            }
            _ => None,
        }
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
mod test_constant_fold;

#[cfg(test)]
mod test_expand;
