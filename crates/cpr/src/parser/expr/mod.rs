use super::{
    directive::{self, PreprocessorIdent},
    Context, Define, Punctuator, Token,
};
use qmc_conversion::*;
use std::{
    fmt,
    ops::{BitAnd, BitOr, Not},
};

pub mod langc_conversion;
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

impl TokenStream {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn expand_in_place(&self, ctx: &Context, output: &mut Self) {
        let mut slice = &self.0[..];
        'outer: loop {
            match slice {
                [] => break 'outer,
                [Token::Identifier(id), Token::Punctuator(Punctuator::ParenOpen), ..] => {
                    let def = ctx
                        .defines
                        .get(id)
                        .expect("function-like macro should be defined");
                    if let Define::Replacement { .. } = def {
                        // TODO: clone value by replacing args identifiers with their passed value,
                        // also check argument count
                        todo!();
                    } else {
                        panic!("{} is not a function-like macro", id);
                    }
                }
                [Token::Identifier(id), rest @ ..] => {
                    slice = rest;
                    if let Some(def) = ctx.defines.get(id) {
                        match def {
                            Define::Value { value, .. } => {
                                value.expand_in_place(ctx, output);
                                continue 'outer;
                            }
                            _ => {}
                        }
                    }
                    output.0.push(Token::Identifier(id.clone()));
                }
                [token, rest @ ..] => {
                    slice = rest;
                    output.0.push(token.clone());
                }
            }
        }
    }

    pub fn expand(&self, ctx: &Context) -> Self {
        let mut res = Self::new();
        self.expand_in_place(ctx, &mut res);
        res
    }

    pub fn parse(&self) -> Expr {
        directive::parser::expr(&self.to_string()).expect("all exprs should parse")
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
        out.0.extend(self.0);
        out.0.push(Punctuator::ParenClose.into());
        out.0.push(Punctuator::Ampersand.into());
        out.0.push(Punctuator::ParenOpen.into());
        out.0.extend(rhs.0);
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
    Call(Box<Expr>, Vec<Expr>),
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
    // Evaluate expression with the current defines
    pub fn eval(&self, ctx: &Context) -> Expr {
        use Expr::*;

        match self {
            True | False => self.clone(),
            And(c) => And(c.iter().map(|v| v.eval(ctx)).collect()),
            Or(c) => Or(c.iter().map(|v| v.eval(ctx)).collect()),
            Not(v) => !v.eval(ctx),
            Defined(name) => match ctx.is_defined(name) {
                Some(true) => True,
                Some(false) => False,
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
