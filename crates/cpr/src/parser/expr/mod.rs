use super::{
    directive::{self, PreprocessorIdent},
    Context, Define, Punctuator, Token,
};
use qmc_conversion::*;
use std::{
    fmt,
    ops::{BitAnd, BitOr, Not},
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

impl TokenStream {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    // TODO: this isn't compliant because of this sample:
    //
    // #define FOO(x) BAR
    // #define BAR(x) x
    // #if FOO()(24) == 24
    //   this_is_truthy();
    // #endif
    //
    // FOO()(24) expand to BAR(24), so we need to rescan,
    // because if FOO() is expanded in place, `(24)` won't be scanned as a
    // macro invocation.
    //
    // One idea is to *not* write `FOO()`'s expansion, `BAR`, to the
    // output, but to a temporary buffer, and have state that knows if
    // we're reading from the buffer or from the input (slice)
    pub fn expand_in_place(&self, ctx: &Context, output: &mut Self) {
        let mut slice = &self.0[..];
        'outer: loop {
            match slice {
                [] => break 'outer,
                [Token::Identifier(id), rest @ ..] => {
                    slice = rest;
                    match ctx.defines.get(id) {
                        None => output.0.push(Token::Identifier(id.clone())),
                        Some(def) => match def {
                            Define::Replacement { .. } => {
                                // TODO: clone value by replacing args identifiers with their passed value,
                                // also check argument count
                                todo!();
                            }
                            Define::Value { value, .. } => {
                                value.expand_in_place(ctx, output);
                            }
                            Define::Blacklist { .. } => {
                                output.0.push(Token::Identifier(id.clone()))
                            }
                        },
                    };
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
        let source = self.to_string();
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
    pub fn constant_fold(&self, ctx: &Context) -> Self {
        use BinaryOperator as BO;
        use Expr::*;

        // TODO: constant folding
        match self {
            Symbol(_name) => {
                // symbols are resolved during macro expansion,
                // if we still have one we're not getting rid of it
                self.clone()
            }
            Defined(name) => match ctx.is_defined(name) {
                Some(b) => Self::bool(b),
                None => self.clone(),
            },
            Call(callee, args) => Call(
                callee.clone(),
                args.iter().map(|arg| arg.constant_fold(ctx)).collect(),
            ),
            True | False => self.clone(),
            And(c) => And(c.iter().map(|v| v.constant_fold(ctx)).collect()),
            Or(c) => Or(c.iter().map(|v| v.constant_fold(ctx)).collect()),
            Not(v) => match v.constant_fold(ctx) {
                True => False,
                False => True,
                Integer(i) => Integer(!i),
                Not(v) => *v,
                v => !v,
            },
            Binary(op, l, r) => match (l.constant_fold(ctx), r.constant_fold(ctx)) {
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
mod constant_fold_tests {
    use super::*;
    use BinaryOperator as BO;
    use Expr::*;

    fn i(i: i64) -> Expr {
        Expr::Integer(i)
    }

    #[test]
    fn test() {
        let ctx = Context::new();
        assert_eq!(BO::Add.build(i(5), i(2)).constant_fold(&ctx), i(7),);
        assert_eq!(BO::Subtract.build(i(3), i(4)).constant_fold(&ctx), i(-1),);
        assert_eq!(
            BO::Add
                .build(i(2), BO::Multiply.build(i(3), i(6)))
                .constant_fold(&ctx),
            i(20),
        );

        assert_eq!(BO::Less.build(i(3), i(6)).constant_fold(&ctx), True);
    }
}
