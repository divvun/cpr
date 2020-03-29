use super::{
    directive::{self, PreprocessorIdent},
    Context, Define, Punctuator, SymbolState, Token,
};
use qmc_conversion::*;
use std::{
    collections::{HashMap, HashSet},
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
        self.must_expand_single_with_blacklist(ctx, &HashSet::new())
    }

    pub fn must_expand_single_with_blacklist(
        &self,
        ctx: &Context,
        blacklist: &HashSet<String>,
    ) -> Self {
        let mut expansions = self.expand_with_blacklist(ctx, blacklist);
        assert_eq!(
            expansions.len(),
            1,
            "more than one expansion: not supported for now. expansions = {:?}",
            expansions,
        );
        let (expr, tokens) = expansions.pop().unwrap();
        assert!(expr.truthy());
        tokens
    }

    pub fn expand(&self, ctx: &Context) -> Vec<(Expr, Self)> {
        self.expand_with_blacklist(ctx, &HashSet::new())
    }

    pub fn expand_with_blacklist(
        &self,
        ctx: &Context,
        blacklist: &HashSet<String>,
    ) -> Vec<(Expr, Self)> {
        let mut output = vec![(Expr::bool(true), Self::new())];
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

        fn parse_arglist<'a, 'b>(
            name: &'a str,
            mut input: &'b [Token],
        ) -> ParseResult<'b, Vec<TokenStream>> {
            let original_input = input;

            let mut depth;
            input = skip_ws(input);

            match input {
                [Token::Punctuator(Punctuator::ParenOpen), rest @ ..] => {
                    input = rest;
                    depth = 1;
                }
                _ => panic!(
                    "macro call to {}: missing opening paren, found instead: {:?}",
                    name, input
                ),
            }

            let mut args: Vec<TokenStream> = vec![vec![].into()];

            fn push(args: &mut Vec<TokenStream>, tok: Token) {
                args.last_mut().unwrap().0.push(tok);
            }

            fn new_arg(args: &mut Vec<TokenStream>) {
                args.push(vec![].into());
            }

            while depth > 0 {
                input = skip_ws(input);

                match depth {
                    1 => match input {
                        [Token::Punctuator(Punctuator::Comma), rest @ ..] => {
                            new_arg(&mut args);
                            input = rest;
                        }
                        [tok @ Token::Punctuator(Punctuator::ParenOpen), rest @ ..] => {
                            depth += 1;
                            push(&mut args, tok.clone());
                            input = rest;
                        }
                        [Token::Punctuator(Punctuator::ParenClose), rest @ ..] => {
                            depth -= 1;
                            input = rest;
                        }
                        [tok, rest @ ..] => {
                            push(&mut args, tok.clone());
                            input = rest;
                        }
                        [] => panic!("unterminated call to macro {}: {:?}", name, original_input),
                    },
                    _ => match input {
                        [tok @ Token::Punctuator(Punctuator::ParenOpen), rest @ ..] => {
                            depth += 1;
                            push(&mut args, tok.clone());
                            input = rest;
                        }
                        [tok @ Token::Punctuator(Punctuator::ParenClose), rest @ ..] => {
                            depth -= 1;
                            push(&mut args, tok.clone());
                            input = rest;
                        }
                        [tok, rest @ ..] => {
                            push(&mut args, tok.clone());
                            input = rest;
                        }
                        [] => panic!(
                            "unterminated paren in call to macro {}: {:?}",
                            name, original_input
                        ),
                    },
                }
            }

            ParseResult {
                data: args,
                rest: input,
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
                [Token::Identifier(id), rest @ ..] if !blacklist.contains(id) => {
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
                                        Define::Replacement {
                                            name,
                                            params,
                                            value,
                                        } => {
                                            let res = parse_arglist(name, slice);
                                            slice = res.rest;
                                            let args = res.data;

                                            log::debug!("name = {:?}", name);
                                            log::debug!("params = {:?}", params);
                                            log::debug!("args = {:?}", args);
                                            log::debug!("value = {:?}", value);

                                            let mut macro_blacklist = blacklist.clone();
                                            macro_blacklist.insert(name.clone());

                                            let args: Vec<TokenStream> = args
                                                .into_iter()
                                                .map(|x| {
                                                    x.must_expand_single_with_blacklist(
                                                        ctx,
                                                        &macro_blacklist,
                                                    )
                                                })
                                                .collect();
                                            log::debug!("expanded args = {:?}", args);

                                            let param_map = if params.names.len() == 0 {
                                                HashMap::new()
                                            } else {
                                                // TODO: variadic macros
                                                assert_eq!(params.names.len(), args.len(), "must pass exact number of arguments when invoking macro");
                                                params
                                                    .names
                                                    .iter()
                                                    .zip(args.iter())
                                                    .map(|(name, arg)| (name.to_string(), arg))
                                                    .collect()
                                            };

                                            let mut res: TokenStream = vec![].into();
                                            for tok in &value.0 {
                                                match tok {
                                                    Token::Identifier(id) => {
                                                        if let Some(arg) = param_map.get(id) {
                                                            res.0.extend(arg.0.iter().cloned());
                                                        } else {
                                                            res.0.push(Token::Identifier(
                                                                id.clone(),
                                                            ));
                                                        }
                                                    }
                                                    tok => res.0.push(tok.clone()),
                                                }
                                            }

                                            for (r2_expr, r2_stream) in
                                                res.expand_with_blacklist(ctx, &macro_blacklist)
                                            {
                                                combined_output.push((
                                                    l_expr.clone()
                                                        & r_expr.clone()
                                                        & r2_expr.clone(),
                                                    l_stream.clone() + r2_stream.clone(),
                                                ));
                                            }
                                        }
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
            Not(v) => !v.assume_undefined(),
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
mod test_constant_fold;

#[cfg(test)]
mod test_expand;
