use super::{
    directive::{self, PreprocessorIdent},
    Context, Define, SymbolState, Token,
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

/// Hide set
pub type HS = HashSet<String>;

/// THS = Token + Hide set
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct THS(Token, HS);

impl THS {
    fn hides(&self, tok: &Token) -> bool {
        if let Token::Name(name) = tok {
            return self.1.contains(name);
        }
        false
    }

    fn glue(self, rhs: Self) -> Self {
        // weeeeeeeeeeeeeeeeee
        let s = format!("{}{}", self.0, rhs.0);
        let mut parsed = directive::parser::token_stream(&s)
            .expect(&format!("while pasting tokens {:?} and {:?}", self, rhs));

        assert_eq!(parsed.0.len(), 1);
        let token = parsed.0.pop().unwrap();
        Self(token, hs_union(&self.1, &rhs.1))
    }
}

#[cfg(test)]
mod test_glue {
    use super::*;

    #[test]
    fn test_glue_identifiers() {
        let l = THS(Token::name("foo"), Default::default());
        let r = THS(Token::name("bar"), Default::default());
        assert_eq!(l.glue(r), THS(Token::name("foobar"), Default::default()),);
    }

    #[test]
    fn test_glue_integers() {
        let l = THS(Token::Int(12), Default::default());
        let r = THS(Token::Int(345), Default::default());
        assert_eq!(l.glue(r), THS(Token::Int(12345), Default::default()),);
    }
}

pub fn concat(l: &[THS], r: &[THS]) -> Vec<THS> {
    l.iter().cloned().chain(r.iter().cloned()).collect()
}

fn skip_ws(is: &[THS]) -> &[THS] {
    match is {
        [THS(Token::WS, _), rest @ ..] => skip_ws(rest),
        rest => rest,
    }
}

// See X3J11/86-196, annotated and corrected version available at:
// https://www.spinellis.gr/blog/20060626/cpp.algo.pdf
//
// t = token
// ts = token stream
// hs = hide set
pub fn expand(ts: &[THS], ctx: &Context) -> Vec<THS> {
    // First, if TS is the empty set, the result is the empty set.
    if ts.is_empty() {
        return vec![];
    }

    // Otherwise, if the token sequence begins with a token whose hide set
    // contains that token, then the result is the token sequence beginning
    // with that token (including its hide set) followed by the result of
    // expand on the rest of the token sequence.
    let (t, ts_p) = (&ts[0], &ts[1..]);
    if t.hides(&t.0) {
        return concat(&[t.clone()], expand(ts_p, ctx).as_ref());
    }

    // Otherwise, if the token sequence begins with an object-like macro, the
    // result is the expansion of the rest of the token sequence beginning with
    // the sequence returned by subst invoked with the replacement token
    // sequence for the macro, two empty sets, the union of the macro’s hide set
    // and the macro itself, and an empty set.
    if let Token::Name(name) = &t.0 {
        if let SymbolState::Defined((_expr, def)) = ctx.lookup(name) {
            if let Define::ObjectLike { value, .. } = def {
                let mut hs = t.1.clone();
                hs.insert(name.clone());
                let sub = subst(
                    value.as_expand_tokens().as_ref(),
                    &[],
                    &[],
                    &hs,
                    Default::default(),
                );
                return expand(concat(sub.as_ref(), ts_p.as_ref()).as_ref(), ctx);
            }
        }
    }

    match skip_ws(ts_p) {
        [THS(Token::Pun('('), _), rest @ ..] => {
            if let Token::Name(name) = &t.0 {
                if let SymbolState::Defined((_expr, def)) = ctx.lookup(name) {
                    if let Define::FunctionLike { value, params, .. } = def {
                        let mut input = rest;
                        let mut actuals: Vec<Vec<THS>> = Vec::new();
                        let mut depth = 1;
                        let mut closparen_hs = None;

                        fn push(actuals: &mut Vec<Vec<THS>>, tok: THS) {
                            actuals.last_mut().unwrap().push(tok);
                        }

                        while depth > 0 {
                            input = skip_ws(input);

                            match depth {
                                1 => match input {
                                    [THS(Token::Pun(','), _), rest @ ..] => {
                                        actuals.push(vec![]);
                                        input = rest;
                                    }
                                    [tok @ THS(Token::Pun('('), _), rest @ ..] => {
                                        depth += 1;
                                        push(&mut actuals, tok.clone());
                                        input = rest;
                                    }
                                    [THS(Token::Pun(')'), hs), rest @ ..] => {
                                        depth -= 1;
                                        closparen_hs = Some(hs);
                                        input = rest;
                                    }
                                    [tok, rest @ ..] => {
                                        push(&mut actuals, tok.clone());
                                        input = rest;
                                    }
                                    [] => panic!(
                                        "unterminated paren in call to macro {:?}: {:?}",
                                        t, rest
                                    ),
                                },
                                _ => match rest {
                                    [tok @ THS(Token::Pun('('), _), rest @ ..] => {
                                        depth += 1;
                                        push(&mut actuals, tok.clone());
                                        input = rest;
                                    }
                                    [tok @ THS(Token::Pun(')'), _), rest @ ..] => {
                                        depth -= 1;
                                        push(&mut actuals, tok.clone());
                                        input = rest;
                                    }
                                    [tok, rest @ ..] => {
                                        push(&mut actuals, tok.clone());
                                        input = rest;
                                    }
                                    [] => panic!(
                                        "unterminated paren in call to macro {:?}: {:?}",
                                        t, rest
                                    ),
                                },
                            }
                        }

                        let ts_pp = input;
                        let closparen_hs = closparen_hs.unwrap(); // note: static analysis gave up
                        let mut hs = HashSet::new();
                        hs.insert(name.into());

                        return expand(
                            concat(
                                subst(
                                    value.as_expand_tokens().as_ref(),
                                    &params.names[..],
                                    &actuals[..],
                                    &hs_union(&hs_intersection(&t.1, closparen_hs), &hs),
                                    vec![],
                                )
                                .as_ref(),
                                ts_pp,
                            )
                            .as_ref(),
                            ctx,
                        );
                    }
                }
            }
        }
        _ => {}
    }

    todo!()
}

// fp = formal params (parameter names)
// ap = actual params, aka 'actuals' (arguments)
pub fn subst(
    is: &[THS],
    fp: &[String],
    ap: &[Vec<THS>],
    hs: &HashSet<String>,
    os: Vec<THS>,
) -> Vec<THS> {
    if is.is_empty() {
        return hs_add(hs, os);
    }

    // Stringizing
    match is {
        [THS(Token::Stringize, _), rest @ ..] => match skip_ws(rest) {
            [THS(Token::Name(name), _), rest @ ..] => {
                if let Some(i) = fp.iter().position(|x| x == name) {
                    return subst(
                        rest,
                        fp,
                        ap,
                        hs,
                        concat(os.as_ref(), &[stringize(ap[i].as_ref())]),
                    );
                }
            }
            _ => {}
        },
        _ => {}
    }

    // Token pasting (argument rhs)
    match is {
        [THS(Token::Paste, _), rest @ ..] => match skip_ws(rest) {
            [THS(Token::Name(name), _), rest @ ..] => {
                if let Some(i) = fp.iter().position(|x| x == name) {
                    let sel = &ap[i];
                    if sel.is_empty() {
                        // TODO: missing cond: "only if actuals can be empty"
                        return subst(rest, fp, ap, hs, os);
                    } else {
                        return subst(rest, fp, ap, hs, glue(os.as_ref(), sel));
                    }
                }
            }
            _ => {}
        },
        _ => {}
    }

    // Token pasting (non-argument)
    match is {
        [THS(Token::Paste, _), rest @ ..] => match skip_ws(rest) {
            [t @ THS { .. }, rest @ ..] => {
                return subst(rest, fp, ap, hs, glue(os.as_ref(), &[t.clone()]));
            }
            _ => {}
        },
        _ => {}
    }

    // Token pasting (argument lhs)
    match is {
        [THS(Token::Name(name_i), _), rest @ ..] => match skip_ws(rest) {
            [pastetok @ THS(Token::Paste, _), rest @ ..] => {
                if let Some(i) = fp.iter().position(|x| x == name_i) {
                    let sel_i = ap[i].clone();
                    if sel_i.is_empty() {
                        match skip_ws(rest) {
                            [THS(Token::Name(name_j), _), rest @ ..] => {
                                if let Some(j) = fp.iter().position(|x| x == name_j) {
                                    let sel_j = ap[j].clone();
                                    return subst(
                                        rest,
                                        fp,
                                        ap,
                                        hs,
                                        concat(os.as_ref(), sel_j.as_ref()),
                                    );
                                }
                            }
                            _ => {}
                        }

                        return subst(rest, fp, ap, hs, os);
                    } else {
                        return subst(
                            concat(&[pastetok.clone()], rest).as_ref(),
                            fp,
                            ap,
                            hs,
                            concat(os.as_ref(), sel_i.as_ref()),
                        );
                    }
                }
            }
            _ => {}
        },
        _ => {}
    }

    // Regular argument replacement
    match is {
        [THS(Token::Name(name), _), rest @ ..] => {
            if let Some(i) = fp.iter().position(|x| x == name) {
                let sel = ap[i].clone();
                return subst(rest, fp, ap, hs, concat(os.as_ref(), sel.as_ref()));
            }
        }
        _ => {}
    }

    // Non-replaced token
    match is {
        [tok, rest @ ..] => return subst(rest, fp, ap, hs, concat(os.as_ref(), &[tok.clone()])),
        _ => unreachable!(),
    }
}

pub fn glue(ls: &[THS], rs: &[THS]) -> Vec<THS> {
    match ls {
        [] => rs.iter().cloned().collect(),
        _ => {
            let (ls, l) = (&ls[..ls.len() - 1], ls.last().cloned().unwrap());
            let (r, rs) = (rs[0].clone(), &rs[1..]);

            let mut res = Vec::new();
            res.extend(ls.iter().cloned());
            res.push(l.glue(r));
            res.extend(rs.iter().cloned());
            res
        }
    }
}

pub fn stringize(input: &[THS]) -> THS {
    let mut s = String::new();
    use std::fmt::Write;
    for tok in input {
        write!(&mut s, "{}", tok.0).unwrap();
    }
    THS(Token::Str(s), Default::default())
}

pub fn hs_add(hs: &HS, mut os: Vec<THS>) -> Vec<THS> {
    for t in os.iter_mut() {
        t.1.extend(hs.iter().cloned())
    }
    os
}

pub fn hs_union(l: &HS, r: &HS) -> HS {
    l.union(r).cloned().collect()
}

pub fn hs_intersection(l: &HS, r: &HS) -> HS {
    l.intersection(r).cloned().collect()
}

impl TokenStream {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn as_expand_tokens(&self) -> Vec<THS> {
        self.0
            .iter()
            .cloned()
            .map(|token| THS(token, Default::default()))
            .collect()
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
                    [Token::WS, rest @ ..] => {
                        input = rest;
                    }
                    rest => return rest,
                }
            }
        }

        fn parse_defined(mut input: &[Token]) -> Option<ParseResult<&String>> {
            match input {
                [Token::Defined, rest @ ..] => {
                    input = skip_ws(rest);
                }
                _ => return None,
            }

            match input {
                [Token::Pun('('), rest @ ..] => match skip_ws(rest) {
                    [Token::Name(name), rest @ ..] => match skip_ws(rest) {
                        [Token::Pun(')'), rest @ ..] => {
                            return Some(ParseResult { rest, data: name });
                        }
                        _ => None,
                    },
                    _ => None,
                },
                [Token::Name(name), rest @ ..] => return Some(ParseResult { rest, data: name }),
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
                [Token::Pun('('), rest @ ..] => {
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
                        [Token::Pun(','), rest @ ..] => {
                            new_arg(&mut args);
                            input = rest;
                        }
                        [tok @ Token::Pun('('), rest @ ..] => {
                            depth += 1;
                            push(&mut args, tok.clone());
                            input = rest;
                        }
                        [Token::Pun(')'), rest @ ..] => {
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
                        [tok @ Token::Pun('('), rest @ ..] => {
                            depth += 1;
                            push(&mut args, tok.clone());
                            input = rest;
                        }
                        [tok @ Token::Pun(')'), rest @ ..] => {
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
                [Token::Name(name), rest @ ..] if !blacklist.contains(name) => {
                    slice = rest;

                    match ctx.defines.get(name) {
                        None => {} // can't replace,
                        Some(defs) => {
                            let mut combined_output = vec![];
                            for (l_expr, l_stream) in output {
                                for (r_expr, r_def) in defs {
                                    match r_def {
                                        Define::ObjectLike {
                                            value: r_stream, ..
                                        } => {
                                            combined_output.push((
                                                l_expr.clone() & r_expr.clone(),
                                                l_stream.clone() + r_stream.clone(),
                                            ));
                                        }
                                        Define::FunctionLike {
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
                                                    Token::Name(name) => {
                                                        if let Some(arg) = param_map.get(name) {
                                                            res.0.extend(arg.0.iter().cloned());
                                                        } else {
                                                            res.0.push(Token::Name(name.clone()));
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

                    push(&mut output, Token::Name(name.clone()));
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
        out.0.push('!'.into());
        out.0.push('('.into());
        out.0.extend(self.0);
        out.0.push(')'.into());
        out
    }
}

impl BitAnd for TokenStream {
    type Output = TokenStream;
    fn bitand(self, rhs: TokenStream) -> TokenStream {
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
