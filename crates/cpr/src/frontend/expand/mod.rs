//! Expands C macro invocations, as closely to the C spec as possible.
//!
//! See X3J11/86-196, annotated and corrected version available at:
//! https://www.spinellis.gr/blog/20060626/cpp.algo.pdf

use super::{grammar, Context, Define, SymbolState, Token};
use std::{collections::HashSet, fmt};
mod iterative;

/// Hide set
pub type HS = HashSet<String>;

/// THS = Token + Hide set
#[derive(Clone, PartialEq, Eq)]
pub struct THS(Token, HS);

impl fmt::Debug for THS {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl THS {
    /// Returns true if `tok` is hidden by this token's hide set
    fn hides(&self, tok: &Token) -> bool {
        if let Token::Name(name) = tok {
            return self.1.contains(name);
        }
        false
    }

    /// Glue two tokens together, by printing them literally next to each
    /// other and re-lexing them.
    ///
    /// TODO: proper error handling
    fn glue(self, rhs: Self) -> Self {
        let s = format!("{}{}", self.0, rhs.0);
        let parsed = grammar::token_stream(&s)
            .unwrap_or_else(|_| panic!("while pasting tokens {:?} and {:?}", self, rhs));
        let mut parsed = &parsed.0[..];

        while let [Token::WS, rest @ ..] | [rest @ .., Token::WS] = parsed {
            parsed = rest;
        }

        assert_eq!(parsed.len(), 1, "{:?} should be a single token", parsed);
        let token = parsed[0].clone();
        Self(token, hs_union(&self.1, &rhs.1))
    }
}

pub fn concat(l: &[THS], r: &[THS]) -> Vec<THS> {
    l.iter().cloned().chain(r.iter().cloned()).collect()
}

fn ws_triml(is: &[THS]) -> &[THS] {
    match is {
        [THS(Token::WS, _), rest @ ..] => ws_triml(rest),
        rest => rest,
    }
}

fn ws_trimr(is: &[THS]) -> &[THS] {
    if is.len() >= 2 && matches!(is[is.len() - 1], THS(Token::WS, _)) {
        &is[..is.len() - 1]
    } else {
        is
    }
}

fn ws_trimboth(is: &[THS]) -> &[THS] {
    ws_triml(ws_trimr(is))
}

use grammar::{MacroParams, TokenSeq};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ExpandError {
    #[error("unclosed macro invocation: {name}")]
    UnclosedMacroInvocation { name: String },
    #[error("invalid 'defined' operator usage: {0}")]
    InvalidDefined(String),
    #[error("invalid token pasting: {0}")]
    InvalidTokenPaste(String),
    #[error("invalid stringizing: {0}")]
    InvalidStringizing(String),
    #[error("missing macro parameter: {0}")]
    MissingMacroParam(String),
}

impl ExpandError {
    pub fn needs_more(&self) -> bool {
        // N.B. this match looks dumb, but don't just add a catch-all that
        // returns `false` - the truth errors need to be more granular. Some
        // cases of "invalid defined", "invalid token paste" and "invalid
        // stringizing" are *actually* just us needing more input.
        match self {
            ExpandError::UnclosedMacroInvocation { .. } => true,
            ExpandError::InvalidDefined(..) => false,
            ExpandError::InvalidTokenPaste(..) => false,
            ExpandError::InvalidStringizing(..) => false,
            ExpandError::MissingMacroParam(..) => false,
        }
    }
}

pub trait Expandable {
    fn as_ths(&self) -> Vec<THS>;
    fn expand(&self, ctx: &Context) -> Result<TokenSeq, ExpandError>;
}

impl Expandable for TokenSeq {
    fn as_ths(&self) -> Vec<THS> {
        self.0
            .iter()
            .cloned()
            .map(|token| THS(token, Default::default()))
            .collect()
    }

    fn expand(&self, ctx: &Context) -> Result<TokenSeq, ExpandError> {
        // let res = expand_ths(&self.as_ths(), ctx)?;
        // Ok(TokenSeq(res.into_iter().map(|ths| ths.0).collect()))

        let is = iterative::Expandable2::as_ths(self);
        let mut os = vec![];
        iterative::expand(is, &mut os, ctx, 0)?;
        Ok(os.into_iter().map(|ths| ths.0).collect::<Vec<_>>().into())
    }
}

/// Expand a token sequence, in the given context
fn expand_ths(ts: &[THS], ctx: &Context) -> Result<Vec<THS>, ExpandError> {
    log::trace!("expand {:?}", ts);

    // First, if TS is the empty set, the result is the empty set.
    if ts.is_empty() {
        return Ok(Default::default());
    }

    // Otherwise, if the token sequence begins with a token whose hide set
    // contains that token, then the result is the token sequence beginning
    // with that token (including its hide set) followed by the result of
    // expand on the rest of the token sequence.
    let (t, ts_p) = (&ts[0], &ts[1..]);
    if t.hides(&t.0) {
        log::trace!("macro {} is hidden by hideset of {:?}", t.0, t);
        return Ok(concat(&[t.clone()], expand_ths(ts_p, ctx)?.as_ref()));
    }

    // Otherwise, if the token sequence begins with an object-like macro, the
    // result is the expansion of the rest of the token sequence beginning with
    // the sequence returned by subst invoked with the replacement token
    // sequence for the macro, two empty sets, the union of the macro’s hide set
    // and the macro itself, and an empty set.
    if let Token::Name(name) = &t.0 {
        if let SymbolState::Defined(def) = ctx.lookup(name) {
            if let Define::ObjectLike { value, .. } = def {
                log::trace!("expanding object-like macro {}", def.name());
                let mut hs = t.1.clone();
                hs.insert(name.clone());
                let sub = subst(value.as_ths().as_ref(), None, &[], &hs, Default::default());
                return expand_ths(concat(sub.as_ref(), ts_p.as_ref()).as_ref(), ctx);
            }
        }
    }

    match ws_triml(ts_p) {
        [THS(Token::Pun('('), _), rest @ ..] => {
            if let Token::Name(name) = &t.0 {
                if let SymbolState::Defined(def) = ctx.lookup(name) {
                    if let Define::FunctionLike { value, params, .. } = def {
                        log::trace!("expanding function-like macro {}", def.name());

                        let mut input = rest;
                        let mut actuals: Vec<Vec<THS>> = vec![vec![]];
                        let mut depth = 1;
                        let mut closparen_hs = None;

                        fn push(actuals: &mut Vec<Vec<THS>>, tok: THS) {
                            actuals.last_mut().unwrap().push(tok);
                        }

                        while depth > 0 {
                            log::trace!("depth={}, input = {:?}", depth, input);

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
                                    [] => {
                                        return Err(ExpandError::UnclosedMacroInvocation {
                                            name: t.0.to_string(),
                                        })
                                    }
                                },
                                _ => match input {
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
                                    [] => {
                                        return Err(ExpandError::UnclosedMacroInvocation {
                                            name: t.0.to_string(),
                                        });
                                    }
                                },
                            }
                        }

                        actuals = actuals
                            .iter()
                            .map(|is| ws_trimboth(is).iter().cloned().collect::<Vec<_>>())
                            .collect();

                        let ts_pp = input;
                        let closparen_hs = closparen_hs.unwrap(); // note: static analysis gave up

                        let mut hs = HashSet::new();
                        hs.insert(name.into());

                        let sub_hs = hs_union(&hs_intersection(&t.1, closparen_hs), &hs);
                        log::trace!("sub_hs = {:?}", sub_hs);
                        let sub_res = subst(
                            value.as_ths().as_ref(),
                            Some(&params),
                            &actuals[..],
                            &sub_hs,
                            vec![],
                        );
                        log::trace!("sub_res = {:?}", sub_res);
                        log::trace!("ts'' = {:?}", ts_pp);

                        return expand_ths(concat(sub_res.as_ref(), ts_pp).as_ref(), ctx);
                    }
                }
            }
        }
        _ => {}
    }

    // Concatenate strings
    match ts {
        [THS(Token::Str(l), hs_l), rest @ ..] => {
            let mut input = rest;
            let mut hs = hs_l.clone();
            let mut parts = vec![l.as_str()];

            'concat_strings: loop {
                match ws_triml(input) {
                    [THS(Token::Str(r), hs_r), rest @ ..] => {
                        parts.push(r.as_str());
                        hs = hs_union(&hs, &hs_r);
                        input = rest;
                    }
                    _ => break 'concat_strings,
                }
            }
            let rest = input;

            return Ok(concat(
                &[THS(Token::Str(parts.join("")), hs)],
                &expand_ths(rest, ctx)?,
            ));
        }
        _ => {}
    }

    // Defined
    fn expand_defined(
        name: &str,
        hs: &HS,
        rest: &[THS],
        ctx: &Context,
    ) -> Result<Vec<THS>, ExpandError> {
        match ctx.lookup(name) {
            SymbolState::Undefined => Ok(concat(
                &[THS(Token::Int(0), hs.clone())],
                &expand_ths(rest, ctx)?,
            )),
            SymbolState::Defined(_) => Ok(concat(
                &[THS(Token::Int(1), hs.clone())],
                &expand_ths(rest, ctx)?,
            )),
        }
    }

    match ts {
        [THS(Token::Defined, hs), rest @ ..] => match ws_triml(rest) {
            [THS(Token::Name(name), _), rest @ ..] => {
                return expand_defined(name, hs, rest, ctx);
            }
            [THS(Token::Pun('('), _), rest @ ..] => match ws_triml(rest) {
                [THS(Token::Name(name), _), rest @ ..] => match ws_triml(rest) {
                    [THS(Token::Pun(')'), _), rest @ ..] => {
                        return expand_defined(name, hs, rest, ctx);
                    }
                    _ => panic!("missing closing paren for `define({:?})`", name),
                },
                _ => panic!(
                    "expected name or '(' after defined, but instead found {:?}",
                    rest
                ),
            },
            _ => {}
        },
        _ => {}
    }

    match ts {
        [tok, ts_p @ ..] => Ok(concat(&[tok.clone()], &expand_ths(ts_p, ctx)?)),
        _ => unreachable!(),
    }
}

// fp = formal params (parameter names)
// ap = actual params, aka 'actuals' (arguments)
fn subst(
    is: &[THS],
    fp: Option<&MacroParams>,
    ap: &[Vec<THS>],
    hs: &HashSet<String>,
    os: Vec<THS>,
) -> Vec<THS> {
    log::trace!("## subst");
    log::trace!("is = {:?}", is);
    log::trace!("os = {:?}", os);

    // Stringizing
    if let [THS(Token::Stringize, _), rest @ ..] = is {
        if let [THS(Token::Name(name), _), rest @ ..] = ws_triml(rest) {
            if let Some(&i) = fp.and_then(|fp| fp.names.get(name.as_str())) {
                log::trace!("subst => stringizing");
                return subst(
                    rest,
                    fp,
                    ap,
                    hs,
                    concat(os.as_ref(), &[stringize(ap[i].as_ref())]),
                );
            }
        }
    }

    // Token pasting (argument rhs)
    if let [THS(Token::Paste, _), rest @ ..] = is {
        if let [THS(Token::Name(name), _), rest @ ..] = ws_triml(rest) {
            if let Some(&i) = fp.and_then(|fp| fp.names.get(name.as_str())) {
                log::trace!("subst => pasting (argument rhs)");
                let sel = &ap[i];
                if sel.is_empty() {
                    // TODO: missing cond: "only if actuals can be empty"
                    return subst(rest, fp, ap, hs, os);
                } else {
                    return subst(rest, fp, ap, hs, glue(os.as_ref(), sel));
                }
            }
        }
    }

    // Token pasting (non-argument)
    if let [THS(Token::Paste, _), rest @ ..] = is {
        if let [t @ THS { .. }, rest @ ..] = ws_triml(rest) {
            log::trace!("subst => pasting (non-argument)");
            return subst(rest, fp, ap, hs, glue(os.as_ref(), &[t.clone()]));
        }
    }

    // Token pasting (argument lhs)
    if let [THS(Token::Name(name_i), _), rest @ ..] = is {
        if let [pastetok @ THS(Token::Paste, _), rest @ ..] = ws_triml(rest) {
            if let Some(&i) = fp.and_then(|fp| fp.names.get(name_i.as_str())) {
                log::trace!("subst => pasting (argument lhs)");

                let sel_i = ap[i].clone();
                if sel_i.is_empty() {
                    match ws_triml(rest) {
                        [THS(Token::Name(name_j), _), rest @ ..] => {
                            if let Some(&j) = fp.and_then(|fp| fp.names.get(name_j.as_str())) {
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
    }

    // Regular argument replacement
    if let [THS(Token::Name(name), _), rest @ ..] = is {
        if let Some(&i) = fp.and_then(|fp| fp.names.get(name.as_str())) {
            let sel = ap[i].clone();
            log::trace!("subst => argument replacement, sel = {:?}", sel);
            return subst(rest, fp, ap, hs, concat(os.as_ref(), sel.as_ref()));
        }
    }

    // Verbatim token
    if let [tok, rest @ ..] = is {
        log::trace!("subst => verbatim token {:?}, marking with {:?}", tok, hs);
        let mut tok = tok.clone();
        tok.1.extend(hs.iter().cloned());
        subst(rest, fp, ap, hs, concat(os.as_ref(), &[tok]))
    } else {
        log::trace!("subst => empty");
        os
    }
}

fn glue(ls: &[THS], rs: &[THS]) -> Vec<THS> {
    match ls {
        [] => rs.to_vec(),
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

fn stringize(input: &[THS]) -> THS {
    let mut s = String::new();
    use std::fmt::Write;
    for tok in input {
        write!(&mut s, "{}", tok.0).unwrap();
    }
    THS(Token::Str(s), Default::default())
}

pub fn hs_union(l: &HS, r: &HS) -> HS {
    l.union(r).cloned().collect()
}

pub fn hs_intersection(l: &HS, r: &HS) -> HS {
    l.intersection(r).cloned().collect()
}

#[cfg(test)]
mod test_glue;

#[cfg(test)]
mod test_expand;
