#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(unused_mut)]

use super::{ExpandError, HS, THS};
use crate::frontend::{
    grammar::{Define, Token, TokenSeq},
    Context, SymbolState,
};
use std::collections::{HashSet, VecDeque};

trait Expandable2 {
    fn as_ths<'a>(&'a self) -> Box<dyn Iterator<Item = THS> + 'a>;
}

impl Expandable2 for TokenSeq {
    fn as_ths<'a>(&'a self) -> Box<dyn Iterator<Item = THS> + 'a> {
        Box::new(
            self.0
                .iter()
                .map(|tok| THS(tok.clone(), Default::default())),
        )
    }
}

/// Main expand routine, calls `subst`
fn expand<'a>(
    mut is: Box<dyn Iterator<Item = THS> + 'a>,
    os: &'a mut Vec<THS>,
    ctx: &'a Context,
    depth: usize,
) -> Result<(), ExpandError> {
    let mut cycle = 0;

    'expand_all: loop {
        cycle += 1;
        log::trace!(
            "[depth {}, cycle {}] expand (out len {})",
            depth,
            cycle,
            os.len(),
        );

        match is.next() {
            None => {
                // First, if TS is the empty set, the result is the empty set.
                return Ok(());
            }
            Some(first) => {
                // Otherwise, if the token sequence begins with a token whose hide set
                // contains that token, then the result is the token sequence beginning
                // with that token (including its hide set) followed by the result of
                // expand on the rest of the token sequence.
                if first.hides(&first.0) {
                    log::trace!("macro {} is hidden by hideset of {:?}", first.0, first);
                    os.push(first.clone());
                    continue 'expand_all;
                }

                // Otherwise, if the token sequence begins with an object-like macro, the
                // result is the expansion of the rest of the token sequence beginning with
                // the sequence returned by subst invoked with the replacement token
                // sequence for the macro, two empty sets, the union of the macro’s hide set
                // and the macro itself, and an empty set.
                if let Token::Name(name) = &first.0 {
                    if let SymbolState::Defined(def) = ctx.lookup(name) {
                        let mut saved = vec![];
                        match expand_single_macro_invocation(
                            is, os, name, &first, def, &mut saved, depth,
                        )? {
                            BranchOutcome::Advance(rest) => {
                                is = rest;
                                continue 'expand_all;
                            }
                            BranchOutcome::Rewind(rest) => {
                                is = Box::new(saved.into_iter().chain(rest));
                            }
                        }
                    }
                }

                // Verbatim token
                os.push(first);
                continue 'expand_all;
            }
        }
    }
}

pub enum BranchOutcome<'a> {
    Advance(Box<dyn Iterator<Item = THS> + 'a>),
    Rewind(Box<dyn Iterator<Item = THS> + 'a>),
}

/// Expands a single macro invocation, either object-like or function-like
fn expand_single_macro_invocation<'a>(
    mut is: Box<dyn Iterator<Item = THS> + 'a>,
    os: &mut Vec<THS>,
    name: &str,
    first: &THS,
    def: &Define,
    saved: &mut Vec<THS>,
    depth: usize,
) -> Result<BranchOutcome<'a>, ExpandError> {
    match def {
        Define::ObjectLike { value, .. } => {
            log::trace!("expanding object-like macro {}", def.name());
            let mut hs = first.1.clone();
            hs.insert(name.to_string());
            let mut temp = Vec::new();
            subst(value.as_ths(), &[], &[], &hs, &mut temp, depth + 1);
            is = Box::new(temp.into_iter().chain(is));
            return Ok(BranchOutcome::Advance(is));
        }
        Define::FunctionLike {
            value,
            name,
            params,
        } => {
            match skip_ws(&mut is, saved) {
                Some(THS(Token::Pun('('), _)) => {
                    // looks like a function invocation, continue
                }
                mut val => {
                    val.take().map(|tok| saved.push(tok));
                    // rewind
                    return Ok(BranchOutcome::Rewind(is));
                }
            }

            log::trace!("Found opening paren, first was: {:?}", first);
            let res = parse_actuals(&mut is, saved, name)?;
            todo!("gotta subst now, actuals = {:#?}", res);
        }
    }
}

/// Skip whitespace, pushing skipped tokens to `saved` for possible rewinding.
fn skip_ws(is: &mut dyn Iterator<Item = THS>, saved: &mut Vec<THS>) -> Option<THS> {
    let mut next = is.next();
    loop {
        match next {
            // as long as we match whitespace, save it and skip it
            Some(t @ THS(Token::WS, _)) => {
                saved.push(t);
                next = is.next();
            }
            // anything else: stop and return it
            Some(t) => return Some(t),
            None => return None,
        }
    }
}

#[derive(Debug)]
struct ParsedActuals {
    actuals: VecDeque<VecDeque<THS>>,
    closparen_hs: Option<HS>,
}

impl ParsedActuals {
    fn new() -> Self {
        let mut actuals = VecDeque::new();
        actuals.push_back(VecDeque::new());
        Self {
            actuals,
            closparen_hs: None,
        }
    }

    fn push(&mut self, tok: THS) {
        self.actuals.back_mut().unwrap().push_back(tok);
    }

    fn next_arg(&mut self) {
        self.actuals.push_back(VecDeque::new());
    }
}

/// Parse arguments for macro invocations
///
///     FOO(BAR(A, B), C)
///         ^           ^
///         starting    ending here
///         here
///
fn parse_actuals<'a>(
    is: &mut dyn Iterator<Item = THS>,
    saved: &mut Vec<THS>,
    name: &str,
) -> Result<ParsedActuals, ExpandError> {
    let mut res = ParsedActuals::new();
    let mut next = is.next();
    let mut depth = 1;

    while depth > 0 {
        match next {
            None => {
                return Err(ExpandError::UnclosedMacroInvocation {
                    name: name.to_string(),
                })
            }
            Some(tok) => {
                match depth {
                    1 => match &tok.0 {
                        Token::Pun(',') => {
                            res.next_arg();
                        }
                        Token::Pun('(') => {
                            depth += 1;
                            res.push(tok.clone());
                        }
                        Token::Pun(')') => {
                            depth -= 1;
                            res.closparen_hs = Some(tok.1.clone());
                        }
                        _ => {
                            res.push(tok.clone());
                        }
                    },
                    _ => {
                        // depth > 1 - keep track of parens but do not advance
                        // arguments
                        match &tok.0 {
                            Token::Pun('(') => depth += 1,
                            Token::Pun(')') => depth -= 1,
                            _ => {}
                        };
                        res.push(tok.clone());
                    }
                }

                saved.push(tok);
                next = is.next();
            }
        }
    }

    Ok(res)
}

fn subst<'a>(
    mut is: Box<dyn Iterator<Item = THS> + 'a>,
    fp: &'a [String],
    ap: &'a [Vec<THS>],
    hs: &'a HashSet<String>,
    os: &'a mut Vec<THS>,
    depth: usize,
) {
    let mut cycle = 0;

    'subst_all: loop {
        cycle += 1;
        log::trace!(
            "[depth {}, cycle {}] subst (out len = {})",
            depth,
            cycle,
            os.len()
        );

        match is.next() {
            None => {
                log::trace!("subst => empty");
                return;
            }
            Some(first) => {
                // TODO: stringizing
                // TODO: token pasting (argument lhs)
                // TODO: token pasting (non-argument)
                // TODO: token pasting (argument lhs)

                // Regular argument replacement

                // Verbatim token
                let mut tok = first.clone();
                tok.1.extend(hs.iter().cloned());
                os.push(tok);
                continue 'subst_all;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::grammar::MacroParams;

    #[test]
    fn test_iterative1() {
        let ts: TokenSeq = vec![Token::name("ONE"), '+'.into(), Token::int(3)].into();
        dbg!(&ts);

        let is = ts.as_ths();
        let mut os = Vec::new();
        let mut ctx = Context::new();
        ctx.push(Define::ObjectLike {
            name: "ONE".into(),
            value: vec![Token::name("TWO")].into(),
        });
        ctx.push(Define::ObjectLike {
            name: "TWO".into(),
            value: vec![Token::name("THREE")].into(),
        });
        ctx.push(Define::ObjectLike {
            name: "THREE".into(),
            value: vec![Token::name("FOUR")].into(),
        });
        ctx.push(Define::ObjectLike {
            name: "FOUR".into(),
            value: vec![Token::name("FIVE")].into(),
        });
        ctx.push(Define::ObjectLike {
            name: "FIVE".into(),
            value: vec![Token::int(5)].into(),
        });
        expand(is, &mut os, &ctx, 0).unwrap();

        dbg!(&os);
    }

    #[test]
    fn test_iterative2() {
        let ts: TokenSeq = vec![
            Token::name("ADD"),
            Token::WS,
            Token::WS,
            Token::WS,
            '('.into(),
            Token::int(1),
            ','.into(),
            Token::int(2),
            ')'.into(),
        ]
        .into();
        dbg!(&ts);

        let is = ts.as_ths();
        let mut os = Vec::new();
        let mut ctx = Context::new();
        ctx.push(Define::FunctionLike {
            name: "ADD".into(),
            params: MacroParams {
                names: vec!["x".into(), "y".into()],
                has_trailing: false,
            },
            value: vec![Token::name("x"), '+'.into(), Token::name("y")].into(),
        });
        expand(is, &mut os, &ctx, 0).unwrap();

        dbg!(&os);
    }

    #[test]
    fn test_iterative3() {
        let ts: TokenSeq = vec![
            Token::name("ADD"),
            Token::WS,
            Token::WS,
            Token::WS,
            '('.into(),
            Token::int(1),
            ','.into(),
            Token::int(2),
            ')'.into(),
        ]
        .into();
        dbg!(&ts);

        let is = ts.as_ths();
        let mut os = Vec::new();
        let mut ctx = Context::new();
        expand(is, &mut os, &ctx, 0).unwrap();

        dbg!(&os);
    }
}
