#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(unused_mut)]

use super::{ExpandError, THS};
use crate::frontend::{
    grammar::{Define, Token, TokenSeq},
    Context, SymbolState,
};
use std::collections::HashSet;

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

pub fn expand<'a>(
    mut is: Box<dyn Iterator<Item = THS> + 'a>,
    os: &'a mut Vec<THS>,
    ctx: &'a Context,
    depth: usize,
) -> Result<(), ExpandError> {
    let mut cycle = 0;
    let mut rescan = 0;

    'expand_all: loop {
        cycle += 1;
        log::trace!(
            "[depth {}, cycle {}, rescan {}] expand (out len {})",
            depth,
            cycle,
            rescan,
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
                        match def {
                            Define::ObjectLike { value, .. } => {
                                log::trace!("expanding object-like macro {}", def.name());
                                let mut hs = first.1.clone();
                                hs.insert(name.clone());
                                let mut temp = Vec::new();
                                subst(value.as_ths(), &[], &[], &hs, &mut temp, depth + 1);
                                is = Box::new(temp.into_iter().chain(is));
                                rescan += 1;
                                continue 'expand_all;
                            }
                            Define::FunctionLike {
                                value,
                                name,
                                params,
                            } => {
                                let mut saved = vec![];
                                let mut next = skip_ws(&mut is, &mut saved);
                                if let Some(tok) = next {
                                    match tok {
                                        THS(Token::Pun('('), _) => {
                                            log::trace!(
                                                "Found opening paren, first was: {:?}",
                                                first
                                            );

                                            let mut actuals: Vec<Vec<THS>> = vec![vec![]];
                                            let mut depth = 1;
                                            let mut closparen_hs = None;

                                            fn push(actuals: &mut Vec<Vec<THS>>, tok: THS) {
                                                actuals.last_mut().unwrap().push(tok);
                                            }

                                            let mut next = is.next();
                                            while depth > 0 {
                                                match next {
                                                    None => {
                                                        return Err(
                                                            ExpandError::UnclosedMacroInvocation {
                                                                name: name.clone(),
                                                            },
                                                        )
                                                    }
                                                    Some(tok) => {
                                                        match depth {
                                                            1 => match &tok.0 {
                                                                Token::Pun(',') => {
                                                                    actuals.push(vec![]);
                                                                }
                                                                Token::Pun('(') => {
                                                                    depth += 1;
                                                                    push(&mut actuals, tok.clone());
                                                                }
                                                                Token::Pun(')') => {
                                                                    depth -= 1;
                                                                    closparen_hs =
                                                                        Some(tok.1.clone());
                                                                }
                                                                _ => {
                                                                    push(&mut actuals, tok.clone());
                                                                }
                                                            },
                                                            _ => {
                                                                todo!();
                                                            }
                                                        }

                                                        saved.push(tok);
                                                        next = is.next();
                                                    }
                                                }
                                            }

                                            todo!("gotta subst now, actuals = {:?}", actuals);
                                        }
                                        t => {
                                            saved.push(t);
                                        }
                                    }
                                }

                                // rewind
                                is = Box::new(saved.into_iter().chain(is));
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

pub fn skip_ws<'a>(is: &mut dyn Iterator<Item = THS>, saved: &mut Vec<THS>) -> Option<THS> {
    let mut next = is.next();
    loop {
        match next {
            Some(t) => match t.0 {
                Token::WS => {
                    saved.push(t);
                    next = is.next();
                }
                _ => {
                    return Some(t);
                }
            },
            None => return None,
        }
    }
}

pub fn subst<'a>(
    mut is: Box<dyn Iterator<Item = THS> + 'a>,
    fp: &'a [String],
    ap: &'a [Vec<THS>],
    hs: &'a HashSet<String>,
    os: &'a mut Vec<THS>,
    depth: usize,
) {
    let mut cycle = 0;
    let mut rescan = 0;

    'subst_all: loop {
        cycle += 1;
        log::trace!(
            "[depth {}, cycle {}, rescan {}] subst (out len = {})",
            depth,
            cycle,
            rescan,
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
