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
                        if let Define::ObjectLike { value, .. } = def {
                            log::trace!("expanding object-like macro {}", def.name());
                            let mut hs = first.1.clone();
                            hs.insert(name.clone());
                            let mut temp = Vec::new();
                            subst(value.as_ths(), &[], &[], &hs, &mut temp, depth + 1);
                            is = Box::new(temp.into_iter().chain(is));
                            rescan += 1;
                            continue 'expand_all;
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

    #[test]
    fn test_iterative() {
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
}
