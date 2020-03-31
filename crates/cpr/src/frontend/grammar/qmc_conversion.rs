use super::Expr;
use quine_mc_cluskey as qmc;
use std::collections::HashMap;

// Maps cpr `Expr` to an `u8` term for Quine Mc-Cluskey simplification.
pub struct Terms {
    map: HashMap<Expr, u8>,
}

impl Terms {
    /// Same term limit as clippy (per @oli_obk), "9 or 10 is probably still reasonable,
    /// just not for a tool that should be fast"
    const MAX_TERMS: usize = 8;

    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn add(&mut self, e: &Expr) -> qmc::Bool {
        qmc::Bool::Term(match self.map.get(e) {
            Some(&t) => t,
            None => {
                let next_term = self.map.len();
                if next_term >= Self::MAX_TERMS {
                    panic!("refusing to add more than {} terms", Self::MAX_TERMS);
                }
                let next_term = next_term as u8;
                self.map.insert(e.clone(), next_term as u8);
                next_term
            }
        })
    }
}

/// Can be turned into a QMC boolean expression
pub trait AsBool {
    fn as_bool(&self, terms: &mut Terms) -> qmc::Bool;
}

/// Can be built form a QMC boolean expression
pub trait FromBool {
    fn from_bool(v: qmc::Bool, terms: &Terms) -> Self;
}

impl AsBool for Expr {
    fn as_bool(&self, terms: &mut Terms) -> qmc::Bool {
        use qmc::Bool;
        use Expr::*;

        match self {
            Integer(0) => Bool::False,
            Integer(_) => Bool::True,
            And(c) => Bool::And(c.into_iter().map(|v| v.as_bool(terms)).collect()),
            Or(c) => Bool::Or(c.into_iter().map(|v| v.as_bool(terms)).collect()),
            Not(v) => Bool::Not(Box::new(v.as_bool(terms))),
            Defined(_) | Symbol(_) | Call(_, _) | Binary(_, _, _) => terms.add(self),
        }
    }
}

impl FromBool for Expr {
    fn from_bool(v: qmc::Bool, terms: &Terms) -> Self {
        use qmc::Bool;
        use Expr::*;

        match v {
            Bool::True => Integer(1),
            Bool::False => Integer(0),
            Bool::And(c) => And(c.into_iter().map(|v| Self::from_bool(v, terms)).collect()),
            Bool::Or(c) => Or(c.into_iter().map(|v| Self::from_bool(v, terms)).collect()),
            Bool::Not(v) => Not(Box::new(Self::from_bool(*v, terms))),
            Bool::Term(t) => {
                // TODO: performance: build a reverse term lookup map once
                // instead of this O(n) lookup
                for (k, &v) in &terms.map {
                    if v == t {
                        return k.clone();
                    }
                }
                panic!("unknown term: {}", t)
            }
        }
    }
}
