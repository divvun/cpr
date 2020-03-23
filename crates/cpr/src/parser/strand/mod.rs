mod variations;
use crate::parser::SourceString;
use variations::variations;

use super::{directive, expr::Expr, Chunk, Context};
use directive::Directive;
use lang_c::{driver, env::Env};

pub struct Atom<'a> {
    pub expr: Expr,
    pub lines: Vec<&'a str>,
}

pub struct Strand<'a> {
    atoms: Vec<Atom<'a>>,
}

impl<'a> Strand<'a> {
    pub fn new() -> Self {
        Self {
            atoms: Default::default(),
        }
    }

    /// Returns true if strand has zero atoms
    pub fn is_empty(&self) -> bool {
        self.atoms.is_empty()
    }

    /// Add a new atom to the list
    pub fn push(&mut self, atom: Atom<'a>) {
        self.atoms.push(atom)
    }

    /// Return the number of atoms in this strand
    pub fn len(&self) -> usize {
        self.atoms.len()
    }

    /// Returns true if all atoms put together parse as a series of C external
    /// declarations
    pub fn has_complete_variants(&self, env: &mut Env, ctx: &Context) -> bool {
        !self.chunks(env, ctx).is_empty()
    }

    pub fn all_atoms(&self) -> impl Iterator<Item = &Atom> {
        self.atoms.iter()
    }

    pub fn into_atoms(self) -> Vec<Atom<'a>> {
        self.atoms
    }

    /// Returns a single String for the source of all given atoms put together
    pub fn source(&self, init_ctx: &Context, iter: &mut dyn Iterator<Item = &Atom>) -> String {
        let mut ctx = init_ctx.clone();
        let mut out = String::new();

        for atom in iter {
            'each_line: for &line in &atom.lines {
                if let Some(directive) =
                    directive::parser::directive(line).expect("should parse all directives")
                {
                    match directive {
                        Directive::Define(d) => {
                            log::debug!("Defining: {:?}", d);
                            ctx.push(d);
                            continue 'each_line;
                        }
                        Directive::Undefine(name) => {
                            log::debug!("Undefining: {:?}", name);
                            ctx.pop(&name);
                            continue 'each_line;
                        }
                        _ => {
                            log::debug!("Ignoring directive {:?}", directive);
                            continue 'each_line;
                        }
                    }
                } else {
                    // non-directive line
                    let tokens =
                        directive::parser::token_stream(line).expect("should tokenize all lines");
                    let line = tokens.expand(&ctx).to_string();

                    log::debug!("Expanded line | {}", &line);
                    out.push_str(&line);
                    out.push('\n');
                }
            }
        }

        // remove final newline, woo
        if out.len() > 0 {
            out.truncate(out.len() - 1)
        }
        out
    }

    /// Parses C code
    pub fn parse(source: String, env: &mut Env) -> Result<driver::Parse, driver::SyntaxError> {
        match lang_c::parser::translation_unit(&source, env) {
            Ok(unit) => Ok(driver::Parse { source, unit }),
            Err(err) => Err(driver::SyntaxError {
                source: source,
                line: err.line,
                column: err.column,
                offset: err.offset,
                expected: err.expected,
            }),
        }
    }

    pub fn chunks(&self, env: &mut Env, ctx: &Context) -> Vec<Chunk> {
        let mut chunks = Vec::new();
        variations(self.len(), &mut |toggles| {
            let pairs: Vec<_> = self.atoms.iter().zip(toggles).collect();
            if let Some((_, false)) = pairs.get(0) {
                // skip first-false
                return;
            }

            let base_expr = pairs.iter().fold(Expr::True, |acc, (atom, on)| {
                acc & if *on {
                    atom.expr.clone()
                } else {
                    !atom.expr.clone()
                }
            });
            let expr = base_expr.constant_fold(ctx).simplify();
            log::debug!(
                "[{}] {:?} => {:?}",
                pairs
                    .iter()
                    .map(|(_, on)| if *on { "1" } else { "0" })
                    .collect::<Vec<_>>()
                    .join(""),
                base_expr,
                expr
            );
            if matches!(expr, Expr::False) {
                log::debug!("(!) Always-false condition");
                return;
            }

            let atoms: Vec<_> = pairs
                .iter()
                .copied()
                .filter_map(|(atom, on)| if on { Some(atom) } else { None })
                .collect();
            if atoms.is_empty() {
                log::debug!("(!) Empty strand");
                return;
            }

            let source = self.source(ctx, &mut atoms.iter().copied());
            log::debug!("Parsing source:\n{:?}", SourceString(source.clone()));
            match Self::parse(source, env) {
                Ok(driver::Parse { source, unit }) => {
                    log::debug!("(✔) Valid chunk");
                    chunks.push(Chunk {
                        source: SourceString(source),
                        unit,
                        expr,
                    })
                }
                Err(e) => {
                    log::debug!("(!) Incomplete strand: {:?}", e);
                }
            }
        });
        if !chunks.is_empty() {
            log::debug!("Found {} complete chunks", chunks.len());
        }
        chunks
    }
}
