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
        !self.chunks(env, ctx).0.is_empty()
    }

    pub fn all_atoms(&self) -> impl Iterator<Item = &Atom> {
        self.atoms.iter()
    }

    pub fn into_atoms(self) -> Vec<Atom<'a>> {
        self.atoms
    }

    pub fn expand_atoms(
        &self,
        init_ctx: &Context,
        iter: &mut dyn Iterator<Item = &Atom>,
    ) -> Vec<(Expr, Vec<String>)> {
        let mut ctx = init_ctx.clone();
        let mut output = vec![(Expr::True, Vec::new())];

        fn push(output: &mut Vec<(Expr, Vec<String>)>, line: String) {
            for (_expr, lines) in output.iter_mut() {
                lines.push(line.clone())
            }
        }

        for atom in iter {
            'each_line: for &line in &atom.lines {
                if let Some(directive) =
                    directive::parser::directive(line).expect("should parse all directives")
                {
                    match directive {
                        Directive::Define(d) => {
                            log::debug!("Defining: {:?}", d);
                            ctx.push(Expr::True, d);
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

                    let expansions = tokens.expand(&ctx);
                    // TODO: add fast path (expansions.len() == 1, expr is True)
                    let mut combined_output = Vec::new();
                    for (l_expr, l_lines) in output.drain(..) {
                        for (r_expr, r_stream) in &expansions {
                            let line = r_stream.to_string();
                            log::debug!("Expanded line | {}", &line);
                            let mut lines = l_lines.clone();
                            lines.push(line);
                            combined_output.push((l_expr.clone() & r_expr.clone(), lines));
                        }
                    }
                    output = combined_output;
                }
            }
        }
        output
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

    pub fn chunks(&self, env: &mut Env, ctx: &Context) -> (Vec<Chunk>, Context) {
        let mut ctx = ctx.clone();

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
            let expr = base_expr.constant_fold(&ctx).simplify();
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

            for (exp_expr, lines) in self.expand_atoms(&ctx, &mut atoms.iter().copied()) {
                let source = lines.join("\n");
                let expr = expr.clone() & exp_expr.clone();

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
            }
        });
        if !chunks.is_empty() {
            log::debug!("Found {} complete chunks", chunks.len());
        }
        (chunks, ctx)
    }
}
