use lang_c::ast;
use lang_c::span::Node;

mod rg;
mod utils;
use utils::*;

struct Translator {
    unit: rg::Unit,
    config: Config,
}

pub struct Config {
    pub arch: Arch,
}

/// Target architectures, named after LLVM
#[derive(Clone, Copy)]
pub enum Arch {
    /// 32-bit X86: Pentium-Pro and above
    X86,
    /// 64-bit X86: EMT64 and AMD64
    X86_64,
}

impl Default for Arch {
    fn default() -> Self {
        Self::X86_64
    }
}

impl argh::FromArgValue for Arch {
    fn from_arg_value(value: &str) -> Result<Self, String> {
        match value {
            "x86" => Ok(Self::X86),
            "x86-64" => Ok(Self::X86_64),
            s => Err(format!("unknown architecture: {:?}", s)),
        }
    }
}

impl Translator {
    fn push<T: Into<rg::TopLevel>>(&mut self, t: T) {
        self.unit.toplevels.push(t.into());
    }

    fn visit_unit(&mut self, declarations: &[ast::ExternalDeclaration]) {
        for extdecl in declarations {
            if let ast::ExternalDeclaration::Declaration(Node {
                node: declaration, ..
            }) = &extdecl
            {
                if declaration.declarators.is_empty() {
                    for spec in nodes(&declaration.specifiers[..]) {
                        self.visit_freestanding_specifier(spec);
                    }
                } else {
                    for init_declarator in nodes(&declaration.declarators[..]) {
                        let declarator = &init_declarator.declarator.node;
                        self.visit_declarator(declaration, declarator);
                    }
                }
            } else {
                log::debug!("visit_unit: not a Declaration: {:#?}", extdecl);
            }
        }
    }

    fn visit_freestanding_specifier(&mut self, spec: &ast::DeclarationSpecifier) {
        if let ast::DeclarationSpecifier::TypeSpecifier(Node { node: tyspec, .. }) = spec {
            match tyspec {
                ast::TypeSpecifier::Struct(Node { node: struty, .. }) => {
                    let st = self.visit_struct(struty);
                    self.push(st);
                }
                _ => {}
            }
        }
    }

    #[must_use]
    fn visit_struct(&mut self, struty: &ast::StructType) -> rg::StructDeclaration {
        let id = match struty.identifier.as_ref().map(borrow_node) {
            Some(x) => x,
            None => panic!("anonymous structs aren't supported"),
        };

        let mut res = rg::StructDeclaration {
            name: rg::Identifier::struct_name(&id.name),
            fields: Default::default(),
        };

        if let Some(declarations) = &struty.declarations {
            for dtion in nodes(&declarations[..]) {
                match dtion {
                    ast::StructDeclaration::Field(Node { node: field, .. }) => {
                        let specifiers = &field.specifiers[..];

                        for dtor in nodes(&field.declarators[..]) {
                            if let Some(Node { node: dtor, .. }) = dtor.declarator.as_ref() {
                                let sftup = StructFieldTuple { field, dtor };
                                log::debug!("{:?} {:?}", specifiers, dtor);

                                let id = match dtor.get_identifier() {
                                    Some(x) => x,
                                    None => panic!("anonymous struct fields aren't supported"),
                                };

                                let typ = self.visit_type(&sftup);
                                let field = rg::StructField {
                                    name: rg::Identifier::name(&id.name),
                                    typ,
                                };
                                res.fields.push(field);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        res
    }

    #[must_use]
    fn visit_type(&mut self, typ: &dyn Typed) -> rg::Type {
        let mut signed = None;
        let mut longness = 0;
        let original_specs: Vec<_> = typ.typespecs().collect();
        let mut specs = &original_specs[..];
        println!("specs = {:?}", specs);

        use ast::TypeSpecifier as TS;
        'process_prefixes: loop {
            match specs {
                [TS::Unsigned, rest @ ..] => {
                    signed = Some(false);
                    specs = rest;
                }
                [TS::Signed, rest @ ..] => {
                    signed = Some(true);
                    specs = rest;
                }
                [TS::Long, rest @ ..] => {
                    longness += 1;
                    specs = rest;
                }
                [TS::Short, rest @ ..] => {
                    longness -= 1;
                    specs = rest;
                }
                [] => {
                    if longness != 0 || signed.is_some() {
                        specs = &[TS::Int];
                        break 'process_prefixes;
                    } else {
                        panic!("unrecognized typespecs: {:#?}");
                    }
                }
                _ => break 'process_prefixes,
            }
        }

        fn pick_sign(signed: Option<bool>, uver: &str, sver: &str) -> rg::Type {
            let name = if signed.unwrap_or(true) { sver } else { uver };
            rg::Type::Name(rg::Identifier::name(name))
        }

        let mut res = match &specs[0] {
            TS::Int => match self.config.arch {
                Arch::X86_64 => match longness {
                    -1 => pick_sign(signed, "u16", "i16"),
                    0 => pick_sign(signed, "u32", "i32"),
                    _ => pick_sign(signed, "u64", "i64"),
                },
                Arch::X86 => match longness {
                    -1 => pick_sign(signed, "u16", "i16"),
                    0 | 1 => pick_sign(signed, "u32", "i32"),
                    _ => pick_sign(signed, "u64", "i64"),
                },
            },
            TS::Char => pick_sign(signed, "u8", "i8"),
            TS::Void => rg::Type::Name(rg::Identifier::name("()")),
            TS::TypedefName(Node { node: id, .. }) => {
                rg::Type::Name(rg::Identifier::name(&id.name))
            }
            TS::Struct(Node { node: struty, .. }) => {
                let id = &struty
                    .identifier
                    .as_ref()
                    .expect("anonymous structs are not suported")
                    .node;

                rg::Type::Name(rg::Identifier::struct_name(&id.name))
            }
            _ => unimplemented!("don't know how to translate type: {:#?}", typ),
        };

        for _d in 0..typ.pointer_depth() {
            res = rg::Type::Pointer {
                konst: typ.is_const(),
                inner: Box::new(res),
            }
        }

        res
    }

    fn visit_declarator(&mut self, dtion: &ast::Declaration, dtor: &ast::Declarator) {
        let id = match dtor.get_identifier() {
            None => {
                log::debug!(
                    "visit_declarator: dtor without identifier {:#?} {:#?}",
                    dtion,
                    dtor
                );
                return;
            }
            Some(x) => x,
        };

        log::debug!("visit_declarator: {}", id.name);

        if let Some(ast::StorageClassSpecifier::Typedef) = dtion.get_storage_class() {
            let typ = self.visit_type(&DeclTuple { dtion, dtor });
            let ad = rg::AliasDeclaration {
                name: rg::Identifier::name(&id.name),
                typ,
            };
            self.push(ad);
        } else if let Some(fdecl) = dtor.get_function() {
            let fd = self.visit_fdecl(dtion, id, dtor, fdecl);
            self.push(fd);
        } else {
            log::debug!(
                "visit_declarator: unsure what to do with {:#?} {:#?}",
                dtion,
                dtor
            );
        }
    }

    #[must_use]
    fn visit_fdecl(
        &mut self,
        dtion: &ast::Declaration,
        id: &ast::Identifier,
        dtor: &ast::Declarator,
        fdecl: &ast::FunctionDeclarator,
    ) -> rg::FunctionDeclaration {
        let ftup = DeclTuple { dtion, dtor };

        let mut res = rg::FunctionDeclaration {
            name: rg::Identifier::name(&id.name),
            params: Default::default(),
            ret: if ftup.is_void() {
                // function is `void fun()`, ignore the void
                None
            } else {
                Some(self.visit_type(&ftup))
            },
        };

        if fdecl.takes_nothing() {
            // function is `fun(void)`, ignore the void
        } else {
            for (i, param) in nodes(&fdecl.parameters[..]).enumerate() {
                let name = param
                    .declarator()
                    .and_then(|dtor| dtor.get_identifier())
                    .map(|id| id.name.clone())
                    .unwrap_or_else(|| format!("__arg{}", i));

                res.params.push(rg::FunctionParam {
                    name: rg::Identifier::name(&name),
                    typ: self.visit_type(param),
                });
            }
        }

        res
    }
}

pub(crate) fn translate_unit(config: Config, decls: &[ast::ExternalDeclaration]) -> rg::Unit {
    let mut translator = Translator {
        unit: rg::Unit { toplevels: vec![] },
        config,
    };
    translator.visit_unit(decls);
    translator.unit
}

pub(crate) fn prelude() -> &'static str {
    return "
    #![allow(non_camel_case_types)]
    ";
}
