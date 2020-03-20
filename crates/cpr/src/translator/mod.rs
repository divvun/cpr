use lang_c::ast;
use lang_c::span::Node;

mod rg;
mod utils;
use utils::*;

// ==================== weeeeeeeeeeeeeeeeeee

struct Translator {
    unit: rg::Unit,
}

impl Translator {
    fn push<T: Into<rg::TopLevel>>(&mut self, t: T) {
        self.unit.toplevels.push(t.into());
    }

    fn visit_unit(&mut self, unit: &ast::TranslationUnit) {
        for extdecl in nodes(&unit.0) {
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
        let ts = typ
            .typespecs()
            .nth(0)
            .expect("should have exactly one typespec");

        let mut res = match ts {
            ast::TypeSpecifier::Int => rg::Type::Name(rg::Identifier::name("i32")),
            ast::TypeSpecifier::Short => rg::Type::Name(rg::Identifier::name("i16")),
            ast::TypeSpecifier::Char => rg::Type::Name(rg::Identifier::name("i8")),
            ast::TypeSpecifier::Void => rg::Type::Name(rg::Identifier::name("()")),
            ast::TypeSpecifier::TypedefName(Node { node: id, .. }) => {
                rg::Type::Name(rg::Identifier::name(&id.name))
            }
            ast::TypeSpecifier::Struct(Node { node: struty, .. }) => {
                let id = &struty
                    .identifier
                    .as_ref()
                    .expect("anonymous structs are not suported")
                    .node;

                rg::Type::Name(rg::Identifier::struct_name(&id.name))
            }
            _ => unimplemented!(),
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

pub(crate) fn translate_unit(unit: &ast::TranslationUnit) -> rg::Unit {
    let mut translator = Translator {
        unit: rg::Unit { toplevels: vec![] },
    };
    translator.visit_unit(unit);
    translator.unit
}

pub(crate) fn prelude() -> &'static str {
    return "
    #![allow(non_camel_case_types)]
    ";
}
