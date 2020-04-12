use indexmap::IndexSet;
use lang_c::{ast, span::Node};
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::PathBuf,
};

mod rg;
mod utils;
use utils::*;

struct Translator<'a> {
    unit: rg::Unit,
    config: &'a Config,
    forward_struct_names: IndexSet<String>,
    declared_struct_names: IndexSet<String>,
}

pub struct Config {
    pub arch: Arch,
}

#[derive(Clone, Copy)]
enum StructVisitMode {
    Forward,
    Collect,
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

impl<'a> Translator<'a> {
    fn new(config: &'a Config, path: PathBuf) -> Self {
        Self {
            config,
            unit: rg::Unit::new(path.to_owned()),
            declared_struct_names: Default::default(),
            forward_struct_names: Default::default(),
        }
    }

    fn push<T: Into<rg::TopLevel>>(&mut self, t: T) {
        self.unit.toplevels.push(t.into());
    }

    fn visit_unit(&mut self, declarations: &[ast::ExternalDeclaration]) {
        let stack = &[];

        for extdecl in declarations {
            if let ast::ExternalDeclaration::Declaration(declaration) = &extdecl {
                let declaration = &declaration.node;

                for spec in nodes(&declaration.specifiers) {
                    match spec {
                        ast::DeclarationSpecifier::TypeSpecifier(ts) => {
                            self.predeclare_types(&[], &ts.node);
                        }
                        _ => {}
                    }
                }

                for init_declarator in nodes(&declaration.declarators[..]) {
                    let declarator = &init_declarator.declarator.node;
                    self.visit_declarator(stack, declaration, declarator);
                }
            } else {
                log::debug!("visit_unit: not a Declaration: {:#?}", extdecl);
            }
        }
    }

    // C allows:
    //   typedef struct a { struct b { int field; } b } a;
    // We need to pre-declare `struct b` and `struct a` before visiting
    // the typedef itself.
    fn predeclare_types(&mut self, stack: &[&str], ts: &ast::TypeSpecifier) {
        match ts {
            ast::TypeSpecifier::Struct(struty) => {
                let struty = borrow_node(struty);
                let name = self.visit_struct(&stack[..], struty, StructVisitMode::Forward);
                let stack = &[name.as_ref()];

                if let Some(dtions) = struty.declarations.as_ref() {
                    for dtion in nodes(&dtions) {
                        match dtion {
                            ast::StructDeclaration::Field(sf) => {
                                let sf = &sf.node;
                                for sq in nodes(&sf.specifiers) {
                                    match sq {
                                        ast::SpecifierQualifier::TypeSpecifier(ts) => {
                                            self.predeclare_types(&stack[..], borrow_node(ts));
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            ast::TypeSpecifier::Enum(enumty) => {
                let enumty = borrow_node(enumty);
                let ed = self.visit_enum(enumty);
                self.push(ed);
            }
            _ => {}
        }
    }

    fn visit_struct(
        &mut self,
        stack: &[&str],
        struty: &ast::StructType,
        mode: StructVisitMode,
    ) -> String {
        let name = match struty.identifier.as_ref().map(borrow_node) {
            Some(x) => x.name.clone(),
            None => self.hash_name(stack, &struty),
        };
        let stack = &[name.as_ref()];

        let mut res = rg::StructDeclaration {
            name: rg::Identifier::struct_name(&name),
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

                                let typ = self.visit_type(stack, &sftup);
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

        match mode {
            StructVisitMode::Forward => {
                if res.fields.is_empty() {
                    self.forward_struct_names.insert(res.name.value.clone());
                } else {
                    self.declared_struct_names.insert(res.name.value.clone());
                    self.push(res);
                }
            }
            StructVisitMode::Collect => {
                self.declared_struct_names.insert(res.name.value.clone());
                self.push(res);
            }
        }
        name
    }

    fn visit_enum(&mut self, enumty: &ast::EnumType) -> rg::EnumDeclaration {
        let id = match enumty.identifier.as_ref().map(borrow_node) {
            Some(x) => x.name.clone(),
            None => self.hash_name(&[], &enumty),
        };

        let mut res = rg::EnumDeclaration {
            name: rg::Identifier::enum_name(&id),
            fields: Default::default(),
        };

        for num in nodes(&enumty.enumerators) {
            let field_id = &num.identifier.node.name;

            let value = num.expression.as_ref().map(|x| x.node.as_expr(self));
            res.fields.push(rg::EnumField {
                name: rg::Identifier::name(field_id),
                value,
            });
        }

        res
    }

    #[must_use]
    fn visit_type(&self, stack: &[&str], typ: &dyn Typed) -> rg::Type {
        let mut signed = None;
        let mut longness = 0;
        let original_specs: Vec<_> = typ.typespecs().collect();
        let mut specs = &original_specs[..];

        use ast::TypeSpecifier as TS;
        'process_prefixes: loop {
            match specs {
                [TS::Unsigned, rest @ ..] | [rest @ .., TS::Unsigned] => {
                    signed = Some(false);
                    specs = rest;
                }
                [TS::Signed, rest @ ..] | [rest @ .., TS::Signed] => {
                    signed = Some(true);
                    specs = rest;
                }
                [TS::Long, rest @ ..] | [rest @ .., TS::Long] => {
                    longness += 1;
                    specs = rest;
                }
                [TS::Short, rest @ ..] | [rest @ .., TS::Short] => {
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

        fn builtin(s: &str) -> rg::Type {
            rg::Type::Name(rg::Identifier::name(s))
        }

        fn pick_sign(signed: Option<bool>, uver: &str, sver: &str) -> rg::Type {
            let name = if signed.unwrap_or(true) { sver } else { uver };
            builtin(name)
        }

        assert_eq!(specs.len(), 1, "must have only one typespec remaining");

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
            TS::Float => builtin("f32"),
            TS::Double => match longness {
                1 => builtin("[u8; 12]"),
                _ => builtin("f64"),
            },
            TS::Char => pick_sign(signed, "u8", "i8"),
            TS::Bool => builtin("bool"),
            TS::Void => builtin("core::ffi::c_void"),
            TS::TypedefName(Node { node: id, .. }) => match id.name.as_ref() {
                "__int8" => pick_sign(signed, "u8", "i8"),
                "__int16" => pick_sign(signed, "u16", "i16"),
                "__int32" => pick_sign(signed, "u32", "i32"),
                "__int64" => pick_sign(signed, "u64", "i64"),
                name => rg::Type::Name(rg::Identifier::name(name)),
            },
            TS::Struct(Node { node: struty, .. }) => {
                let id = &struty
                    .identifier
                    .as_ref()
                    .map(|x| x.node.name.clone())
                    .unwrap_or_else(|| self.hash_name(stack, struty));

                rg::Type::Name(rg::Identifier::struct_name(id))
            }
            TS::Enum(Node { node: enumty, .. }) => {
                let id = &enumty
                    .identifier
                    .as_ref()
                    .map(|x| x.node.name.clone())
                    .unwrap_or_else(|| self.hash_name(stack, enumty));

                rg::Type::Name(rg::Identifier::enum_name(id))
            }
            _ => unimplemented!(
                "{:?}: don't know how to translate type: {:#?}",
                self.unit.path,
                typ
            ),
        };

        for _d in 0..typ.pointer_depth() {
            res = rg::Type::Pointer {
                konst: typ.is_const(),
                inner: Box::new(res),
            }
        }

        res
    }

    fn visit_declarator(
        &mut self,
        stack: &[&str],
        dtion: &ast::Declaration,
        dtor: &ast::Declarator,
    ) {
        // println!("declaration = {:#?}", dtion);
        // println!("declarator  = {:#?}", dtor);

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
            let typ = self.visit_type(&[], &DeclTuple { dtion, dtor });
            let ad = rg::AliasDeclaration {
                name: rg::Identifier::name(&id.name),
                typ,
            };
            self.push(ad);
        } else if let Some(fdecl) = dtor.get_function() {
            let fd = self.visit_fdecl(stack, dtion, id, dtor, fdecl);
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
        stack: &[&str],
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
                Some(self.visit_type(stack, &ftup))
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
                    typ: self.visit_type(stack, param),
                });
            }
        }

        res
    }

    fn hash_name<T>(&self, stack: &[&str], t: &T) -> String
    where
        T: Hash,
    {
        let mut h = DefaultHasher::new();
        t.hash(&mut h);

        let harsh = harsh::Harsh::default();
        let h = harsh.encode(&[h.finish()]);
        [stack.join("_"), "", h].join("_")
    }

    fn collect_opaque_structs(&mut self) {
        let mut opaque_structs: Vec<_> = self
            .forward_struct_names
            .difference(&self.declared_struct_names)
            .map(|name| rg::StructDeclaration {
                fields: Default::default(),
                name: rg::Identifier::name(name),
            })
            .collect();

        for s in opaque_structs.drain(..) {
            self.push(s);
        }
    }
}

/// Converts to a Rust constant expression
trait AsExpr {
    fn as_expr(&self, trans: &Translator) -> rg::Expr;
}

impl AsExpr for ast::Constant {
    fn as_expr(&self, _trans: &Translator) -> rg::Expr {
        rg::Expr::Constant(self.clone())
    }
}

impl AsExpr for ast::BinaryOperatorExpression {
    fn as_expr(&self, trans: &Translator) -> rg::Expr {
        rg::Expr::BinaryOperator(
            self.operator.node.clone(),
            Box::new(self.lhs.node.as_expr(trans)),
            Box::new(self.rhs.node.as_expr(trans)),
        )
    }
}

impl AsExpr for ast::CastExpression {
    fn as_expr(&self, trans: &Translator) -> rg::Expr {
        rg::Expr::Cast(
            trans.visit_type(&[], &self.type_name.node),
            Box::new(self.expression.node.as_expr(trans)),
        )
    }
}

impl AsExpr for ast::Identifier {
    fn as_expr(&self, _trans: &Translator) -> rg::Expr {
        rg::Expr::Identifier(self.name.clone())
    }
}

impl AsExpr for ast::Expression {
    fn as_expr(&self, trans: &Translator) -> rg::Expr {
        match self {
            ast::Expression::Identifier(v) => v.node.as_expr(trans),
            ast::Expression::Constant(v) => v.node.as_expr(trans),
            ast::Expression::StringLiteral(_) => todo!(),
            ast::Expression::GenericSelection(_) => todo!(),
            ast::Expression::Member(_) => todo!(),
            ast::Expression::Call(_) => todo!(),
            ast::Expression::CompoundLiteral(_) => todo!(),
            ast::Expression::SizeOf(ty) => rg::Expr::SizeOf(trans.visit_type(&[], &ty.node)),
            ast::Expression::AlignOf(ty) => rg::Expr::AlignOf(trans.visit_type(&[], &ty.node)),
            ast::Expression::UnaryOperator(_) => todo!(),
            ast::Expression::Cast(v) => v.node.as_expr(trans),
            ast::Expression::BinaryOperator(v) => v.node.as_expr(trans),
            ast::Expression::Conditional(_) => todo!(),
            ast::Expression::Comma(_) => todo!(),
            ast::Expression::OffsetOf(_) => todo!(),
            ast::Expression::VaArg(_) => todo!(),
            ast::Expression::Statement(_) => todo!(),
        }
    }
}

pub(crate) fn translate_unit(
    config: &Config,
    path: PathBuf,
    decls: &[ast::ExternalDeclaration],
) -> rg::Unit {
    let mut translator = Translator::new(config, path);
    translator.visit_unit(decls);
    translator.collect_opaque_structs();
    translator.unit
}

pub(crate) fn prelude() -> &'static str {
    use indoc::indoc;
    return indoc!(
        r#"
        // This file was automatically generated by cpr.
        // You probably don't want to edit it manually.

        #![allow(non_camel_case_types)]
        #![allow(non_upper_case_globals)]
        #![allow(non_snake_case)]
        "#
    );
}

#[cfg(test)]
mod test_translator;
