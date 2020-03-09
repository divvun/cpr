use lang_c::ast;
use lang_c::span::Node;
use std::io::{self, Write};

mod utils;
use utils::*;

struct Writer<'a> {
    w: &'a mut dyn Write,
}

impl<'a> io::Write for Writer<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.w.write(buf)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.w.flush()
    }
}

impl<'a> Writer<'a> {
    fn emit_unit(&mut self, unit: &ast::TranslationUnit) -> io::Result<()> {
        for extdecl in nodes(&unit.0) {
            if let ast::ExternalDeclaration::Declaration(Node {
                node: declaration, ..
            }) = &extdecl
            {
                if declaration.declarators.is_empty() {
                    for spec in nodes(&declaration.specifiers[..]) {
                        self.emit_freestanding_specifier(spec)?;
                    }
                } else {
                    for init_declarator in nodes(&declaration.declarators[..]) {
                        let declarator = &init_declarator.declarator.node;
                        self.emit_declarator(declaration, declarator)?;
                        writeln!(self, ";")?;
                    }
                }
            } else {
                log::debug!("emit_unit: not a Declaration: {:#?}", extdecl);
            }
        }

        Ok(())
    }

    fn emit_freestanding_specifier(&mut self, spec: &ast::DeclarationSpecifier) -> io::Result<()> {
        if let ast::DeclarationSpecifier::TypeSpecifier(Node { node: tyspec, .. }) = spec {
            match tyspec {
                ast::TypeSpecifier::Struct(Node { node: struty, .. }) => {
                    self.emit_struct(struty)?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn emit_struct(&mut self, struty: &ast::StructType) -> io::Result<()> {
        let id = match struty.identifier.as_ref().map(borrow_node) {
            Some(x) => x,
            None => return Ok(()),
        };

        writeln!(self, "pub struct {} {{", id.name)?;
        if let Some(declarations) = &struty.declarations {
            for dtion in nodes(&declarations[..]) {
                match dtion {
                    ast::StructDeclaration::Field(Node { node: field, .. }) => {
                        let specifiers = &field.specifiers[..];

                        for dtor in nodes(&field.declarators[..]) {
                            if let Some(Node { node: dtor, .. }) = dtor.declarator.as_ref() {
                                log::debug!("{:?} {:?}", specifiers, dtor);

                                let id = match get_identifier(dtor) {
                                    Some(x) => x,
                                    None => continue,
                                };
                                write!(self, "{name}: ", name = id.name)?;
                                self.emit_type(specifiers, &dtor.derived[..])?;
                                writeln!(self, ";")?;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        writeln!(self, "}}")?;

        Ok(())
    }

    fn emit_declarator(
        &mut self,
        dtion: &ast::Declaration,
        dtor: &ast::Declarator,
    ) -> io::Result<()> {
        let id = match get_identifier(dtor) {
            None => {
                log::debug!(
                    "emit_declarator: dtor without identifier {:#?} {:#?}",
                    dtion,
                    dtor
                );
                return Ok(());
            }
            Some(x) => x,
        };

        log::debug!("emit_declarator: {}", id.name);

        if let Some(ast::StorageClassSpecifier::Typedef) = get_storage_class(dtion) {
            // typedef
            write!(self, "type {} = ", id.name)?;
            self.emit_type(&dtion.specifiers, &dtor.derived)?;
        } else if let Some(function_declarator) = get_function(dtor) {
            // non-typedef
            self.emit_fdecl(dtion, id, dtor, function_declarator)?;
        } else {
            log::debug!(
                "emit_declarator: unsure what to do with {:#?} {:#?}",
                dtion,
                dtor
            );
        }

        Ok(())
    }

    fn emit_fdecl(
        &mut self,
        dtion: &ast::Declaration,
        id: &ast::Identifier,
        dtor: &ast::Declarator,
        fdecl: &ast::FunctionDeclarator,
    ) -> io::Result<()> {
        writeln!(self, "extern {c:?} {{", c = "C")?;
        write!(self, "    fn {name}(", name = id.name)?;

        for (i, param) in nodes(&fdecl.parameters[..]).enumerate() {
            if i > 0 {
                write!(self, ", ")?;
            }

            let declarator = param.declarator.as_ref().map(borrow_node);
            let name = declarator
                .and_then(get_identifier)
                .map(|id| id.name.clone())
                .unwrap_or_else(|| format!("__arg{}", i));
            write!(self, "{}: ", name)?;

            let derived = declarator.map(|dtor| &dtor.derived[..]).unwrap_or_default();
            self.emit_type(&param.specifiers[..], derived)?;
        }

        write!(self, ") -> ")?;
        self.emit_type(&dtion.specifiers[..], &dtor.derived[..])?;
        writeln!(self, ";")?;
        writeln!(self, "}} // extern {c:?}", c = "C")?;

        Ok(())
    }

    fn emit_typespec(&mut self, ts: &ast::TypeSpecifier) -> io::Result<()> {
        match ts {
            ast::TypeSpecifier::Int => write!(self, "i32"),
            ast::TypeSpecifier::Short => write!(self, "i16"),
            ast::TypeSpecifier::Char => write!(self, "i8"),
            ast::TypeSpecifier::Void => write!(self, "()"),
            ast::TypeSpecifier::TypedefName(Node { node: id, .. }) => write!(self, "{}", id.name),
            ast::TypeSpecifier::Struct(Node { node: struty, .. }) => {
                let id = &struty
                    .identifier
                    .as_ref()
                    .expect("anonymous structs are not suported")
                    .node;
                // struty.
                write!(self, "struct_{}", id.name)?;
                Ok(())
            }
            _ => unimplemented!(),
        }
    }

    fn emit_type<SQ>(
        &mut self,
        specifiers: &[Node<SQ>],
        derived: &[Node<ast::DerivedDeclarator>],
    ) -> io::Result<()>
    where
        SQ: AsSpecifierQualifier,
    {
        let specifiers = specifiers
            .iter()
            .map(borrow_node)
            .filter_map(AsSpecifierQualifier::as_specqual)
            .collect::<Vec<ast::SpecifierQualifier>>();

        let is_const = specifiers.iter().any(IsConst::is_const);
        for derived in nodes(derived) {
            match derived {
                ast::DerivedDeclarator::Pointer(_qualifiers) => {
                    if is_const {
                        write!(self, "*const ")?;
                    } else {
                        write!(self, "*mut ")?;
                    }
                }
                _ => {}
            }
        }

        for spec in specifiers {
            match spec {
                ast::SpecifierQualifier::TypeSpecifier(Node { node: ts, .. }) => {
                    self.emit_typespec(&ts)?;
                }
                _ => {}
            }
        }

        Ok(())
    }
}

pub fn emit_unit(w: &mut dyn io::Write, unit: &ast::TranslationUnit) -> io::Result<()> {
    let mut w = Writer { w };
    w.emit_unit(unit)
}
