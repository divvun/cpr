use lang_c::ast;
use lang_c::span::Node;
use std::io;

fn write_specifiers(
    w: &mut dyn io::Write,
    specifiers: &[Node<ast::DeclarationSpecifier>],
) -> io::Result<()> {
    for specifier_node in specifiers {
        let specifier = &specifier_node.node;
        match specifier {
            ast::DeclarationSpecifier::TypeSpecifier(ts) => {
                write_type(w, &ts.node)?;
            }
            _ => {}
        }
    }

    Ok(())
}

fn write_type(w: &mut dyn io::Write, ts: &ast::TypeSpecifier) -> io::Result<()> {
    match ts {
        ast::TypeSpecifier::Int => write!(w, "i32"),
        ast::TypeSpecifier::Short => write!(w, "i16"),
        ast::TypeSpecifier::Char => write!(w, "i8"),
        ast::TypeSpecifier::Void => write!(w, "()"),
        ast::TypeSpecifier::TypedefName(id) => write!(w, "{}", id.node.name),
        _ => unimplemented!(),
    }
}

fn get_storage_class(d: &ast::Declaration) -> Option<&ast::StorageClassSpecifier> {
    for s in &d.specifiers {
        match &s.node {
            ast::DeclarationSpecifier::StorageClass(sc) => return Some(&sc.node),
            _ => {}
        }
    }
    None
}

fn get_function(declarator: &ast::Declarator) -> Option<&ast::FunctionDeclarator> {
    for derived_node in &declarator.derived {
        let derived = &derived_node.node;
        match derived {
            ast::DerivedDeclarator::Function(fd) => return Some(&fd.node),
            _ => {}
        }
    }

    None
}

pub fn walk_ast(w: &mut dyn io::Write, unit: &ast::TranslationUnit) -> io::Result<()> {
    for decl in &unit.0 {
        match &decl.node {
            ast::ExternalDeclaration::Declaration(ref declaration_node) => {
                let declaration = &declaration_node.node;
                for init_declarator_node in &declaration.declarators {
                    let declarator = &init_declarator_node.node.declarator.node;
                    match &declarator.kind.node {
                        ast::DeclaratorKind::Identifier(id) => {
                            println!("Identifier {:?}", id.node.name);

                            match get_storage_class(&declaration) {
                                Some(ast::StorageClassSpecifier::Typedef) => {
                                    write!(w, "type {} = ", id.node.name)?;

                                    for der in &declarator.derived {
                                        println!("derived {:?}", der.node);
                                        match der.node {
                                            ast::DerivedDeclarator::Pointer(_) => {
                                                write!(w, "*mut ")?;
                                            }
                                            _ => {}
                                        }
                                    }

                                    write_specifiers(w, &declaration.specifiers)?;
                                    writeln!(w, ";")?;
                                }
                                _ => {
                                    if let Some(function_declarator) = get_function(declarator) {
                                        // writeln!(w, "/* Function {:#?} */", function_declarator)?;
                                        writeln!(w, "extern {c:?} {{", c = "C")?;
                                        write!(w, "    fn {name}(", name = id.node.name)?;

                                        for (i, parameter_declaration_node) in
                                            function_declarator.parameters.iter().enumerate()
                                        {
                                            let parameter_declaration =
                                                &parameter_declaration_node.node;

                                            if i > 0 {
                                                write!(w, ", ")?;
                                            }

                                            write_specifiers(w, &parameter_declaration.specifiers)?;
                                        }

                                        write!(w, ") -> ")?;
                                        write_specifiers(w, &declaration.specifiers)?;
                                        writeln!(w, ";")?;
                                        writeln!(w, "}} // extern {c:?}", c = "C")?;
                                    } else {
                                        writeln!(
                                            w,
                                            "/* Unknown declarator = {:#?} */",
                                            declarator
                                        )?;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    Ok(())
}
