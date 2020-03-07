use lang_c::ast;
use std::io;

fn write_type(w: &mut dyn io::Write, ts: &ast::TypeSpecifier) -> io::Result<()> {
    match ts {
        ast::TypeSpecifier::Int => write!(w, "i32"),
        ast::TypeSpecifier::Short => write!(w, "i16"),
        ast::TypeSpecifier::Char => write!(w, "i8"),
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

pub fn walk_ast(w: &mut dyn io::Write, unit: &ast::TranslationUnit) -> io::Result<()> {
    for decl in &unit.0 {
        match &decl.node {
            ast::ExternalDeclaration::Declaration(ref decl) => {
                write!(w, "\n")?;
                write!(w, "// decl")?;

                println!("\n=== found decl {:#?}", decl.node);

                for tor in &decl.node.declarators {
                    let tor = &tor.node.declarator;
                    match &tor.node.kind.node {
                        ast::DeclaratorKind::Identifier(id) => {
                            println!("Identifier {:?}", id.node.name);

                            match get_storage_class(&decl.node) {
                                Some(ast::StorageClassSpecifier::Typedef) => {
                                    writeln!(w)?;
                                    write!(w, "type {} = ", id.node.name)?;
                                }
                                _ => {}
                            }

                            for der in &tor.node.derived {
                                match der.node {
                                    ast::DerivedDeclarator::Pointer(_) => {
                                        write!(w, "*mut ")?;
                                    }
                                    _ => {}
                                }
                                println!("derived {:?}", der.node)
                            }

                            for spec in &decl.node.specifiers {
                                match &spec.node {
                                    ast::DeclarationSpecifier::TypeSpecifier(ts) => {
                                        write_type(w, &ts.node)?;
                                        writeln!(w, ";")?;
                                    }
                                    _ => {}
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
