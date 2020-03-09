use lang_c::ast;
use lang_c::span::Node;
use std::io;

fn nodes<'a, T>(nodes: &'a [Node<T>]) -> impl Iterator<Item = &'a T> + 'a
where
    T: 'a,
{
    nodes.iter().map(borrow_node)
}

fn borrow_node<T>(node: &Node<T>) -> &T {
    &node.node
}

fn emit_type(
    w: &mut dyn io::Write,
    specifiers: &[Node<ast::DeclarationSpecifier>],
    derived: &[Node<ast::DerivedDeclarator>],
) -> io::Result<()> {
    let is_const = nodes(specifiers).any(|dspec| {
        if let ast::DeclarationSpecifier::TypeQualifier(Node {
            node: ast::TypeQualifier::Const,
            ..
        }) = dspec
        {
            return true;
        }
        false
    });
    for derived in nodes(derived) {
        match derived {
            ast::DerivedDeclarator::Pointer(qualifiers) => {
                if is_const {
                    write!(w, "const *")?;
                } else {
                    write!(w, "mut *")?;
                }
            }
            _ => {}
        }
    }

    for dspec in nodes(specifiers) {
        match dspec {
            ast::DeclarationSpecifier::TypeSpecifier(Node { node: ts, .. }) => {
                emit_typespec(w, ts)?;
            }
            _ => {}
        }
    }

    Ok(())
}

fn emit_typespec(w: &mut dyn io::Write, ts: &ast::TypeSpecifier) -> io::Result<()> {
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

pub fn get_identifier(declarator: &ast::Declarator) -> Option<&ast::Identifier> {
    if let ast::DeclaratorKind::Identifier(Node { node: id, .. }) = &declarator.kind.node {
        Some(id)
    } else {
        None
    }
}

pub fn emit_fdecl(
    w: &mut dyn io::Write,
    dtion: &ast::Declaration,
    id: &ast::Identifier,
    dtor: &ast::Declarator,
    fdecl: &ast::FunctionDeclarator,
) -> io::Result<()> {
    writeln!(w, "extern {c:?} {{", c = "C")?;
    write!(w, "    fn {name}(", name = id.name)?;

    for (i, param) in nodes(&fdecl.parameters[..]).enumerate() {
        if i > 0 {
            write!(w, ", ")?;
        }

        let declarator = param.declarator.as_ref().map(borrow_node);
        let name = declarator
            .and_then(get_identifier)
            .map(|id| id.name.clone())
            .unwrap_or_else(|| format!("__arg{}", i));
        write!(w, "{}: ", name)?;

        let derived = declarator.map(|dtor| &dtor.derived[..]).unwrap_or_default();
        emit_type(w, &param.specifiers[..], derived)?;
    }

    write!(w, ") -> ")?;
    emit_type(w, &dtion.specifiers[..], &dtor.derived[..])?;
    writeln!(w, ";")?;
    writeln!(w, "}} // extern {c:?}", c = "C")?;

    Ok(())
}

pub fn emit_declarator(
    w: &mut dyn io::Write,
    dtion: &ast::Declaration,
    dtor: &ast::Declarator,
) -> io::Result<()> {
    let id = match get_identifier(dtor) {
        None => return Ok(()),
        Some(x) => x,
    };

    if let Some(ast::StorageClassSpecifier::Typedef) = get_storage_class(dtion) {
        // typedef
        write!(w, "type {} = ", id.name)?;
        emit_type(w, &dtion.specifiers, &dtor.derived)?;
    } else if let Some(function_declarator) = get_function(dtor) {
        // non-typedef
        emit_fdecl(w, dtion, id, dtor, function_declarator)?;
    }

    Ok(())
}

pub fn emit_unit(w: &mut dyn io::Write, unit: &ast::TranslationUnit) -> io::Result<()> {
    for extdecl in nodes(&unit.0) {
        if let ast::ExternalDeclaration::Declaration(Node {
            node: declaration, ..
        }) = &extdecl
        {
            for init_declarator in nodes(&declaration.declarators) {
                let declarator = &init_declarator.declarator.node;
                emit_declarator(w, declaration, declarator)?;
                writeln!(w, ";")?;
            }
        }
    }

    Ok(())
}
