use lang_c::ast;
use lang_c::span::Node;

pub(crate) fn nodes<'a, T>(nodes: &'a [Node<T>]) -> impl Iterator<Item = &'a T> + 'a
where
    T: 'a,
{
    nodes.iter().map(borrow_node)
}

pub(crate) fn borrow_node<T>(node: &Node<T>) -> &T {
    &node.node
}

pub(crate) trait AsSpecifierQualifier {
    fn as_specqual(&self) -> Option<ast::SpecifierQualifier>;
}

impl AsSpecifierQualifier for ast::SpecifierQualifier {
    fn as_specqual(&self) -> Option<ast::SpecifierQualifier> {
        Some(self.clone())
    }
}

impl AsSpecifierQualifier for ast::DeclarationSpecifier {
    fn as_specqual(&self) -> Option<ast::SpecifierQualifier> {
        match self {
            Self::TypeSpecifier(ts) => Some(ast::SpecifierQualifier::TypeSpecifier(ts.clone())),
            Self::TypeQualifier(tq) => Some(ast::SpecifierQualifier::TypeQualifier(tq.clone())),
            _ => None,
        }
    }
}

pub(crate) trait IsConst {
    fn is_const(&self) -> bool;
}

impl IsConst for ast::SpecifierQualifier {
    fn is_const(&self) -> bool {
        if let Self::TypeQualifier(Node {
            node: ast::TypeQualifier::Const,
            ..
        }) = self
        {
            return true;
        }
        false
    }
}

pub(crate) fn get_storage_class(d: &ast::Declaration) -> Option<&ast::StorageClassSpecifier> {
    for s in &d.specifiers {
        match &s.node {
            ast::DeclarationSpecifier::StorageClass(sc) => return Some(&sc.node),
            _ => {}
        }
    }
    None
}

pub(crate) fn get_function(declarator: &ast::Declarator) -> Option<&ast::FunctionDeclarator> {
    for derived_node in &declarator.derived {
        let derived = &derived_node.node;
        match derived {
            ast::DerivedDeclarator::Function(fd) => return Some(&fd.node),
            _ => {}
        }
    }

    None
}

pub(crate) fn get_identifier(declarator: &ast::Declarator) -> Option<&ast::Identifier> {
    if let ast::DeclaratorKind::Identifier(Node { node: id, .. }) = &declarator.kind.node {
        Some(id)
    } else {
        None
    }
}
