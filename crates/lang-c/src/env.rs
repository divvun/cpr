use std::collections::HashSet;

use ast::*;
use span::Node;
use strings;

pub struct Env {
    pub typenames: Vec<HashSet<String>>,
    pub extensions_gnu: bool,
    pub extensions_clang: bool,
    pub extensions_msvc: bool,
    pub reserved: HashSet<&'static str>,
    pub(crate) is_ignoring_reserved: bool,
    pub(crate) is_single_line_mode: bool,
}

impl Env {
    #[cfg(test)]
    pub fn new() -> Env {
        Env::with_gnu()
    }

    pub fn with_core() -> Env {
        let mut reserved = HashSet::default();
        reserved.extend(strings::RESERVED_C11.iter());
        Env {
            extensions_gnu: false,
            extensions_clang: false,
            extensions_msvc: false,
            typenames: Vec::new(),
            reserved: reserved,
            is_ignoring_reserved: false,
            is_single_line_mode: false,
        }
    }

    pub fn with_gnu() -> Env {
        let mut typenames = HashSet::default();
        let mut reserved = HashSet::default();
        typenames.insert("__builtin_va_list".to_owned());
        reserved.extend(strings::RESERVED_C11.iter());
        reserved.extend(strings::RESERVED_GNU.iter());
        Env {
            extensions_gnu: true,
            extensions_clang: false,
            extensions_msvc: false,
            typenames: vec![typenames],
            reserved: reserved,
            is_ignoring_reserved: false,
            is_single_line_mode: false,
        }
    }

    pub fn with_clang() -> Env {
        let mut typenames = HashSet::default();
        let mut reserved = HashSet::default();
        typenames.insert("__builtin_va_list".to_owned());
        reserved.extend(strings::RESERVED_C11.iter());
        reserved.extend(strings::RESERVED_GNU.iter());
        reserved.extend(strings::RESERVED_CLANG.iter());
        Env {
            extensions_gnu: true,
            extensions_clang: true,
            extensions_msvc: false,
            typenames: vec![typenames],
            reserved: reserved,
            is_ignoring_reserved: false,
            is_single_line_mode: false,
        }
    }

    pub fn with_msvc() -> Env {
        let mut typenames = HashSet::default();
        let mut reserved = HashSet::default();
        typenames.insert("__int8".to_owned());
        typenames.insert("__int16".to_owned());
        typenames.insert("__int32".to_owned());
        typenames.insert("__int64".to_owned());
        reserved.extend(strings::RESERVED_C11.iter());
        Env {
            extensions_gnu: false,
            extensions_clang: false,
            extensions_msvc: true,
            typenames: vec![typenames],
            reserved: reserved,
            is_ignoring_reserved: false,
            is_single_line_mode: false,
        }
    }

    pub fn enter_scope(&mut self) {
        self.typenames.push(HashSet::new());
    }

    pub fn leave_scope(&mut self) {
        self.typenames.pop().expect("more scope pops than pushes");
    }

    pub fn ignore_reserved(&mut self, ignore: bool) {
        self.is_ignoring_reserved = ignore;
    }

    pub fn single_line_mode(&mut self, enable: bool) {
        self.is_single_line_mode = enable;
    }

    pub fn add_typename(&mut self, s: &str) {
        let scope = self
            .typenames
            .last_mut()
            .expect("at least one scope should be always present");
        scope.insert(s.to_string());
    }

    pub fn is_typename(&self, s: &str) -> bool {
        self.typenames.iter().any(|sc| sc.contains(s))
    }

    pub fn handle_declaration(&mut self, declaration: &Declaration) {
        if declaration.specifiers.iter().any(is_typedef) {
            for init_decl in &declaration.declarators {
                if let Some(name) = find_declarator_name(&init_decl.node.declarator.node.kind.node)
                {
                    self.add_typename(name);
                }
            }
        }
    }
}

fn is_typedef(ds: &Node<DeclarationSpecifier>) -> bool {
    match &ds.node {
        &DeclarationSpecifier::StorageClass(Node {
            node: StorageClassSpecifier::Typedef,
            ..
        }) => true,
        _ => false,
    }
}

fn find_declarator_name(d: &DeclaratorKind) -> Option<&str> {
    match d {
        &DeclaratorKind::Abstract => None,
        &DeclaratorKind::Identifier(ref i) => Some(&i.node.name),
        &DeclaratorKind::Declarator(ref d) => find_declarator_name(&d.node.kind.node),
    }
}
