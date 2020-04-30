use std::{
    cell::{RefCell, RefMut},
    collections::{HashMap, HashSet},
};

use crate::ast::*;
use crate::span::Node;
use crate::strings;

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum Symbol {
    Typename,
    Identifier,
}

pub struct ParserEnv<'a> {
    inner: RefCell<&'a mut Env>,
}

impl<'a> ParserEnv<'a> {
    pub fn get(&self) -> RefMut<&'a mut Env> {
        self.inner.borrow_mut()
    }
}

pub struct Env {
    symbols: Vec<HashMap<String, Symbol>>,
    pub builtin_typenames: HashSet<String>,
    pub extensions_gnu: bool,
    pub extensions_clang: bool,
    pub extensions_msvc: bool,
    pub reserved: HashSet<&'static str>,
    pub is_ignoring_reserved: bool,
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
            symbols: vec![HashMap::default()],
            extensions_gnu: false,
            extensions_clang: false,
            extensions_msvc: false,
            builtin_typenames: HashSet::new(),
            reserved,
            is_ignoring_reserved: false,
        }
    }

    pub fn with_gnu() -> Env {
        let mut builtin_typenames = HashSet::default();
        builtin_typenames.insert("__builtin_va_list".to_owned());
        let mut reserved = HashSet::default();
        reserved.extend(strings::RESERVED_C11.iter());
        reserved.extend(strings::RESERVED_GNU.iter());
        Env {
            symbols: vec![HashMap::default()],
            extensions_gnu: true,
            extensions_clang: false,
            extensions_msvc: false,
            builtin_typenames,
            reserved,
            is_ignoring_reserved: false,
        }
    }

    pub fn with_clang() -> Env {
        let mut builtin_typenames = HashSet::default();
        builtin_typenames.insert("__builtin_va_list".to_owned());
        let mut reserved = HashSet::default();
        reserved.extend(strings::RESERVED_C11.iter());
        reserved.extend(strings::RESERVED_GNU.iter());
        reserved.extend(strings::RESERVED_CLANG.iter());
        Env {
            symbols: vec![HashMap::default()],
            extensions_gnu: true,
            extensions_clang: true,
            extensions_msvc: false,
            builtin_typenames,
            reserved,
            is_ignoring_reserved: false,
        }
    }

    pub fn with_msvc() -> Env {
        let mut builtin_typenames = HashSet::default();
        builtin_typenames.insert("__int8".to_owned());
        builtin_typenames.insert("__int16".to_owned());
        builtin_typenames.insert("__int32".to_owned());
        builtin_typenames.insert("__int64".to_owned());
        let mut reserved = HashSet::default();
        reserved.extend(strings::RESERVED_C11.iter());
        Env {
            symbols: vec![HashMap::default()],
            extensions_gnu: false,
            extensions_clang: false,
            extensions_msvc: true,
            builtin_typenames,
            reserved,
            is_ignoring_reserved: false,
        }
    }

    pub fn enter_scope(&mut self) {
        self.symbols.push(HashMap::new());
    }

    pub fn leave_scope(&mut self) {
        self.symbols.pop().expect("more scope pops than pushes");
    }

    pub fn ignore_reserved(&mut self, ignore: bool) {
        self.is_ignoring_reserved = ignore;
    }

    pub fn is_typename(&self, s: &str) -> bool {
        self.builtin_typenames.contains(s)
            || self.symbols.iter().any(|sc| {
                sc.get(s)
                    .map(|s| matches!(s, Symbol::Typename))
                    .unwrap_or(false)
            })
    }

    pub fn handle_declarator(&mut self, d: &Node<Declarator>, sym: Symbol) {
        if let Some(name) = find_declarator_name(&d.node.kind.node) {
            self.add_symbol(name, sym);
        }
    }

    pub fn add_symbol(&mut self, s: &str, symbol: Symbol) {
        let scope = self
            .symbols
            .last_mut()
            .expect("at least one scope should be always present");
        scope.insert(s.to_string(), symbol);
    }

    pub fn for_parser<'a>(&'a mut self) -> ParserEnv<'a> {
        ParserEnv {
            inner: RefCell::new(self),
        }
    }
}

fn find_declarator_name(d: &DeclaratorKind) -> Option<&str> {
    match d {
        &DeclaratorKind::Abstract => None,
        &DeclaratorKind::Identifier(ref i) => Some(&i.node.name),
        &DeclaratorKind::Declarator(ref d) => find_declarator_name(&d.node.kind.node),
    }
}
