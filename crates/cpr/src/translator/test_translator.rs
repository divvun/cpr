use super::*;
use crate::frontend::{grammar::Include, Context, Parser, SourceProvider};
use indoc::indoc;
use lang_c::env::Env;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

trait UnitExtension {
    fn must_have_alias(&self, name: &str, f: &dyn Fn(&rg::AliasDeclaration));
}

impl UnitExtension for rg::Unit {
    fn must_have_alias(&self, name: &str, f: &dyn Fn(&rg::AliasDeclaration)) {
        let d = self
            .toplevels
            .iter()
            .filter_map(|tl| {
                match tl {
                    rg::TopLevel::AliasDeclaration(ad) => {
                        if ad.name.value == name {
                            return Some(ad);
                        }
                    }
                    _ => {}
                };
                None
            })
            .next()
            .unwrap_or_else(|| panic!("should have an alias with name {:?}", name));
        f(d);
    }
}

trait TypeExtension {
    fn must_be(&self, pattern: &str);
}

impl TypeExtension for rg::Type {
    fn must_be(&self, pattern: &str) {
        assert_eq!(pattern, format!("{}", self), "(expected is on the left)")
    }
}

struct TestSourceProvider {
    files: HashMap<PathBuf, String>,
}

impl TestSourceProvider {
    fn new() -> Self {
        Self {
            files: Default::default(),
        }
    }
}

impl SourceProvider for TestSourceProvider {
    fn resolve(&self, include: &Include) -> Option<(PathBuf, String)> {
        let path: &Path = include.as_ref();
        self.files
            .get(path)
            .map(|s| (path.to_path_buf(), s.clone()))
    }
}

fn parse_single_unit(input: &str) -> rg::Unit {
    let mut provider = TestSourceProvider::new();
    provider.files.insert("root.h".into(), input.into());

    let ctx = Context::new();
    let env = Env::with_msvc();
    let mut parser = Parser::new(Box::new(provider), ctx, env);
    parser.parse_path("root.h".into()).unwrap();

    let unit = parser.units.values().next().unwrap();
    let config = Config { arch: Arch::X86_64 };
    translate_unit(&config, unit.path.clone(), &unit.declarations[..])
}

#[test]
fn simple_typedefs() {
    let unit = parse_single_unit(indoc!(
        "
        typedef short SHORT;
        typedef short int SHORT_INT;
        typedef int INT;
        typedef long LONG;
        typedef long int LONG_INT;
        typedef long long LONG_LONG;
        typedef long long int LONG_LONG_INT;
         
        typedef unsigned short USHORT;
        typedef unsigned short int USHORT_INT;
        typedef unsigned int UINT;
        typedef unsigned long ULONG;
        typedef unsigned long int ULONG_INT;
        typedef unsigned long long ULONG_LONG;
        typedef unsigned long long int ULONG_LONG_INT;
         
        typedef signed short SSHORT;
        typedef signed short int SSHORT_INT;
        typedef signed int SINT;
        typedef signed long SLONG;
        typedef signed long int SLONG_INT;
        typedef signed long long SLONG_LONG;
        typedef signed long long int SLONG_LONG_INT;
        "
    ));

    for n in &["SHORT", "SHORT_INT", "SSHORT", "SSHORT_INT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("i16"));
    }
    for n in &["USHORT", "USHORT_INT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("u16"));
    }
    for n in &["INT", "SINT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("i32"));
    }
    for n in &["UINT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("u32"));
    }
    for n in &[
        "LONG",
        "SLONG",
        "LONG_INT",
        "SLONG_INT",
        "LONG_LONG",
        "SLONG_LONG",
        "LONG_LONG_INT",
        "SLONG_LONG_INT",
    ] {
        unit.must_have_alias(n, &|d| d.typ.must_be("i64"));
    }
    for n in &["ULONG", "ULONG_INT", "ULONG_LONG", "ULONG_LONG_INT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("u64"));
    }
}

#[test]
fn multiple_typedefs() {
    let unit = parse_single_unit(indoc!(
        "
        typedef unsigned int UINT, *LPUINT, *const LCPUINT;
        "
    ));
    unit.must_have_alias("UINT", &|d| d.typ.must_be("u32"));
    unit.must_have_alias("LPUINT", &|d| d.typ.must_be("*mut u32"));
    unit.must_have_alias("LCPUINT", &|d| d.typ.must_be("*const u32"));
}
