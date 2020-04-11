use super::*;
use crate::frontend::{grammar::Include, Context, Parser, SourceProvider};
use indoc::indoc;
use lang_c::env::Env;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

trait UnitExtension {
    fn must_have_alias(&self, name: &str) -> &rg::AliasDeclaration;
}

impl UnitExtension for rg::Unit {
    fn must_have_alias(&self, name: &str) -> &rg::AliasDeclaration {
        self.toplevels
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
            .unwrap_or_else(|| panic!("should have an alias with name {:?}", name))
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
fn test_simple_typedefs() {
    let unit = parse_single_unit(indoc!(
        "
        typedef short SHORT;
        "
    ));
    unit.must_have_alias("SHORT").typ.must_be("i16");

    let unit = parse_single_unit(indoc!(
        "
        typedef unsigned int UINT, *PUINT;
        "
    ));
    unit.must_have_alias("UINT").typ.must_be("u32");
    unit.must_have_alias("PUINT").typ.must_be("*mut u32");
}
