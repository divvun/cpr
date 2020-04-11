use super::*;
use crate::frontend::{grammar::Include, Context, Parser, SourceProvider};
use indoc::indoc;
use lang_c::env::Env;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

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

#[test]
fn test_simple_typedefs() {
    let mut provider = TestSourceProvider::new();
    provider.files.insert(
        "root.h".into(),
        indoc!(
            "
            typedef short SHORT;
            "
        )
        .into(),
    );

    let ctx = Context::new();
    let env = Env::with_msvc();
    let mut parser = Parser::new(Box::new(provider), ctx, env);
    parser.parse_path("root.h".into()).unwrap();

    let unit = parser.units.values().next().unwrap();
    let config = Config { arch: Arch::X86_64 };
    let unit = translate_unit(&config, unit.path.clone(), &unit.declarations[..]);
    assert_eq!(1, unit.toplevels.len());
    assert!(matches!(
        &unit.toplevels[0],
        rg::TopLevel::AliasDeclaration(rg::AliasDeclaration {
            name: rg::Identifier {
                value: typedef_name
            },
            typ: rg::Type::Name(rg::Identifier { value: type_name }),
        }) if typedef_name == "SHORT" && type_name == "i16"
    ));
}
