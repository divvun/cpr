use super::*;
use crate::frontend::{
    grammar::Include, Context, Error, FileInfo, FilePath, IdGenerator, Parser, SourceDir,
    SourceProvider,
};
use indoc::indoc;
use lang_c::{ast, env::Env};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

trait UnitExtension {
    fn must_have_alias<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::AliasDeclaration));
    fn must_have_enum<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::EnumDeclaration));
    fn must_have_struct<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::StructDeclaration));
    fn must_have_function<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::FunctionDeclaration));
    fn must_have_constant<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::Constant));

    fn must_have_alias_count(&self, count: usize);
    fn must_have_struct_count(&self, count: usize);
    fn must_have_enum_count(&self, count: usize);
}

impl UnitExtension for rg::Unit {
    fn must_have_alias<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::AliasDeclaration)) {
        let name = name.as_ref();
        let d = self
            .toplevels
            .iter()
            .filter_map(|tl| {
                match tl {
                    rg::TopLevel::AliasDeclaration(d) => {
                        if d.name.value == name {
                            return Some(d);
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

    fn must_have_enum<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::EnumDeclaration)) {
        let name = name.as_ref();
        let d = self
            .toplevels
            .iter()
            .filter_map(|tl| {
                match tl {
                    rg::TopLevel::EnumDeclaration(d) => {
                        if d.name.value == name {
                            return Some(d);
                        }
                    }
                    _ => {}
                };
                None
            })
            .next()
            .unwrap_or_else(|| panic!("should have an enum with name {:?}", name));
        f(d);
    }

    fn must_have_struct<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::StructDeclaration)) {
        let name = name.as_ref();
        let d = self
            .toplevels
            .iter()
            .filter_map(|tl| {
                match tl {
                    rg::TopLevel::StructDeclaration(d) => {
                        if d.name.value == name {
                            return Some(d);
                        }
                    }
                    _ => {}
                };
                None
            })
            .next()
            .unwrap_or_else(|| panic!("should have a struct with name {:?}", name));
        f(d);
    }

    fn must_have_function<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::FunctionDeclaration)) {
        let name = name.as_ref();
        let d = self
            .toplevels
            .iter()
            .filter_map(|tl| {
                match tl {
                    rg::TopLevel::FunctionDeclaration(d) => {
                        if d.name.value == name {
                            return Some(d);
                        }
                    }
                    _ => {}
                };
                None
            })
            .next()
            .unwrap_or_else(|| panic!("should have a function with name {:?}", name));
        f(d);
    }

    fn must_have_constant<N: AsRef<str>>(&self, name: N, f: &dyn Fn(&rg::Constant)) {
        let name = name.as_ref();
        let d = self
            .toplevels
            .iter()
            .filter_map(|tl| {
                match tl {
                    rg::TopLevel::Constant(d) => {
                        if d.name.value == name {
                            return Some(d);
                        }
                    }
                    _ => {}
                };
                None
            })
            .next()
            .unwrap_or_else(|| panic!("should have a constant with name {:?}", name));
        f(d);
    }

    fn must_have_alias_count(&self, count: usize) {
        let actual = self
            .toplevels
            .iter()
            .filter(|tl| matches!(tl, rg::TopLevel::AliasDeclaration(_)))
            .count();
        assert_eq!(count, actual, "(expected is on the left)");
    }

    fn must_have_struct_count(&self, count: usize) {
        let actual = self
            .toplevels
            .iter()
            .filter(|tl| matches!(tl, rg::TopLevel::StructDeclaration(_)))
            .count();
        assert_eq!(count, actual, "(expected is on the left)");
    }

    fn must_have_enum_count(&self, count: usize) {
        let actual = self
            .toplevels
            .iter()
            .filter(|tl| matches!(tl, rg::TopLevel::EnumDeclaration(_)))
            .count();
        assert_eq!(count, actual, "(expected is on the left)");
    }
}

trait ConstantExtension {
    fn must_be(&self, val: &str);
}

impl ConstantExtension for rg::Constant {
    fn must_be(&self, val: &str) {
        assert_eq!(
            val, self.value,
            "(checking constant value, expected is on the left)"
        );
    }
}

trait EnumExtension {
    fn must_have_field(&self, name: &str, f: &dyn Fn(&rg::EnumField));
}

impl EnumExtension for rg::EnumDeclaration {
    fn must_have_field(&self, name: &str, f: &dyn Fn(&rg::EnumField)) {
        let field = self
            .fields
            .iter()
            .find(|f| f.name.value == name)
            .unwrap_or_else(|| {
                panic!(
                    "enum {:?} should have field with name {:?}",
                    self.name.value, name
                )
            });
        f(field);
    }
}

trait EnumFieldExtension {
    fn must_be_none(&self);
    fn must_be_some(&self, f: &dyn Fn(&rg::Expr));
}

impl EnumFieldExtension for rg::EnumField {
    fn must_be_none(&self) {
        assert!(
            self.value.is_none(),
            "field {:?} should be None",
            self.name.value
        )
    }
    fn must_be_some(&self, f: &dyn Fn(&rg::Expr)) {
        f(self
            .value
            .as_ref()
            .unwrap_or_else(|| panic!("field {:?} should be Some", self.name.value)));
    }
}

trait StructExtension {
    fn must_have_field(&self, name: &str, f: &dyn Fn(&rg::StructField));
    fn must_be_opaque(&self);
}

impl StructExtension for rg::StructDeclaration {
    fn must_have_field(&self, name: &str, f: &dyn Fn(&rg::StructField)) {
        let field = self
            .fields
            .iter()
            .find(|f| f.name.value == name)
            .unwrap_or_else(|| {
                panic!(
                    "struct {:?} should have field with name {:?}",
                    self.name.value, name
                )
            });
        f(field);
    }
    fn must_be_opaque(&self) {
        assert_eq!(
            0,
            self.fields.len(),
            "expected struct {} to be opaque (no fields)",
            self.name.value
        );
    }
}

trait FunctionDeclarationExtension {
    fn must_have_param(&self, name: &str, f: &dyn Fn(&rg::FunctionParam));
}

impl FunctionDeclarationExtension for rg::FunctionDeclaration {
    fn must_have_param(&self, name: &str, f: &dyn Fn(&rg::FunctionParam)) {
        let field = self
            .params
            .iter()
            .find(|f| f.name.value == name)
            .unwrap_or_else(|| {
                panic!(
                    "function {:?} should have param with name {:?}",
                    self.name.value, name
                )
            });
        f(field);
    }
}

trait StringExtension {
    fn struct_name(self) -> String;
    fn enum_name(self) -> String;
    fn const_pointer_name(self) -> String;
    fn mut_pointer_name(self) -> String;
    fn ctype(self) -> String;
}

impl<'a> StringExtension for &'a str {
    fn struct_name(self) -> String {
        rg::Identifier::struct_name(self).value
    }
    fn enum_name(self) -> String {
        rg::Identifier::enum_name(self).value
    }
    fn const_pointer_name(self) -> String {
        format!("*const {}", self)
    }
    fn mut_pointer_name(self) -> String {
        format!("*mut {}", self)
    }
    fn ctype(self) -> String {
        format!("::std::os::raw::c_{}", self)
    }
}

trait TypeExtension {
    fn must_be<P: AsRef<str>>(&self, pat: P);
    fn must_be_const_pointer(&self) -> &rg::Type;
    fn must_be_mut_pointer(&self) -> &rg::Type;
    fn must_be_name(&self) -> String;
}

impl TypeExtension for rg::Type {
    fn must_be<P: AsRef<str>>(&self, pat: P) {
        assert_eq!(
            pat.as_ref(),
            format!("{}", self),
            "(expected is on the left)"
        )
    }
    fn must_be_name(&self) -> String {
        match self {
            rg::Type::Name(s) => s.value.clone(),
            _ => panic!("must be name: {:?}", self),
        }
    }
    fn must_be_const_pointer(&self) -> &rg::Type {
        match self {
            rg::Type::Pointer { konst: true, inner } => inner.as_ref(),
            _ => panic!("must be const pointer: {:?}", self),
        }
    }
    fn must_be_mut_pointer(&self) -> &rg::Type {
        match self {
            rg::Type::Pointer {
                konst: false,
                inner,
            } => inner.as_ref(),
            _ => panic!("must be mut pointer: {:?}", self),
        }
    }
}

trait ExprExtension {
    fn must_be_integer(&self, value: &str);
    fn must_be_identifier(&self, value: &str);
    fn must_be_binary(&self, op: ast::BinaryOperator, f: &dyn Fn(&rg::Expr, &rg::Expr));
}

impl ExprExtension for rg::Expr {
    fn must_be_integer(&self, value: &str) {
        match self {
            rg::Expr::Constant(ast::Constant::Integer(int)) => {
                assert_eq!(value, int.number.as_ref(), "(expected is on the left)")
            }
            _ => panic!("expr should be constant {}: {:?}", value, self),
        }
    }
    fn must_be_identifier(&self, value: &str) {
        match self {
            rg::Expr::Identifier(s) if s == value => {
                // good!
            }
            _ => panic!("expr should be identifier {}: {:?}", value, self),
        }
    }
    fn must_be_binary(&self, op: ast::BinaryOperator, f: &dyn Fn(&rg::Expr, &rg::Expr)) {
        match self {
            rg::Expr::BinaryOperator(actual_op, lhs, rhs) if actual_op == &op => {
                f(lhs.as_ref(), rhs.as_ref())
            }
            _ => panic!("expr should be binary operator {:?}: {:?}", op, self),
        }
    }
}

struct TestSourceProvider {
    files: HashMap<PathBuf, String>,
    path_to_id: HashMap<FilePath, FileId>,
    id_to_info: HashMap<FileId, FileInfo>,
}

impl TestSourceProvider {
    fn new() -> Self {
        Self {
            files: Default::default(),
            path_to_id: Default::default(),
            id_to_info: Default::default(),
        }
    }
}

impl SourceProvider for TestSourceProvider {
    fn resolve(
        &mut self,
        idgen: &mut IdGenerator,
        working_dir: &SourceDir,
        include: &Include,
    ) -> Result<FileId, Error> {
        let inc_path: &Path = include.as_ref();
        let path = FilePath {
            dir: working_dir.clone(),
            rel_path: inc_path.to_path_buf(),
        };

        let id = self.path_to_id.get(&path).copied().unwrap_or_else(|| {
            let id = idgen.generate_id();
            let info = FileInfo {
                id,
                path: path.clone(),
            };
            self.path_to_id.insert(path, info.id);
            self.id_to_info.insert(info.id, info);
            id
        });

        Ok(id)
    }
    fn info(&self, id: FileId) -> Option<&FileInfo> {
        self.id_to_info.get(&id)
    }
    fn read(&self, id: FileId) -> Result<String, Error> {
        let info = self.id_to_info.get(&id).ok_or(Error::UnknownFileId)?;
        Ok(self.files.get(&info.path.rel_path).unwrap().clone())
    }
}

fn parse_units_with(provider: Box<dyn SourceProvider>, ctx: Context, env: Env) -> Vec<rg::Unit> {
    let mut parser = Parser::new(provider, ctx, env);
    let dir = SourceDir {
        pkg: "root".into(),
        path: ".".into(),
    };
    let id = parser
        .provider
        .resolve(&mut parser.idgen, &dir, &Include::Quoted("root.h".into()))
        .unwrap();
    parser.parse_file(id).unwrap();

    let config = Config { arch: Arch::X86_64 };

    parser
        .ordered_files
        .iter()
        .map(|inc| {
            let unit = parser.units.get(inc).unwrap();
            translate_unit(
                &config,
                parser.provider.as_ref(),
                id,
                &unit.declarations[..],
            )
        })
        .collect()
}

fn parse_units(provider: Box<dyn SourceProvider>) -> Vec<rg::Unit> {
    let ctx = Context::new();
    let env = Env::with_msvc();
    parse_units_with(provider, ctx, env)
}

fn parse_unit(input: &str) -> rg::Unit {
    let mut provider = TestSourceProvider::new();
    provider.files.insert("root.h".into(), input.into());
    let v = parse_units(Box::new(provider));
    assert_eq!(v.len(), 1, "should generate single unit");
    v.into_iter().next().unwrap()
}

fn provider(name_source_pairs: &[(&str, &str)]) -> Box<dyn SourceProvider> {
    let mut provider = TestSourceProvider::new();
    for (k, v) in name_source_pairs.iter().cloned() {
        provider.files.insert(k.into(), v.into());
    }
    Box::new(provider)
}

#[test]
fn typedef_integers() {
    let unit = parse_unit(indoc!(
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
        unit.must_have_alias(n, &|d| d.typ.must_be("short".ctype()));
    }
    for n in &["USHORT", "USHORT_INT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("ushort".ctype()));
    }
    for n in &["INT", "SINT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("int".ctype()));
    }
    for n in &["UINT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("uint".ctype()));
    }
    for n in &["LONG", "SLONG", "LONG_INT", "SLONG_INT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("long".ctype()));
    }
    for n in &["ULONG", "ULONG_INT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("ulong".ctype()));
    }
    for n in &["LONG_LONG", "SLONG_LONG", "LONG_LONG_INT", "SLONG_LONG_INT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("longlong".ctype()));
    }
    for n in &["ULONG_LONG", "ULONG_LONG_INT"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("ulonglong".ctype()));
    }
}

#[test]
fn typedef_bool() {
    let unit = parse_unit(indoc!(
        "
        typedef _Bool BOOL;
        "
    ));
    unit.must_have_alias("BOOL", &|f| f.typ.must_be("bool"));
}

#[test]
fn typedef_msvc_fixed_size_integers() {
    let unit = parse_unit(indoc!(
        "
        typedef __int8 INT8;
        typedef __int16 INT16;
        typedef __int32 INT32;
        typedef __int64 INT64;
        typedef unsigned __int8 UINT8;
        typedef unsigned __int16 UINT16;
        typedef unsigned __int32 UINT32;
        typedef unsigned __int64 UINT64;
        "
    ));
    unit.must_have_alias("INT8", &|d| d.typ.must_be("i8"));
    unit.must_have_alias("INT16", &|d| d.typ.must_be("i16"));
    unit.must_have_alias("INT32", &|d| d.typ.must_be("i32"));
    unit.must_have_alias("INT64", &|d| d.typ.must_be("i64"));
    unit.must_have_alias("UINT8", &|d| d.typ.must_be("u8"));
    unit.must_have_alias("UINT16", &|d| d.typ.must_be("u16"));
    unit.must_have_alias("UINT32", &|d| d.typ.must_be("u32"));
    unit.must_have_alias("UINT64", &|d| d.typ.must_be("u64"));
}

#[test]
fn typedef_rare_specifier_order() {
    let unit = parse_unit(indoc!(
        "
        typedef unsigned long int ULI;
        typedef unsigned int long UIL;
        typedef long unsigned int LUI;
        typedef long int unsigned LIU;
        typedef int unsigned long IUL;
        typedef int long unsigned ILU;
        "
    ));
    for n in &["ULI", "UIL", "LUI", "LIU", "IUL", "ILU"] {
        unit.must_have_alias(n, &|d| d.typ.must_be("ulong".ctype()));
    }
}

#[test]
fn typedef_chars() {
    let unit = parse_unit(indoc!(
        "
        typedef char CHAR;
        typedef signed char SCHAR;
        typedef unsigned char UCHAR;
        "
    ));

    unit.must_have_alias("CHAR", &|d| d.typ.must_be("char".ctype()));
    unit.must_have_alias("SCHAR", &|d| d.typ.must_be("schar".ctype()));
    unit.must_have_alias("UCHAR", &|d| d.typ.must_be("uchar".ctype()));
}

#[test]
fn typedef_floats() {
    let unit = parse_unit(indoc!(
        "
        typedef float FLOAT;
        typedef double DOUBLE;
        typedef long double LDOUBLE;
        "
    ));
    unit.must_have_alias("FLOAT", &|d| d.typ.must_be("f32"));
    unit.must_have_alias("DOUBLE", &|d| d.typ.must_be("f64"));
    unit.must_have_alias("LDOUBLE", &|d| d.typ.must_be("[u8; 12]"));
}

#[test]
fn multiple_typedefs() {
    let unit = parse_unit(indoc!(
        "
        typedef unsigned int UINT, *LPUINT, *const LCPUINT;
        "
    ));
    unit.must_have_alias("UINT", &|d| d.typ.must_be("uint".ctype()));
    unit.must_have_alias("LPUINT", &|d| {
        d.typ.must_be("uint".ctype().mut_pointer_name())
    });
    unit.must_have_alias("LCPUINT", &|d| {
        d.typ.must_be("uint".ctype().const_pointer_name())
    });
}

#[test]
fn enum_constants() {
    let unit = parse_unit(indoc!(
        "
        typedef enum Color {
            Red = 0,
            Green = 1,
            Blue,
        } Color, *PColor;
        "
    ));
    unit.must_have_alias("Color", &|d| d.typ.must_be("Color".enum_name()));
    unit.must_have_alias("PColor", &|d| {
        d.typ.must_be("Color".enum_name().mut_pointer_name())
    });
    unit.must_have_enum("Color".enum_name(), &|d| {
        d.must_have_field("Red", &|f| f.must_be_some(&|e| e.must_be_integer("0")));
        d.must_have_field("Green", &|f| f.must_be_some(&|e| e.must_be_integer("1")));
        d.must_have_field("Blue", &|f| f.must_be_none());
    })
}

#[test]
fn enum_exprs() {
    let unit = parse_unit(indoc!(
        "
        enum BadEnum {
            A = 1,
            B = A+A,
            C,
        };
        "
    ));
    unit.must_have_enum("BadEnum".enum_name(), &|d| {
        d.must_have_field("A", &|f| f.must_be_some(&|e| e.must_be_integer("1")));
        d.must_have_field("B", &|f| {
            f.must_be_some(&|e| {
                e.must_be_binary(ast::BinaryOperator::Plus, &|lhs, rhs| {
                    lhs.must_be_identifier("A");
                    rhs.must_be_identifier("A");
                })
            })
        });
        d.must_have_field("C", &|f| f.must_be_none());
    })
}

#[test]
fn nested_structs() {
    let unit = parse_unit(indoc!(
        "
        typedef unsigned long long ULLONG;
        typedef struct S {
            ULLONG a;
            const struct {
                char c;
            } *b;
        } s, *sp;
        "
    ));
    unit.must_have_struct_count(2);
    unit.must_have_struct("S".struct_name(), &|s| {
        s.must_have_field("a", &|f| f.typ.must_be("ULLONG"));
        s.must_have_field("b", &|f| {
            let anon_name = f.typ.must_be_const_pointer().must_be_name();
            unit.must_have_struct(&anon_name, &|s| {
                s.must_have_field("c", &|f| f.typ.must_be("char".ctype()))
            });
        })
    });
    unit.must_have_alias("s", &|d| d.typ.must_be("S".struct_name()));
    unit.must_have_alias("sp", &|d| {
        d.typ.must_be("S".struct_name().mut_pointer_name())
    });
}

#[test]
fn stddef_wchar_t() {
    let units = parse_units(provider(&[
        (
            "stddef.h",
            indoc!(
                "
                // Definitions of common types
                #ifdef _WIN64
                    typedef unsigned __int64 size_t;
                    typedef __int64          ptrdiff_t;
                    typedef __int64          intptr_t;
                #else
                    typedef unsigned int     size_t;
                    typedef int              ptrdiff_t;
                    typedef int              intptr_t;
                #endif

                // Provide a typedef for wchar_t for use under /Zc:wchar_t-
                #ifndef _WCHAR_T_DEFINED
                    #define _WCHAR_T_DEFINED
                    typedef unsigned short wchar_t;
                #endif
                "
            ),
        ),
        (
            "root.h",
            indoc!(
                "
                #define _WIN64
                #include <stddef.h>
                void foobar(wchar_t c, size_t s);
                "
            ),
        ),
    ]));

    let unit = units.iter().find(|&u| u.id == FileId(2)).unwrap();
    unit.must_have_function("foobar", &|f| {
        f.must_have_param("c", &|p| p.typ.must_be("wchar_t"));
        f.must_have_param("s", &|p| p.typ.must_be("size_t"));
    });
}

#[test]
fn opaque_then_not() {
    let unit = parse_unit(indoc!(
        "
        struct A {
            struct B *b;
        };
        "
    ));
    unit.must_have_struct("B".struct_name(), &|d| d.must_be_opaque());

    let unit_before = parse_unit(indoc!(
        "
        struct B {
            int foobar;
        };

        struct A {
            struct B *b;
        };
        "
    ));
    let unit_after = parse_unit(indoc!(
        "
        struct A {
            struct B *b;
        };

        struct B {
            int foobar;
        };
        "
    ));

    for unit in &[unit_before, unit_after] {
        unit.must_have_struct("B".struct_name(), &|d| d.must_have_field("foobar", &|_| {}));
    }
}

#[test]
fn three_musketeers() {
    let unit = parse_unit(indoc!(
        "
        typedef int (A)(int);
        typedef A (*B);
        typedef B C;
        "
    ));
    unit.must_have_alias("A", &|_| {});
    unit.must_have_alias("B", &|d| d.typ.must_be("A"));
    unit.must_have_alias("C", &|d| d.typ.must_be("B"));
}

#[test]
fn typedef_deja_vu() {
    let unit = parse_unit(indoc!(
        "
        typedef int INT;
        typedef int INT;
        "
    ));
    unit.must_have_alias("INT", &|d| d.typ.must_be("int".ctype()));
    unit.must_have_alias_count(1);
}

#[test]
fn typedef_deja_vu_multi() {
    let unit = parse_unit(indoc!(
        "
        typedef struct A { int one; } A, *PA;
        typedef struct A { int two; } A, *PA;
        typedef struct A { int two; } A;
        typedef struct A { int two; } *PA;
        "
    ));
    unit.must_have_alias("A", &|d| d.typ.must_be("A".struct_name()));
    unit.must_have_alias("PA", &|d| {
        d.typ.must_be("A".struct_name().mut_pointer_name())
    });
    unit.must_have_alias_count(2);
}

#[test]
fn declspec() {
    let unit = parse_unit(indoc!(
        "
        __declspec(dllimport)
        int

        get_number(
            int a,
            int b
        );
        "
    ));
    unit.must_have_function("get_number", &|_| {})
}

#[test]
fn mega_constant() {
    // trivia: this made the older, recursive function of the macro expander
    // blow the stack, because of the sheer amount of expansion and substitution
    // going on.
    let unit = parse_unit(indoc!(
        "
        #define XSTATE_LEGACY_FLOATING_POINT        (0)
        #define XSTATE_LEGACY_SSE                   (1)
        #define XSTATE_GSSE                         (2)
        #define XSTATE_AVX                          (XSTATE_GSSE)
        #define XSTATE_MPX_BNDREGS                  (3)
        #define XSTATE_MPX_BNDCSR                   (4)
        #define XSTATE_AVX512_KMASK                 (5)
        #define XSTATE_AVX512_ZMM_H                 (6)
        #define XSTATE_AVX512_ZMM                   (7)
        #define XSTATE_IPT                          (8)
        #define XSTATE_CET_U                        (11)
        #define XSTATE_LWP                          (62)
        #define MAXIMUM_XSTATE_FEATURES             (64)

        #define XSTATE_MASK_LEGACY_FLOATING_POINT   (1ui64 << (XSTATE_LEGACY_FLOATING_POINT))
        #define XSTATE_MASK_LEGACY_SSE              (1ui64 << (XSTATE_LEGACY_SSE))
        #define XSTATE_MASK_LEGACY                  (XSTATE_MASK_LEGACY_FLOATING_POINT | \
                                                    XSTATE_MASK_LEGACY_SSE)

        #define XSTATE_MASK_GSSE                    (1ui64 << (XSTATE_GSSE))
        #define XSTATE_MASK_AVX                     (XSTATE_MASK_GSSE)
        #define XSTATE_MASK_MPX                     ((1ui64 << (XSTATE_MPX_BNDREGS)) | \
                                                    (1ui64 << (XSTATE_MPX_BNDCSR)))

        #define XSTATE_MASK_AVX512                  ((1ui64 << (XSTATE_AVX512_KMASK)) | \
                                                    (1ui64 << (XSTATE_AVX512_ZMM_H)) | \
                                                    (1ui64 << (XSTATE_AVX512_ZMM)))

        #define XSTATE_MASK_IPT                     (1ui64 << (XSTATE_IPT))
        #define XSTATE_MASK_CET_U                   (1ui64 << (XSTATE_CET_U))
        #define XSTATE_MASK_LWP                     (1ui64 << (XSTATE_LWP))

        #define XSTATE_MASK_ALLOWED \
        (XSTATE_MASK_LEGACY | XSTATE_MASK_AVX | XSTATE_MASK_MPX | XSTATE_MASK_AVX512 | \
        XSTATE_MASK_IPT | XSTATE_MASK_CET_U | XSTATE_MASK_LWP)
        "
    ));
    unit.must_have_constant("XSTATE_IPT", &|c| c.must_be("8"));
}
