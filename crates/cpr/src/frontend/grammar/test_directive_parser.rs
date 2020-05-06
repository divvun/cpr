use super::*;

use Token as T;
use T::WS as __;

fn name(s: &str) -> T {
    T::Name(s.into())
}

fn int(i: i64) -> T {
    T::Int(i)
}

#[test]
fn not_a_directive() {
    assert_eq!(directive(""), Ok(None));
    assert_eq!(directive("int foobar();"), Ok(None));
}

#[test]
fn define_objectlike_empty() {
    assert_eq!(
        directive("#define FOO"),
        Ok(Some(Directive::Define(Define::ObjectLike {
            name: "FOO".into(),
            value: vec![].into()
        })))
    );
}

#[test]
fn define_objectlike() {
    assert_eq!(
        directive("#define FOO BAR"),
        Ok(Some(Directive::Define(Define::ObjectLike {
            name: "FOO".into(),
            value: vec![name("BAR")].into()
        })))
    );
}

#[test]
fn define_objectlike_2() {
    assert_eq!(
        directive("#define FOO BAR(BAZ)"),
        Ok(Some(Directive::Define(Define::ObjectLike {
            name: "FOO".into(),
            value: vec![name("BAR"), '('.into(), name("BAZ"), ')'.into(),].into()
        })))
    );
}

#[test]
fn define_functionlike_1() {
    assert_eq!(
        directive("#define FOO(X, Y) X + Y"),
        Ok(Some(Directive::Define(Define::FunctionLike {
            name: "FOO".into(),
            params: MacroParams::new(&["X", "Y"], false),
            value: vec![name("X"), __, '+'.into(), __, name("Y")].into()
        })))
    );
}

#[test]
fn define_functionlike_variadic_1() {
    assert_eq!(
        directive("#define WHY(...) __VA_ARGS__"),
        Ok(Some(Directive::Define(Define::FunctionLike {
            name: "WHY".into(),
            params: MacroParams::new(&[], true),
            value: vec![name("__VA_ARGS__")].into(),
        })))
    )
}

#[test]
fn define_functionlike_variadic_2() {
    assert_eq!(
        directive("#define WHY(a, b, ...) a b __VA_ARGS__"),
        Ok(Some(Directive::Define(Define::FunctionLike {
            name: "WHY".into(),
            params: MacroParams::new(&["a", "b"], true),
            value: vec![name("a"), __, name("b"), __, name("__VA_ARGS__")].into(),
        })))
    )
}

#[test]
fn include_angle() {
    assert_eq!(
        directive("#include <foo/bar/baz.h>"),
        Ok(Some(Directive::Include(IncludeDirective::Complete(
            Include::System("foo/bar/baz.h".into())
        ))))
    )
}

#[test]
fn include_quoted() {
    assert_eq!(
        directive(r#"#include "shared/um/sure.h""#),
        Ok(Some(Directive::Include(IncludeDirective::Complete(
            Include::Quoted("shared/um/sure.h".into())
        ))))
    )
}

#[test]
fn if_directive() {
    assert_eq!(
        directive("#if FOO"),
        Ok(Some(Directive::If(vec![name("FOO")].into()))),
    );

    assert_eq!(
        directive("#if(1+2)"),
        Ok(Some(Directive::If(
            vec!['('.into(), int(1), '+'.into(), int(2), ')'.into()].into()
        ))),
    );
}

#[test]
fn ifdef() {
    assert_eq!(
        directive("#ifdef FOO_BAR"),
        Ok(Some(Directive::If(
            vec![Token::Defined, '('.into(), name("FOO_BAR"), ')'.into()].into()
        ))),
    );

    assert_eq!(
        directive("#ifndef FOO_BAR"),
        Ok(Some(Directive::If(
            vec![
                '!'.into(),
                Token::Defined,
                '('.into(),
                name("FOO_BAR"),
                ')'.into()
            ]
            .into()
        ))),
    );
}

#[test]
fn define_function_like_no_whitespace() {
    let res = directive("#define foo()bar()");
    assert!(res.is_ok());
}
