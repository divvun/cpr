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
    assert_eq!(parser::directive(""), Ok(None));
    assert_eq!(parser::directive("int foobar();"), Ok(None));
}

#[test]
fn define_objectlike_empty() {
    assert_eq!(
        parser::directive("#define FOO"),
        Ok(Some(Directive::Define(Define::ObjectLike {
            name: "FOO".into(),
            value: vec![].into()
        })))
    );
}

#[test]
fn define_objectlike() {
    assert_eq!(
        parser::directive("#define FOO BAR"),
        Ok(Some(Directive::Define(Define::ObjectLike {
            name: "FOO".into(),
            value: vec![name("BAR")].into()
        })))
    );
}

#[test]
fn define_objectlike_2() {
    assert_eq!(
        parser::directive("#define FOO BAR(BAZ)"),
        Ok(Some(Directive::Define(Define::ObjectLike {
            name: "FOO".into(),
            value: vec![name("BAR"), '('.into(), name("BAZ"), ')'.into(),].into()
        })))
    );
}

#[test]
fn define_functionlike_1() {
    assert_eq!(
        parser::directive("#define FOO(X, Y) X + Y"),
        Ok(Some(Directive::Define(Define::FunctionLike {
            name: "FOO".into(),
            params: MacroParams {
                names: vec!["X".into(), "Y".into()],
                has_trailing: false
            },
            value: vec![name("X"), __, '+'.into(), __, name("Y")].into()
        })))
    );
}

#[test]
fn include_angle() {
    assert_eq!(
        parser::directive("#include <foo/bar/baz.h>"),
        Ok(Some(Directive::Include(Include::System(
            "foo/bar/baz.h".into()
        ))))
    )
}

#[test]
fn include_quoted() {
    assert_eq!(
        parser::directive(r#"#include "shared/um/sure.h""#),
        Ok(Some(Directive::Include(Include::Quoted(
            "shared/um/sure.h".into()
        ))))
    )
}

#[test]
fn ifdef() {
    assert_eq!(
        parser::directive("#ifdef FOO_BAR"),
        Ok(Some(Directive::If(
            vec![Token::Defined, '('.into(), name("FOO_BAR"), ')'.into()].into()
        ))),
    );

    assert_eq!(
        parser::directive("#ifndef FOO_BAR"),
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
