use super::*;

use Punctuator as P;
use Token as T;
use T::Whitespace as __;

fn id(s: &str) -> T {
    T::Identifier(s.into())
}

fn int(i: i64) -> T {
    T::Integer(i)
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
        Ok(Some(Directive::Define(Define::Value {
            name: "FOO".into(),
            value: vec![].into()
        })))
    );
}

#[test]
fn define_objectlike() {
    assert_eq!(
        parser::directive("#define FOO BAR"),
        Ok(Some(Directive::Define(Define::Value {
            name: "FOO".into(),
            value: vec![id("BAR")].into()
        })))
    );
}

#[test]
fn define_objectlike_2() {
    assert_eq!(
        parser::directive("#define FOO BAR(BAZ)"),
        Ok(Some(Directive::Define(Define::Value {
            name: "FOO".into(),
            value: vec![
                id("BAR"),
                P::ParenOpen.into(),
                id("BAZ"),
                P::ParenClose.into(),
            ]
            .into()
        })))
    );
}

#[test]
fn define_functionlike_1() {
    assert_eq!(
        parser::directive("#define FOO(X, Y) X + Y"),
        Ok(Some(Directive::Define(Define::Replacement {
            name: "FOO".into(),
            params: MacroParams {
                names: vec!["X".into(), "Y".into()],
                has_trailing: false
            },
            value: vec![id("X"), __, P::Plus.into(), __, id("Y")].into()
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
            vec![
                Token::defined(),
                Punctuator::ParenOpen.into(),
                id("FOO_BAR"),
                Punctuator::ParenClose.into()
            ]
            .into()
        ))),
    );

    assert_eq!(
        parser::directive("#ifndef FOO_BAR"),
        Ok(Some(Directive::If(
            vec![
                Punctuator::Bang.into(),
                Token::defined(),
                Punctuator::ParenOpen.into(),
                id("FOO_BAR"),
                Punctuator::ParenClose.into()
            ]
            .into()
        ))),
    );
}
