use super::*;
use crate::parser::MacroParams;
use Punctuator::*;

fn expands_to(ctx: &Context, src: &[Token], dst: &[Token], msg: &str) {
    let input: TokenStream = src.iter().cloned().collect::<Vec<_>>().into();
    let output: TokenStream = dst.iter().cloned().collect::<Vec<_>>().into();
    assert_eq!(input.must_expand_single(ctx), output, "{}", msg);
}

#[test]
fn defined() {
    let mut ctx = Context::new();
    ctx.push(
        Expr::bool(true),
        Define::Value {
            name: "FOO".into(),
            value: vec![].into(),
        },
    );

    expands_to(&ctx, &[], &[], "empty stream should stay empty");
    expands_to(&ctx, &[Token::id("FOO")], &[], "empty def should expand");
    expands_to(
        &ctx,
        &[Token::defined(), Token::id("FOO")],
        &[Token::bool(true)],
        "empty def counts as defined (no parens)",
    );
    expands_to(
        &ctx,
        &[
            Token::defined(),
            ParenOpen.into(),
            Token::id("FOO"),
            ParenClose.into(),
        ],
        &[Token::bool(true)],
        "empty def counts as defined (with parens)",
    );
    expands_to(
        &ctx,
        &[Token::defined(), Token::id("BAR")],
        &[Token::bool(false)],
        "undefined symbol is falsy (no parens)",
    );
    expands_to(
        &ctx,
        &[
            Token::defined(),
            ParenOpen.into(),
            Token::id("BAR"),
            ParenClose.into(),
        ],
        &[Token::bool(false)],
        "undefined symbol is falsy (with parens)",
    );
}

#[test]
fn function_like_noargs() {
    let mut ctx = Context::new();
    ctx.push(
        Expr::bool(true),
        Define::Replacement {
            name: "FOO".into(),
            params: MacroParams {
                names: vec![],
                has_trailing: false,
            },
            value: vec![].into(),
        },
    );

    expands_to(
        &ctx,
        &[Token::id("FOO"), ParenOpen.into(), ParenClose.into()],
        &[],
        "simple macro invocation",
    );
}

#[test]
fn function_like_one_arg() {
    let mut ctx = Context::new();
    ctx.push(
        Expr::bool(true),
        Define::Replacement {
            name: "FOO".into(),
            params: MacroParams {
                names: vec!["X".into()],
                has_trailing: false,
            },
            value: vec![Token::id("X")].into(),
        },
    );

    expands_to(
        &ctx,
        &[Token::id("FOO"), ParenOpen.into(), ParenClose.into()],
        &[],
        "one arg (empty token stream)",
    );
    expands_to(
        &ctx,
        &[
            Token::id("FOO"),
            ParenOpen.into(),
            Token::id("x"),
            ParenClose.into(),
        ],
        &[Token::id("x")],
        "one arg (single token)",
    );
    expands_to(
        &ctx,
        &[
            Token::id("FOO"),
            ParenOpen.into(),
            Token::id("foo"),
            Plus.into(),
            Token::id("bar"),
            ParenClose.into(),
        ],
        &[Token::id("foo"), Plus.into(), Token::id("bar")],
        "one arg (three tokens)",
    )
}

#[test]
fn function_like_two_args() {
    let mut ctx = Context::new();
    ctx.push(
        Expr::bool(true),
        Define::Replacement {
            name: "ADD".into(),
            params: MacroParams {
                names: vec!["X".into(), "Y".into()],
                has_trailing: false,
            },
            value: vec![Token::id("X"), Plus.into(), Token::id("Y")].into(),
        },
    );
    ctx.push(
        Expr::bool(true),
        Define::Replacement {
            name: "MUL".into(),
            params: MacroParams {
                names: vec!["X".into(), "Y".into()],
                has_trailing: false,
            },
            value: vec![Token::id("X"), Star.into(), Token::id("Y")].into(),
        },
    );

    expands_to(
        &ctx,
        &[
            Token::id("ADD"),
            ParenOpen.into(),
            Token::int(2),
            Comma.into(),
            Token::int(4),
            ParenClose.into(),
        ],
        &[Token::int(2), Plus.into(), Token::int(4)],
        "two args",
    );
}
