use super::*;

fn expands_to(ctx: &Context, src: &[Token], dst: &[Token], msg: &str) {
    let input: TokenStream = src.iter().cloned().collect::<Vec<_>>().into();
    let output: TokenStream = dst.iter().cloned().collect::<Vec<_>>().into();
    assert_eq!(input.must_expand_single(ctx), output, "{}", msg);
}

#[test]
fn defined() {
    use Punctuator::*;

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
