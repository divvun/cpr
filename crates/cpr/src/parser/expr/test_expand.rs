use super::*;
use crate::parser::MacroParams;
use directive::Directive;

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
        Define::ObjectLike {
            name: "FOO".into(),
            value: vec![].into(),
        },
    );

    expands_to(&ctx, &[], &[], "empty stream should stay empty");
    expands_to(&ctx, &[Token::name("FOO")], &[], "empty def should expand");
    expands_to(
        &ctx,
        &[Token::Defined, Token::name("FOO")],
        &[Token::bool(true)],
        "empty def counts as defined (no parens)",
    );
    expands_to(
        &ctx,
        &[Token::Defined, '('.into(), Token::name("FOO"), ')'.into()],
        &[Token::bool(true)],
        "empty def counts as defined (with parens)",
    );
    expands_to(
        &ctx,
        &[Token::Defined, Token::name("BAR")],
        &[Token::bool(false)],
        "undefined symbol is falsy (no parens)",
    );
    expands_to(
        &ctx,
        &[Token::Defined, '('.into(), Token::name("BAR"), ')'.into()],
        &[Token::bool(false)],
        "undefined symbol is falsy (with parens)",
    );
}

#[test]
fn function_like_noargs() {
    let mut ctx = Context::new();
    ctx.push(
        Expr::bool(true),
        Define::FunctionLike {
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
        &[Token::name("FOO"), '('.into(), ')'.into()],
        &[],
        "simple macro invocation",
    );
}

#[test]
fn function_like_one_arg() {
    let mut ctx = Context::new();
    ctx.push(
        Expr::bool(true),
        Define::FunctionLike {
            name: "FOO".into(),
            params: MacroParams {
                names: vec!["X".into()],
                has_trailing: false,
            },
            value: vec![Token::name("X")].into(),
        },
    );

    expands_to(
        &ctx,
        &[Token::name("FOO"), '('.into(), ')'.into()],
        &[],
        "one arg (empty token stream)",
    );
    expands_to(
        &ctx,
        &[Token::name("FOO"), '('.into(), Token::name("x"), ')'.into()],
        &[Token::name("x")],
        "one arg (single token)",
    );
    expands_to(
        &ctx,
        &[
            Token::name("FOO"),
            '('.into(),
            Token::name("foo"),
            '+'.into(),
            Token::name("bar"),
            ')'.into(),
        ],
        &[Token::name("foo"), '+'.into(), Token::name("bar")],
        "one arg (three tokens)",
    )
}

#[test]
fn function_like_two_args() {
    let mut ctx = Context::new();
    ctx.push(
        Expr::bool(true),
        Define::FunctionLike {
            name: "ADD".into(),
            params: MacroParams {
                names: vec!["X".into(), "Y".into()],
                has_trailing: false,
            },
            value: vec![Token::name("X"), '+'.into(), Token::name("Y")].into(),
        },
    );
    ctx.push(
        Expr::bool(true),
        Define::FunctionLike {
            name: "MUL".into(),
            params: MacroParams {
                names: vec!["X".into(), "Y".into()],
                has_trailing: false,
            },
            value: vec![Token::name("X"), '*'.into(), Token::name("Y")].into(),
        },
    );

    expands_to(
        &ctx,
        &[
            Token::name("ADD"),
            '('.into(),
            Token::int(2),
            ','.into(),
            Token::int(4),
            ')'.into(),
        ],
        &[Token::int(2), '+'.into(), Token::int(4)],
        "two args",
    );

    expands_to(
        &ctx,
        &[
            Token::name("ADD"),
            '('.into(),
            Token::name("MUL"),
            '('.into(),
            Token::int(2),
            ','.into(),
            Token::int(3),
            ')'.into(),
            ','.into(),
            Token::int(4),
            ')'.into(),
        ],
        &[
            Token::int(2),
            '*'.into(),
            Token::int(3),
            '+'.into(),
            Token::int(4),
        ],
        "nested calls",
    );
}

#[test]
fn readable_tests() {
    fn def(ctx: &mut Context, input: &str) {
        let dir = directive::parser::directive(input)
            .expect("test directive must be parsable")
            .expect("test must specify exactly one directive");
        let def = match dir {
            Directive::Define(d) => d,
            _ => panic!(),
        };
        ctx.push(Expr::bool(true), def)
    }

    fn exp(ctx: &Context, input: &str, output: &str) {
        let input = directive::parser::token_stream(input).unwrap();
        let output = directive::parser::token_stream(output).unwrap();
        expands_to(ctx, &input.0, &output.0, "");
    }

    let mut ctx = Context::new();
    def(&mut ctx, "#define EMPTY() ");
    def(&mut ctx, "#define IDENTITY(x) x");
    def(&mut ctx, "#define ADD(x, y) x+y");
    def(&mut ctx, "#define MUL(x, y) x*y");
    def(&mut ctx, "#define FOO(x) FOO()");

    exp(&ctx, "EMPTY()", "");
    exp(&ctx, "1+EMPTY()3", "1+3");
    exp(&ctx, "IDENTITY(9)+IDENTITY(2)", "9+2");
    exp(&ctx, "ADD(MUL(1,2),3)", "1*2+3");
    exp(&ctx, "ADD(ADD(ADD(ADD(1, 2), 3), 4), 5)", "1+2+3+4+5");
    exp(&ctx, "FOO(y)", "FOO()");
}
