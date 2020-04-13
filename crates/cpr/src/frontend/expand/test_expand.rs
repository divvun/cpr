use super::*;
use grammar::{Directive, MacroParams, TokenSeq};

fn expands_to(ctx: &Context, src: &[Token], dst: &[Token], msg: &str) {
    let input: TokenSeq = src.iter().cloned().collect::<Vec<_>>().into();
    let output: TokenSeq = dst.iter().cloned().collect::<Vec<_>>().into();
    assert_eq!(input.expand(ctx).unwrap(), output, "{}", msg);
}

#[test]
fn defined() {
    let mut ctx = Context::new();
    ctx.push(Define::ObjectLike {
        name: "FOO".into(),
        value: vec![].into(),
    });

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
    ctx.push(Define::FunctionLike {
        name: "FOO".into(),
        params: MacroParams {
            names: vec![],
            has_trailing: false,
        },
        value: vec![].into(),
    });

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
    ctx.push(Define::FunctionLike {
        name: "FOO".into(),
        params: MacroParams {
            names: vec!["X".into()],
            has_trailing: false,
        },
        value: vec![Token::name("X")].into(),
    });

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
    ctx.push(Define::FunctionLike {
        name: "ADD".into(),
        params: MacroParams {
            names: vec!["X".into(), "Y".into()],
            has_trailing: false,
        },
        value: vec![Token::name("X"), '+'.into(), Token::name("Y")].into(),
    });
    ctx.push(Define::FunctionLike {
        name: "MUL".into(),
        params: MacroParams {
            names: vec!["X".into(), "Y".into()],
            has_trailing: false,
        },
        value: vec![Token::name("X"), '*'.into(), Token::name("Y")].into(),
    });

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
        let dir = grammar::directive(input)
            .expect("test directive must be parsable")
            .expect("test must specify exactly one directive");
        let def = match dir {
            Directive::Define(d) => d,
            _ => panic!(),
        };
        ctx.push(def)
    }

    fn exp(ctx: &Context, input: &str, output: &str) {
        let input = grammar::token_stream(input).unwrap();
        let output = grammar::token_stream(output).unwrap();
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

#[test]
fn test_compliant() {
    fn def(ctx: &mut Context, input: &str) {
        let dir = grammar::directive(input)
            .expect("test directive must be parsable")
            .expect("test must specify exactly one directive");
        let def = match dir {
            Directive::Define(d) => d,
            _ => panic!(),
        };
        ctx.push(def)
    }

    fn exp(ctx: &Context, input: &str, output: &str) {
        log::debug!("=============================================");
        let input = grammar::token_stream(input).unwrap();
        let expected = grammar::token_stream(output).unwrap();
        let actual = input.expand(&ctx).unwrap();
        log::debug!("expected = {:?}", expected);
        log::debug!("actual = {:?}", actual);
        assert_eq!(actual, expected, "(actual is on the left)");
    }

    let mut ctx = Context::new();
    exp(
        &ctx,
        r#""C strings" " can be" " disjoint""#,
        r#""C strings can be disjoint""#,
    );

    def(&mut ctx, "#define EMPTY() ");
    def(&mut ctx, "#define IDENTITY(x) x");
    def(&mut ctx, "#define ADD(x, y) x+y");
    def(&mut ctx, "#define MUL(x, y) x*y");
    def(&mut ctx, "#define FOO(x) FOO()");
    def(&mut ctx, "#define PASTE(x, y) x ## y");
    def(&mut ctx, "#define STRGZ(x) # x");
    def(&mut ctx, "#define STRGZ2(x, y) # x # y");
    def(&mut ctx, "#define STRGZ3(x, y, z) # x # y # z");
    def(&mut ctx, "#define INC(x,y) INC(x,INC(x,y))");

    exp(&ctx, "defined EMPTY", "1");
    exp(&ctx, "defined (EMPTY)", "1");
    exp(&ctx, "defined(EMPTY )", "1");
    exp(&ctx, "defined  (    EMPTY  ) ", "1 ");
    exp(&ctx, "defined NOTDEFINED", "0");

    exp(&ctx, "EMPTY()", "");
    exp(&ctx, "1+EMPTY()3", "1+3");
    exp(&ctx, "IDENTITY(9)+IDENTITY(2)", "9+2");
    exp(&ctx, "ADD(MUL(1,2),3)", "1*2+3");
    exp(&ctx, "ADD(ADD(ADD(1,2),3),4)", "1+2+3+4");
    exp(&ctx, "FOO(y)", "FOO()");
    exp(&ctx, "PASTE(foo,bar)", "foobar");
    exp(&ctx, "PASTE(123,456)", "123456");
    exp(&ctx, "STRGZ(2 + 3)", r#""2 + 3""#);
    exp(&ctx, "STRGZ(   2 + 3        )", r#""2 + 3""#);
    exp(&ctx, "STRGZ2(  foo ,  bar )", r#""foo" "bar""#);
    exp(&ctx, "STRGZ3( foo, bar , baz)", r#""foo" "bar" "baz""#);
    exp(&ctx, "INC(1,2)", "INC(1,INC(1,2))")
}
