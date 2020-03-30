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
fn tokens_int() {
    assert_eq!(parser::token_stream("123"), Ok(vec![int(123)].into()));
    assert_eq!(parser::token_stream("0x123"), Ok(vec![int(0x123)].into()));
    assert_eq!(parser::token_stream("0123"), Ok(vec![int(0o123)].into()));
}

#[test]
fn tokens_str() {
    assert_eq!(
        parser::token_stream(r#" "hello world" "#),
        Ok(vec![Token::WS, Token::Str("hello world".into()), Token::WS].into()),
    );

    assert_eq!(
        parser::token_stream(r#" "escaped \" double quote" "#),
        Ok(vec![
            Token::WS,
            Token::Str(r#"escaped \" double quote"#.into()),
            Token::WS
        ]
        .into())
    );

    assert_eq!(
        parser::token_stream(r#""foo""bar""#),
        Ok(vec![Token::Str("foobar".into()),].into()),
        "actual is on the left"
    );
    assert_eq!(
        parser::token_stream(r#""foo" "bar""#),
        Ok(vec![Token::Str("foobar".into()),].into()),
        "actual is on the left"
    );
    assert_eq!(
        parser::token_stream(r#""foo"    "bar""#),
        Ok(vec![Token::Str("foobar".into()),].into()),
        "actual is on the left"
    )
}

#[test]
fn tokens_exprs() {
    assert_eq!(
        parser::token_stream("2 + 4"),
        Ok(vec![int(2), __, '+'.into(), __, int(4)].into())
    );

    assert_eq!(
        parser::token_stream("f(x) = y;"),
        Ok(vec![
            name("f"),
            '('.into(),
            name("x"),
            ')'.into(),
            __,
            '='.into(),
            __,
            name("y"),
            ';'.into(),
        ]
        .into())
    );
}

#[test]
fn regression_1() {
    let input = "#define API_SET_BY_ORDINAL(X,O,PO)                  X @##O NONAME PRIVATE";
    parser::token_stream(input).unwrap();
}
