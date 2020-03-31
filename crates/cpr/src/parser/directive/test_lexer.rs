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
fn tokens_pun() {
    assert_eq!(
        parser::token_stream("?:"),
        Ok(vec!['?'.into(), ':'.into()].into())
    );
}

#[test]
fn tokens_exprs() {
    assert_eq!(
        parser::token_stream("201703L"),
        Ok(vec![int(201703)].into())
    );
    assert_eq!(parser::token_stream("0xbull"), Ok(vec![int(0xb)].into()));

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

#[test]
fn regression_2() {
    let input = "#define _Analysis_mode_(mode) __pragma(warning(disable: 28110 28111 28161 28162)) typedef _Analysis_mode_impl_(mode) int __GENSYM(__prefast_analysis_mode_flag)";
    parser::token_stream(input).unwrap();
}
