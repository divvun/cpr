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
fn tokens() {
    assert_eq!(parser::token_stream("123"), Ok(vec![int(123)].into()));
    assert_eq!(parser::token_stream("0x123"), Ok(vec![int(0x123)].into()));
    assert_eq!(parser::token_stream("0123"), Ok(vec![int(0o123)].into()));

    assert_eq!(
        parser::token_stream("2 + 4"),
        Ok(vec![int(2), __, P::Plus.into(), __, int(4)].into())
    );

    assert_eq!(
        parser::token_stream("f(x) = y;"),
        Ok(vec![
            id("f"),
            P::ParenOpen.into(),
            id("x"),
            P::ParenClose.into(),
            __,
            P::Equal.into(),
            __,
            id("y"),
            P::Semicolon.into(),
        ]
        .into())
    );
}

#[test]
fn regression_1() {
    let input = "#define API_SET_BY_ORDINAL(X,O,PO)                  X @##O NONAME PRIVATE";
    parser::token_stream(input).unwrap();
}
