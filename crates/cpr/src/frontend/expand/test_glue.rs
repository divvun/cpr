use super::*;

#[test]
fn test_glue_identifiers() {
    let l = THS(Token::name("foo"), Default::default());
    let r = THS(Token::name("bar"), Default::default());
    assert_eq!(l.glue(r), THS(Token::name("foobar"), Default::default()),);
}

#[test]
fn test_glue_integers() {
    let l = THS(Token::Int(12), Default::default());
    let r = THS(Token::Int(345), Default::default());
    assert_eq!(l.glue(r), THS(Token::Int(12345), Default::default()),);
}
