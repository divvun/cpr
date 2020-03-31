use super::*;

fn sym(s: &str) -> Expr {
    Expr::Symbol(s.to_string())
}

fn def(s: &str) -> Expr {
    Expr::Defined(s.to_string())
}

#[test]
fn simple_exprs() {
    assert_eq!(expr("azAZ09_"), Ok(sym("azAZ09_")));
    assert_eq!(expr("__$USD"), Ok(sym("__$USD")));
    assert!(expr("sorry🔥no_emoji").is_err());

    assert_eq!(expr("(foobar)"), Ok(sym("foobar")));
    assert_eq!(expr("((foobar))"), Ok(sym("foobar")));
    assert_eq!(expr("a && b"), Ok(Expr::And(vec![sym("a"), sym("b")])));
    assert_eq!(expr("a || b"), Ok(Expr::Or(vec![sym("a"), sym("b")])));
    assert_eq!(expr("defined foo"), Ok(def("foo")));
    assert_eq!(expr("defined(foo)"), Ok(def("foo")));

    assert_eq!(
        expr("getstuff()"),
        Ok(Expr::Call("getstuff".to_string(), vec![]))
    );
    assert_eq!(
        expr("identity(x)"),
        Ok(Expr::Call("identity".to_string(), vec![sym("x")]))
    );
    assert_eq!(
        expr("add(x, y)"),
        Ok(Expr::Call("add".to_string(), vec![sym("x"), sym("y")]))
    );
    assert!(expr("missing_arg(x, y,)").is_err());
}

#[test]
fn binary_ops() {
    assert_eq!(
        expr("a > b"),
        Ok(Expr::Binary(
            BinaryOperator::Greater,
            Box::new(sym("a")),
            Box::new(sym("b"))
        ))
    );
    assert_eq!(
        expr("a >= b"),
        Ok(Expr::Binary(
            BinaryOperator::GreaterOrEqual,
            Box::new(sym("a")),
            Box::new(sym("b"))
        ))
    );
    assert_eq!(
        expr("a < b"),
        Ok(Expr::Binary(
            BinaryOperator::Less,
            Box::new(sym("a")),
            Box::new(sym("b"))
        ))
    );
    assert_eq!(
        expr("a <= b"),
        Ok(Expr::Binary(
            BinaryOperator::LessOrEqual,
            Box::new(sym("a")),
            Box::new(sym("b"))
        ))
    );
    assert_eq!(
        expr("a == b"),
        Ok(Expr::Binary(
            BinaryOperator::Equals,
            Box::new(sym("a")),
            Box::new(sym("b"))
        ))
    );
    assert_eq!(
        expr("a != b"),
        Ok(Expr::Binary(
            BinaryOperator::NotEquals,
            Box::new(sym("a")),
            Box::new(sym("b"))
        ))
    );
    assert_eq!(
        expr("a | b"),
        Ok(Expr::Binary(
            BinaryOperator::BitwiseOr,
            Box::new(sym("a")),
            Box::new(sym("b"))
        ))
    );
}

#[test]
fn precedence() {
    assert_eq!(
        expr("a && b && c"),
        Ok(Expr::And(vec![sym("a"), sym("b"), sym("c")]))
    );
    assert_eq!(
        expr("a && b || c"),
        Ok(Expr::Or(vec![
            Expr::And(vec![sym("a"), sym("b"),]),
            sym("c")
        ]))
    );
    assert_eq!(
        expr("a || b && c"),
        Ok(Expr::Or(vec![
            sym("a"),
            Expr::And(vec![sym("b"), sym("c")]),
        ]))
    );
    assert_eq!(
        expr("a && b || c && d"),
        Ok(Expr::Or(vec![
            Expr::And(vec![sym("a"), sym("b")]),
            Expr::And(vec![sym("c"), sym("d")]),
        ]))
    );
    assert_eq!(
        expr("a || b && c || d"),
        Ok(Expr::Or(vec![
            sym("a"),
            Expr::And(vec![sym("b"), sym("c")]),
            sym("d"),
        ]))
    );

    assert_eq!(
        expr("foo && bar(baz)"),
        Ok(Expr::And(vec![
            sym("foo"),
            Expr::Call("bar".to_string(), vec![sym("baz")]),
        ]))
    );
    assert_eq!(
        expr("defined foo && bar"),
        Ok(Expr::And(vec![def("foo"), sym("bar"),]))
    );
    assert_eq!(
        expr("foo && defined bar"),
        Ok(Expr::And(vec![sym("foo"), def("bar"),]))
    );
    assert!(expr("defined add(x, y)").is_err())
}

#[test]
fn regression_1() {
    expr("0 && ( false || false || false || !false )").unwrap();
}
