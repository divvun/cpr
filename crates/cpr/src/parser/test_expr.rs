use super::*;

fn def(s: &str) -> Expr {
    Expr::Defined(s.to_string())
}

fn assert_simplifies(init: Expr, gives: Expr) {
    let simple = init.simplify();
    assert_eq!(
        simple, gives,
        "\ninit = {:?}\nsimple = {:?}\ngives = {:?}\n",
        init, simple, gives
    )
}

fn ape() -> Expr {
    def("ape")
}
fn bar() -> Expr {
    def("bar")
}
fn chai() -> Expr {
    def("chai")
}
fn ding() -> Expr {
    def("ding")
}

#[test]
fn test_simplify_identity() {
    assert_simplifies(ape(), ape());
}

#[test]
fn test_simplify_not_0() {
    assert_simplifies(!ape(), !ape());
}

#[test]
fn test_simplify_not_1() {
    assert_simplifies(!!ape(), ape());
}

#[test]
fn test_simplify_not_2() {
    assert_simplifies(!!!ape(), !ape());
}

#[test]
fn test_simplify_and_1() {
    assert_simplifies(ape() & ape(), ape());
}

#[test]
fn test_simplify_and_1b() {
    assert_simplifies(ape() & !ape(), Expr::bool(false));
}

#[test]
fn test_simplify_and_2() {
    assert_simplifies(ape() & (ape() & bar()), ape() & bar());
}

#[test]
fn test_simplify_and_3() {
    assert_simplifies((ape() & bar()) & ape(), ape() & bar());
}

#[test]
fn test_simplify_or_1() {
    assert_simplifies(ape() | ape(), ape());
}

#[test]
fn test_simplify_or_2() {
    assert_simplifies(ape() | !ape(), Expr::bool(true));
}

#[test]
fn test_simplify_or_3() {
    assert_simplifies(ape() | Expr::bool(false), ape());
}

#[test]
fn test_simplify_or_4() {
    assert_simplifies(Expr::bool(false) | ape(), ape());
}

#[test]
fn test_simplify_deeply_nested() {
    assert_simplifies(
        ape() & (ape() & bar()) & ((ape() & bar()) & chai()),
        ape() & bar() & chai(),
    );
}

#[test]
fn test_simplify_nested_negated() {
    assert_simplifies(ape() & !(ape() & bar()), ape() & !bar());
}

#[test]
fn test_simplify_nested_negated_2() {
    let a = || ape();
    let b = || ape() & bar();
    let c = || ape() & bar() & chai();

    assert_simplifies(a() & b() & c(), ape() & bar() & chai());
    assert_simplifies(a() & b() & !c(), ape() & bar() & !chai());
    assert_simplifies(a() & !b() & c(), Expr::bool(false));
    assert_simplifies(a() & !b() & !c(), ape() & !bar());
}

#[test]
fn test_simplify_nested_negated_3() {
    let init = ape() & (ape() & bar()) & (ape() & !bar()) & ape();
    assert_simplifies(init, Expr::bool(false));
}

#[test]
fn test_simplify_nested_negated_4() {
    let init = ape() & (ape() & bar()) & !(ape() & !bar()) & ape();
    assert_simplifies(init, ape() & bar());
}

#[test]
fn test_simplify_nested_negated_5() {
    let init = ape() & !(ape() & bar()) & (ape() & !bar()) & ape();
    assert_simplifies(init, ape() & !bar());
}

#[test]
fn test_simplify_nested_negated_6() {
    let init = ape() & !(ape() & bar()) & !(ape() & !bar()) & ape();
    assert_simplifies(init, Expr::bool(false));
}
