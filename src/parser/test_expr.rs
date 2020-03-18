use super::*;

fn expr(s: &str) -> Expr {
    Expr::from(s)
}

fn assert_simplifies(init: Expr, gives: Expr) {
    let simple = init.simplify();
    let gives = gives.sort();
    assert_eq!(
        simple,
        gives,
        "\ninit = {:?}\nsimple = {:?}\ngives = {:?}\n",
        init,
        simple,
        gives.sort()
    )
}

fn ape() -> Expr {
    expr("ape")
}
fn bar() -> Expr {
    expr("bar")
}
fn chai() -> Expr {
    expr("chai")
}
fn ding() -> Expr {
    expr("ding")
}

#[test]
fn test_sort_1() {
    let v = bar() | ape();
    assert_eq!(v.sort(), ape() | bar());
}

#[test]
fn test_sort_2() {
    let v = chai() & bar() & ape();
    assert_eq!(v.sort(), ape() & bar() & chai(), "orig = {:?}", v);
}

#[test]
fn test_sort_3() {
    let v = chai() & bar() | ape();
    assert_eq!(v.sort(), ape() | bar() & chai());
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
    assert_simplifies(ape() & !ape(), Expr::False);
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
    assert_simplifies(ape() | !ape(), Expr::True);
}

#[test]
fn test_simplify_or_3() {
    assert_simplifies(ape() | Expr::False, ape());
}

#[test]
fn test_simplify_or_4() {
    assert_simplifies(Expr::False | ape(), ape());
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
    assert_simplifies(a() & !b() & c(), Expr::False);
    assert_simplifies(a() & !b() & !c(), ape() & !bar());
}

#[test]
fn test_simplify_nested_negated_3() {
    let init = ape() & (ape() & bar()) & (ape() & !bar()) & ape();
    assert_simplifies(init, Expr::False);
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
    assert_simplifies(init, Expr::False);
}
