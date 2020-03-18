use super::*;

fn expr(s: &str) -> Expr {
    Expr::from(s)
}

fn assert_reduces(init: Expr, gives: Expr) {
    let reduced = init.reduce();
    let gives = gives.sort();
    assert_eq!(
        reduced,
        gives,
        "\ninit = {:?}\nreduced = {:?}\ngives = {:?}\n",
        init,
        reduced,
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
fn test_reduce_identity() {
    assert_reduces(ape(), ape());
}

#[test]
fn test_reduce_not_0() {
    assert_reduces(!ape(), !ape());
}

#[test]
fn test_reduce_not_1() {
    assert_reduces(!!ape(), ape());
}

#[test]
fn test_reduce_not_2() {
    assert_reduces(!!!ape(), !ape());
}

#[test]
fn test_reduce_and_1() {
    assert_reduces(ape() & ape(), ape());
}

#[test]
fn test_reduce_and_1b() {
    assert_reduces(ape() & !ape(), Expr::False);
}

#[test]
fn test_reduce_and_2() {
    assert_reduces(ape() & (ape() & bar()), ape() & bar());
}

#[test]
fn test_reduce_and_3() {
    assert_reduces((ape() & bar()) & ape(), ape() & bar());
}

#[test]
fn test_reduce_or_1() {
    assert_reduces(ape() | ape(), ape());
}

#[test]
fn test_reduce_or_2() {
    assert_reduces(ape() | !ape(), Expr::True);
}

#[test]
fn test_reduce_or_3() {
    assert_reduces(ape() | Expr::False, ape());
}

#[test]
fn test_reduce_or_4() {
    assert_reduces(Expr::False | ape(), ape());
}

#[test]
fn test_reduce_deeply_nested() {
    assert_reduces(
        ape() & (ape() & bar()) & ((ape() & bar()) & chai()),
        ape() & bar() & chai(),
    );
}

#[test]
fn test_reduce_nested_negated() {
    assert_reduces(ape() & !(ape() & bar()), ape() & !bar());
}

#[test]
fn test_reduce_nested_negated_2() {
    let a = || ape();
    let b = || ape() & bar();
    let c = || ape() & bar() & chai();

    assert_reduces(a() & b() & c(), ape() & bar() & chai());
    assert_reduces(a() & b() & !c(), ape() & bar() & !chai());
    assert_reduces(a() & !b() & c(), Expr::False);
    assert_reduces(a() & !b() & !c(), ape() & !bar());
}

#[test]
fn test_reduce_nested_negated_3() {
    let init = ape() & (ape() & bar()) & (ape() & !bar()) & ape();
    assert_reduces(init, Expr::False);
}

#[test]
fn test_reduce_nested_negated_4() {
    let init = ape() & (ape() & bar()) & !(ape() & !bar()) & ape();
    assert_reduces(init, ape() & bar());
}

#[test]
fn test_reduce_nested_negated_5() {
    let init = ape() & !(ape() & bar()) & (ape() & !bar()) & ape();
    assert_reduces(init, ape() & !bar());
}

#[test]
fn test_reduce_nested_negated_6() {
    let init = ape() & !(ape() & bar()) & !(ape() & !bar()) & ape();
    assert_reduces(init, Expr::False);
}
