use super::*;

fn expr(s: &str) -> Expr {
    Expr::from(s)
}

fn assert_reduces(init: Expr, gives: Expr) {
    assert_eq!(init.reduce(), gives, "reducing {:?}", init)
}

fn foo() -> Expr {
    expr("foo")
}
fn bar() -> Expr {
    expr("bar")
}

#[test]
fn test_permute() {
    let mut permutes = Vec::new();
    (foo() & bar()).permute(&mut |v| permutes.push(v));
    assert_eq!(&permutes[..], &[foo() & bar(), bar() & foo()]);
}

#[test]
fn test_not_1() {
    assert_reduces(!!foo(), foo());
}

#[test]
fn test_not_2() {
    assert_reduces(!!!foo(), !foo());
}

#[test]
fn test_and_1() {
    assert_reduces(foo() & foo(), foo());
}

#[test]
fn test_and_2() {
    assert_reduces(foo() & (foo() & bar()), foo() & bar());
}

#[test]
fn test_and_3() {
    assert_reduces((foo() & bar()) & foo(), foo() & bar());
}

#[test]
fn test_or_1() {
    assert_reduces(foo() | foo(), foo());
}

#[test]
fn test_or_2() {
    assert_reduces(foo() | !foo(), Expr::True);
}

#[test]
fn test_or_3() {
    assert_reduces(foo() | Expr::False, foo());
}

#[test]
fn test_or_4() {
    assert_reduces(Expr::False | foo(), foo());
}
