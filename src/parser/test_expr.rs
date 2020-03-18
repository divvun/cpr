use super::*;

fn expr(s: &str) -> Expr {
    Expr::from(s)
}

fn assert_reduces(init: Expr, gives: Expr) {
    assert_eq!(init.reduce(), gives, "reducing {:?}", init)
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

#[test]
fn test_permute_1() {
    let mut permutes = Vec::new();
    (ape() & bar()).permute(&mut |v| permutes.push(v));
    assert_eq!(&permutes[..], &[ape() & bar(), bar() & ape()]);
}

#[test]
fn test_permute_2() {
    let mut permutes = Vec::new();
    (ape() | bar()).permute(&mut |v| permutes.push(v));
    assert_eq!(&permutes[..], &[ape() | bar(), bar() | ape()]);
}

#[test]
fn test_permute_3() {
    let mut permutes = Vec::new();
    (!(ape() | bar())).permute(&mut |v| permutes.push(v));
    assert_eq!(&permutes[..], &[!(ape() | bar()), !(bar() | ape())]);
}

#[test]
fn test_permute_4() {
    let mut permutes = Vec::new();
    ((ape() | bar()) | chai()).permute(&mut |v| permutes.push(v));
    assert_eq!(
        &permutes[..],
        &[
            (ape() | bar()) | chai(),
            chai() | (ape() | bar()),
            (bar() | ape()) | chai(),
            chai() | (bar() | ape()),
        ]
    );
}

#[test]
fn test_permute_5() {
    let mut permutes = Vec::new();
    ((ape() | bar()) & chai()).permute(&mut |v| permutes.push(v));
    assert_eq!(
        &permutes[..],
        &[
            (ape() | bar()) & chai(),
            chai() & (ape() | bar()),
            (bar() | ape()) & chai(),
            chai() & (bar() | ape()),
        ]
    );
}

#[test]
fn test_sort_1() {
    (bar() | ape()).permute(&mut |v| {
        assert_eq!(v.sort(), ape() | bar());
    })
}

#[test]
fn test_sort_2() {
    (chai() & bar() & ape()).permute(&mut |v| {
        assert_eq!(v.sort(), ape() & (bar() & chai()));
    })
}

#[test]
fn test_sort_3() {
    ((chai() & bar()) | ape()).permute(&mut |v| {
        assert_eq!(v.sort(), ape() | (bar() & chai()));
    })
}

#[test]
fn test_not_1() {
    assert_reduces(!!ape(), ape());
}

#[test]
fn test_not_2() {
    assert_reduces(!!!ape(), !ape());
}

#[test]
fn test_and_1() {
    assert_reduces(ape() & ape(), ape());
}

#[test]
fn test_and_2() {
    assert_reduces(ape() & (ape() & bar()), ape() & bar());
}

#[test]
fn test_and_3() {
    assert_reduces((ape() & bar()) & ape(), ape() & bar());
}

#[test]
fn test_or_1() {
    assert_reduces(ape() | ape(), ape());
}

#[test]
fn test_or_2() {
    assert_reduces(ape() | !ape(), Expr::True);
}

#[test]
fn test_or_3() {
    assert_reduces(ape() | Expr::False, ape());
}

#[test]
fn test_or_4() {
    assert_reduces(Expr::False | ape(), ape());
}
