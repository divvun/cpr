use super::*;

fn expr(s: &str) -> Expr {
    Expr::from(s)
}

fn assert_reduces(init: Expr, gives: Expr) {
    init.permute(&mut |perm| {
        assert_eq!(
            perm.reduce().sort(),
            gives.sort(),
            "\ninit = {:?}\nperm = {:?}\ngives = {:?}",
            init,
            perm,
            gives.sort()
        )
    });
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
    let original = (ape() | bar()) | chai();
    let mut actual = Vec::new();
    original.permute(&mut |v| actual.push(v));
    let expected = &[
        (ape() | bar()) | chai(),
        chai() | (ape() | bar()),
        (bar() | ape()) | chai(),
        chai() | (bar() | ape()),
        ape() | (bar() | chai()),
        (bar() | chai()) | ape(),
        ape() | (chai() | bar()),
        (chai() | bar()) | ape(),
    ];
    assert_eq!(
        &actual[..],
        expected,
        "\noriginal = {:#?}\nexpected = {:#?}\nactual = {:#?}",
        original,
        expected,
        &actual[..]
    );
}

#[test]
fn test_permute_5() {
    let original = (ape() | bar()) & chai();
    let mut actual = Vec::new();
    original.permute(&mut |v| actual.push(v));
    let expected = &[
        (ape() | bar()) & chai(),
        chai() & (ape() | bar()),
        (bar() | ape()) & chai(),
        chai() & (bar() | ape()),
    ];
    assert_eq!(
        &actual[..],
        expected,
        "\noriginal = {:#?}\nexpected = {:#?}\nactual = {:#?}",
        original,
        expected,
        &actual[..]
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
    let orig = chai() & bar() & ape();
    orig.permute(&mut |v| {
        assert_eq!(v.sort(), ape() & (bar() & chai()), "orig = {:?}", orig);
    })
}

#[test]
fn test_sort_3() {
    ((chai() & bar()) | ape()).permute(&mut |v| {
        assert_eq!(v.sort(), ape() | (bar() & chai()));
    })
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
