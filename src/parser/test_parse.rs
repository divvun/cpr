use super::*;

fn expr(s: &str) -> Expr {
    Expr::from(s)
}

fn parse(source: &str) -> ParsedUnit {
    ParsedUnit::parse(source.as_ref()).expect("unit should parse")
}

fn test_single(source: &str, expected: Expr) {
    let u = parse(source);
    let (_, actual) = u.dependencies.iter().next().unwrap();
    assert_eq!(*actual, expected);
}

#[test]
fn test_true() {
    test_single(
        "
#include <stdio.h>
        ",
        Expr::True,
    );
}

#[test]
fn test_one_ifdef() {
    test_single(
        "
#ifdef FOO
#include <stdio.h>
#endif
        ",
        expr("FOO"),
    );
}

#[test]
fn test_two_ifdefs() {
    test_single(
        "
#ifdef FOO
#ifdef BAR
#include <stdio.h>
#endif
#endif
        ",
        expr("BAR") & expr("FOO"),
    );
}

#[test]
fn test_else() {
    test_single(
        "
#ifdef FOO
// nothing
#else
#include <stdio.h>
#endif
        ",
        !expr("FOO"),
    )
}

#[test]
fn test_nested_else() {
    test_single(
        "
#ifdef FOO
#ifdef BAR
#else
#include <stdio.h>
#endif
#endif
        ",
        !expr("BAR") & expr("FOO"),
    )
}

// #[test]
// fn test_single_if_defined() {
//     test_single(
//         "
// #if defined(FOO)
// #include <stdio.h>
// #endif
//                 ",
//         expr("FOO"),
//     );
// }
