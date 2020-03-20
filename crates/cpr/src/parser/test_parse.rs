use super::*;

fn def(s: &str) -> Expr {
    Expr::Defined(s.to_string())
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
        def("FOO"),
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
        def("FOO") & def("BAR"),
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
        !def("FOO"),
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
        def("FOO") & !def("BAR"),
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
