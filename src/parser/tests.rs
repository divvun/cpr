use super::*;

fn def(s: &str) -> Defined {
    Defined::from(s)
}

fn parse(source: &str) -> ParsedUnit {
    ParsedUnit::parse(source.into()).expect("unit should parse")
}

fn test_single(source: &str, expected: Defined) {
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
        Defined::True,
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
        def("BAR") & def("FOO"),
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
        !def("BAR") & def("FOO"),
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
//         def("FOO"),
//     );
// }
