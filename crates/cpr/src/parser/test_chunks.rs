use super::*;
use indoc::indoc;
use std::error::Error;

fn def(s: &str) -> Expr {
    Expr::Defined(s.to_string())
}

fn parse(source: &str) -> ParsedUnit {
    ParsedUnit::parse(source.as_ref()).expect("unit should parse")
}

fn chunks(source: &str, deps: &[&ChunkedUnit]) -> Vec<Chunk> {
    parse(source)
        .chunkify(deps, &Context::new())
        .unwrap()
        .chunks
}

fn test(source: &str, expected_chunks: &[(Expr, &str)]) {
    let actual_chunks = chunks(source, &[]);
    assert_chunks(&actual_chunks[..], expected_chunks);
}

fn assert_chunks(actual_chunks: &[Chunk], expected_chunks: &[(Expr, &str)]) {
    assert_eq!(actual_chunks.len(), expected_chunks.len());
    for (i, (actual, expected)) in actual_chunks.iter().zip(expected_chunks.iter()).enumerate() {
        assert_eq!(actual.expr, expected.0, "expr for chunk #{}", i);
        assert_eq!(
            actual.source,
            expected.1.trim().into(),
            "source for chunk #{}",
            i
        );
    }
}

#[test]
fn single_line_comment() {
    test(
        indoc!(
            "
            // single-line comment
            int foo();
            "
        ),
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn single_line_comment_continued() {
    test(
        indoc!(
            "
            // single-line comment, \\
            continued
            int foo();
            "
        ),
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn multi_line_comment_1_line() {
    test(
        indoc!(
            "
            /* classic multi-line comment */
            int foo();
            "
        ),
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn multi_line_comment_2_lines() {
    test(
        indoc!(
            "
            /* classic multi-line comment
            * but on multiple lines */
            int foo();
            "
        ),
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn multi_line_comment_nested() {
    test(
        indoc!(
            "
            int/* boop */foo();
            "
        ),
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn string_literal_1() {
    test(
        indoc!(
            "
            char *c = \"hello /* world */\";
            "
        ),
        &[(Expr::True, "char *c = \"hello /* world */\";")],
    );
}

#[test]
fn string_literal_2() {
    test(
        indoc!(
            "
            char *c = \"hello // world\";
            "
        ),
        &[(Expr::True, "char *c = \"hello // world\";")],
    );
}

#[test]
fn single_atom_strands() {
    test(
        indoc!(
            "
            #ifdef FOO
            int foo();
            #endif

            #ifdef BAR
            int bar();
            #endif
            "
        ),
        &[(def("FOO"), "int foo();"), (def("BAR"), "int bar();")],
    )
}

#[test]
fn nested_ifdefs() {
    test(
        indoc!(
            "
            #ifdef FOO
            int foo();

            #ifdef BAR
            int foobar();
            #endif // BAR

            #endif // FOO
            "
        ),
        &[
            (def("FOO"), "int foo();"),
            (def("FOO") & def("BAR"), "int foobar();"),
        ],
    );
}

#[test]
fn chunks_gated_struct_field() {
    test(
        indoc!(
            "
            struct foo {
                int lawful;
            #ifdef EVIL
                int evil;
            #endif // EVIL
            };
            "
        ),
        &[
            (
                def("EVIL"),
                indoc!(
                    "
                    struct foo {
                    int lawful;
                    int evil;
                    };
                    "
                ),
            ),
            (
                !def("EVIL"),
                indoc!(
                    "
                    struct foo {
                    int lawful;
                    };
                    "
                ),
            ),
        ],
    )
}

#[test]
fn chunks_gated_struct_field_ifelse() {
    test(
        indoc!(
            "
            struct foo {
                int lawful;
            #ifdef EVIL
                int evil;
            #else 
                int good;
            #endif
            };
            "
        ),
        &[
            (
                def("EVIL"),
                indoc!(
                    "
                    struct foo {
                    int lawful;
                    int evil;
                    };
                    "
                ),
            ),
            (
                !def("EVIL"),
                indoc!(
                    "
                    struct foo {
                    int lawful;
                    int good;
                    };
                    "
                ),
            ),
        ],
    )
}

#[test]
fn gated_struct_close_ifelse() {
    test(
        indoc!(
            "
            struct foo {
                int lawful;
            #ifdef EVIL
            };
            #else 
            };
            #endif
            "
        ),
        &[
            (
                def("EVIL"),
                indoc!(
                    "
                    struct foo {
                    int lawful;
                    };
                    "
                ),
            ),
            (
                !def("EVIL"),
                indoc!(
                    "
                    struct foo {
                    int lawful;
                    };
                    "
                ),
            ),
        ],
    )
}

#[test]
fn gated_struct_close_convoluted() {
    test(
        indoc!(
            "
            struct foo {
                int foo;
            #ifdef BAR
                struct bar {
                    int bar;
            #endif // BAR
            #ifdef BAZ
                } nested;
            #endif // BAZ
            };
            "
        ),
        &[
            (
                def("BAR") & def("BAZ"),
                indoc!(
                    "
                    struct foo {
                    int foo;
                    struct bar {
                    int bar;
                    } nested;
                    };
                    "
                ),
            ),
            (
                !def("BAR") & !def("BAZ"),
                indoc!(
                    "
                    struct foo {
                    int foo;
                    };
                    "
                ),
            ),
        ],
    )
}

#[test]
fn typedef_in_different_chunk() {
    test(
        indoc!(
            "
            typedef struct foo {
                int a;
            } foo;

            #ifdef WOOPS
            int noop(void);
            #endif

            int bar(foo *f);
            "
        ),
        &[
            (
                Expr::True,
                indoc!(
                    "
                    typedef struct foo {
                    int a;
                    } foo;
                    "
                ),
            ),
            (
                def("WOOPS"),
                indoc!(
                    "
                    int noop(void);
                    "
                ),
            ),
            (
                Expr::True,
                indoc!(
                    "
                    int bar(foo *f);
                    "
                ),
            ),
        ],
    )
}

#[test]
fn typedef_in_different_unit() -> Result<(), Box<dyn Error>> {
    let dep_source = indoc!(
        "
        typedef struct foo {
            int bar;
            int baz;
        } foo;
        "
    );
    let root_source = indoc!(
        "
        void set_foo(foo *arg);
        foo *get_foo(void);
        "
    );
    let dep = parse(dep_source);
    let root = parse(root_source);

    let dep_unit = dep.chunkify(&[], &Context::new())?;
    let root_unit = root.chunkify(&[&dep_unit], &Context::new())?;
    assert_chunks(
        &root_unit.chunks[..],
        &[(
            Expr::True,
            indoc!(
                "
                void set_foo(foo *arg);
                foo *get_foo(void);
                "
            ),
        )],
    );

    Ok(())
}

#[test]
fn cplusplus() {
    test(
        indoc!(
            r#"
            #ifdef __cplusplus
            extern "C" {
            #endif

            int foo(int bar);

            #ifdef __cplusplus
            }
            #endif
            "#
        ),
        &[(
            Expr::True,
            indoc!(
                "
                int foo(int bar);
                "
            ),
        )],
    )
}

#[test]
fn include_guard() {
    test(
        indoc!(
            "
            #ifndef ROOT_H
            #define ROOT_H

            int foobar(void);

            #endif
            "
        ),
        &[(
            !def("ROOT_H"),
            indoc!(
                "
                int foobar(void);
                "
            ),
        )],
    )
}
