use super::*;

fn expr(s: &str) -> Expr {
    Expr::from(s)
}

fn parse(source: &str) -> ParsedUnit {
    ParsedUnit::parse(source.as_ref()).expect("unit should parse")
}

fn chunks(source: &str) -> Vec<Chunk> {
    parse(source).chunks().unwrap()
}

fn test(source: &str, expected_chunks: &[(Expr, &str)]) {
    let actual_chunks = chunks(source);
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
        "
// single-line comment
int foo();
    ",
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn single_line_comment_continued() {
    test(
        "
// single-line comment, \\
continued
int foo();
    ",
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn multi_line_comment_1_line() {
    test(
        "
/* classic multi-line comment */
int foo();
    ",
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn multi_line_comment_2_lines() {
    test(
        "
/* classic multi-line comment
 * but on multiple lines */
int foo();
    ",
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn multi_line_comment_nested() {
    test(
        "
int/* boop */foo();
    ",
        &[(Expr::True, "int foo();")],
    );
}

#[test]
fn string_literal_1() {
    test(
        "
char *c = \"hello /* world */\";
    ",
        &[(Expr::True, "char *c = \"hello /* world */\";")],
    );
}

#[test]
fn string_literal_2() {
    test(
        "
char *c = \"hello // world\";
    ",
        &[(Expr::True, "char *c = \"hello // world\";")],
    );
}

#[test]
fn chunks_basic() {
    test(
        "
#ifdef FOO
int foo();
#endif

#ifdef BAR
int bar();
#endif
    ",
        &[(expr("FOO"), "int foo();"), (expr("BAR"), "int bar();")],
    )
}

#[test]
fn chunks_nested() {
    test(
        "
#ifdef FOO
int foo();
#ifdef BAR
int foobar();
#endif // BAR
#endif // FOO
        ",
        &[
            (expr("FOO"), "int foo();"),
            (expr("FOO") & expr("BAR"), "int foobar();"),
        ],
    );
}

#[test]
fn chunks_evil() {
    test(
        "
struct foo {
    int lawful;
#ifdef EVIL
    int evil;
#endif // EVIL
}
        ",
        &[],
    )
}
