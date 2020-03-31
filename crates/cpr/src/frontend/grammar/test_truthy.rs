use super::*;

#[test]
fn truth_tests() {
    fn test(b: bool, input: &str) {
        let expr = expr(input).unwrap();
        assert_eq!(b, expr.truthy())
    }
    fn truthy(input: &str) {
        test(true, input)
    }
    fn falsey(input: &str) {
        test(false, input)
    }

    truthy("1");
    truthy("1-2");
    truthy("124");
    truthy("!0");
    falsey("0");
    falsey("!1");
}
