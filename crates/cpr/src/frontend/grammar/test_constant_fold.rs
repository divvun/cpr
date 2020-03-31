use super::*;
use BinaryOperator as BO;

fn i(i: i64) -> Expr {
    Expr::Integer(i)
}

#[test]
fn naive() {
    assert_eq!(BO::Add.build(i(5), i(2)).constant_fold(), i(7),);
    assert_eq!(BO::Subtract.build(i(3), i(4)).constant_fold(), i(-1),);
    assert_eq!(
        BO::Add
            .build(i(2), BO::Multiply.build(i(3), i(6)))
            .constant_fold(),
        i(20),
    );

    assert_eq!(BO::Less.build(i(3), i(6)).constant_fold(), i(1));
}
