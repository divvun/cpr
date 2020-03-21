use super::super::expr::{
    BinaryOperator as BO,
    Expr::{self, *},
};
use lang_c::ast::{
    self, BinaryOperator as CBO, CallExpression, Constant, Expression as CE, IntegerBase,
};

pub trait IntoExpr {
    /// Convert to cpr expression
    fn into_expr(self) -> Expr;
}

impl IntoExpr for CE {
    fn into_expr(self) -> Expr {
        match self {
            CE::Constant(n) => match n.node {
                Constant::Integer(il) => match il.base {
                    // TODO: rename
                    IntegerBase::Hexademical /* sic. */ => Integer(i64::from_str_radix(il.number.as_ref(), 16).unwrap()),
                    _ => todo!(),
                },
                _ => unimplemented!(),
            },
            CE::Call(n) => {
                let CallExpression {
                    callee, arguments, ..
                } = n.node;
                Call(
                    Box::new(callee.node.into_expr()),
                    arguments.into_iter().map(|n| n.node.into_expr()).collect(),
                )
            }
            CE::Identifier(n) => Symbol(n.node.name),
            CE::BinaryOperator(n) => {
                let ast::BinaryOperatorExpression { operator, lhs, rhs } = n.node;

                Binary(
                    match operator.node {
                        CBO::Greater => BO::Greater,
                        CBO::GreaterOrEqual => BO::GreaterOrEqual,
                        CBO::Less => BO::Less,
                        CBO::LessOrEqual => BO::LessOrEqual,
                        CBO::Equals => BO::Equals,
                        CBO::NotEquals => BO::NotEquals,
                        CBO::BitwiseOr => BO::BitwiseOr,
                        _ => {
                            panic!(
                                "unsupported operator in preprocessor expression: {:?}",
                                operator.node
                            );
                        }
                    },
                    Box::new(lhs.node.into_expr()),
                    Box::new(rhs.node.into_expr()),
                )
            }
            _ => {
                log::debug!("Got CExpr: {:#?}", self);
                unimplemented!();
            }
        }
    }
}
