// Expression tests

#[test]
fn test_binary_op() -> Result<(), Box<dyn std::error::Error>> {
    use super::nodes::*;
    use crate::parser::ProgramParser;
    let parser = ProgramParser::new();
    let expr = Box::new(Program {
        stmts: vec![Box::new(Stmt::Expr(Box::new(Expr::BinaryOp {
            lhs: Box::new(Expr::Numeric(1)),
            op: BinaryOp::Add,
            rhs: Box::new(Expr::BinaryOp {
                lhs: Box::new(Expr::Numeric(2)),
                op: BinaryOp::Mul,
                rhs: Box::new(Expr::Numeric(3)),
            }),
        })))],
    });
    let val = parser.parse("1 + 2 * 3;")?;
    assert_eq!(val, expr);
    Ok(())
}
