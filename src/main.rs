use std::error::Error;

use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(pub simple_expr);

fn main() -> Result<(), Box<dyn Error>> {
    let parser = simple_expr::ExprParser::new();
    let res = parser.parse("32 * 24 + 68 * (70 + 60 - 32) * (38 + (20 -9))")?;
    println!("{:#?}", res);
    Ok(())
}
