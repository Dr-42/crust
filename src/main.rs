use std::error::Error;

use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(pub parser);

const TEXT_TO_PARSE: &str = r"
a : i32 = 91;
b : i32 = 2;
c : i64 = a + b *  --3 ^ 4 + 5 + -a;
d : f64;
mamba : snake<str, i32, ptr** animal> = black;
";

fn main() -> Result<(), Box<dyn Error>> {
    let parser = parser::ProgramParser::new();
    let tree = parser.parse(TEXT_TO_PARSE)?;
    println!("{:#?}", tree);
    Ok(())
}
