use std::error::Error;

use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(pub parser);

fn main() -> Result<(), Box<dyn Error>> {
    let parser = parser::ProgramParser::new();
    let tree = parser.parse("a + b *  --3 ^ 4 + 5 + -a;")?;
    println!("{:#?}", tree);
    Ok(())
}
