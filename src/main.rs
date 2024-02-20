use std::error::Error;

use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(pub parser);

const TEXT_TO_PARSE: &str = r#"
d : i32 = a[3][x + 1] + b[3] * 2;
f : i32;
mamba : snake<str, i32, ptr** animal> = snake_new::<str, i32, ptr** animal>("Rattle", 5, parent);
mozart : artist<musician> = d.loper.mamba;
mozart.play("Symphony No. 40 in G minor, K. 550");
struct Vec3 <T:Num + Copy> {
    x: T,
    y: T,
    z: T,
}
"#;

fn main() -> Result<(), Box<dyn Error>> {
    let parser = parser::ProgramParser::new();
    let tree = parser.parse(TEXT_TO_PARSE)?;
    println!("{:#?}", tree);
    Ok(())
}
