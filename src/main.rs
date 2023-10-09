use crust::lexer;
use std::{env::args, error::Error};

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        println!("Usage: {} <name>", args[0]);
        return Err("No file name provided".into());
    }
    let src_name = &args[1];

    let mut lexer = lexer::Lexer::new(src_name.to_string());
    lexer.tokenize()?;
    println!("{:#?}", lexer.tokens);
    Ok(())
}
