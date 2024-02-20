use std::error::Error;

use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(pub parser);

/*
fn main() -> Result<(), Box<dyn Error>> {
    let file = std::env::args().nth(1).expect("No file provided");
    let text = std::fs::read_to_string(&file)?;
    let text = ast::preremove_comments(&text);
    let program = parser::ProgramParser::new().parse(&text);
    match program {
        Ok(program) => {
            println!("{:#?}", program);
        }
        Err(e) => {
            eprintln!("Error parsing file: {:?}", file);
            eprintln!("{:?}", e);
        }
    }
    Ok(())
}
*/

fn main() -> Result<(), Box<dyn Error>> {
    for file in std::fs::read_dir("examples")? {
        let file = file?;
        let path = file.path();
        if path.is_file() {
            let text = std::fs::read_to_string(&path)?;
            let text = ast::preremove_comments(&text);
            let program = parser::ProgramParser::new().parse(&text);
            match program {
                Ok(program) => {
                    // Output path outs/FILENAME.ast
                    if !std::path::Path::new("outs").exists() {
                        std::fs::create_dir("outs")?;
                    }
                    let mut out_path = std::path::PathBuf::from("outs");
                    out_path.push(path.file_name().unwrap());
                    std::fs::write(out_path.with_extension("ast"), format!("{:#?}", program))?;
                }
                Err(e) => {
                    eprintln!("Error parsing file: {:?}", path);
                    eprintln!("{:?}", e);
                }
            }
        }
    }
    Ok(())
}
