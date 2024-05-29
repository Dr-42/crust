use std::error::Error;

use ast::{decldata::DeclData, tych::TychContext};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(pub parser);

fn main() -> Result<(), Box<dyn Error>> {
    for file in std::fs::read_dir("examples")? {
        let file = file?;
        let path = file.path();
        if path.is_file() {
            let text = std::fs::read_to_string(&path)?;
            let mut files = SimpleFiles::new();
            let file_name = path.file_name().unwrap().to_str().unwrap();
            let file_id = files.add(file_name, &text);
            let program = ast::parse(&text);
            match program {
                Ok(program) => {
                    // Output path outs/FILENAME.ast
                    if !std::path::Path::new("outs").exists() {
                        std::fs::create_dir("outs")?;
                    }
                    let mut out_path = std::path::PathBuf::from("outs");
                    out_path.push(path.file_name().unwrap());
                    std::fs::write(out_path.with_extension("ast"), format!("{:#?}", program))?;
                    println!("Parse Pass: {:?}", path);

                    // Type check
                    let mut tych_context = TychContext::new(file_id);
                    match tych_context.tych_program(file_id, *program) {
                        Ok(_) => {
                            println!("Type Check Pass: {:?}", path);
                        }
                        Err(e) => {
                            eprintln!("Error type checking file: {:?}", path);
                            let diag = e;
                            let writer = StandardStream::stderr(ColorChoice::Always);
                            let config = codespan_reporting::term::Config::default();

                            term::emit(&mut writer.lock(), &config, &files, &diag)?;
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Error parsing file: {:?}", path);
                    let diag = Diagnostic::error()
                        .with_message(e.message)
                        .with_labels(vec![Label::primary(file_id, e.span).with_message("here")]);

                    let writer = StandardStream::stderr(ColorChoice::Always);
                    let config = codespan_reporting::term::Config::default();

                    term::emit(&mut writer.lock(), &config, &files, &diag)?;
                }
            }
        }
    }
    Ok(())
}
