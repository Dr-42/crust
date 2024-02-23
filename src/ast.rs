use std::error::Error;

use crate::parser;

pub mod decldata;
pub mod nodes;
pub mod preprocess;
pub mod typecheck;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::{self, termcolor::StandardStream};
use lalrpop_util::lexer::Token;
use lalrpop_util::ParseError;

pub type Span = codespan::Span;

fn convert_error<'input>(
    file_id: usize,
    error: ParseError<usize, Token<'input>, &'static str>,
) -> Vec<Diagnostic<usize>>
where
{
    match error {
        ParseError::InvalidToken { location } => {
            vec![Diagnostic::error()
                .with_message("Invalid token")
                .with_labels(vec![
                    Label::primary(file_id, location..location).with_message("Unexpected token")
                ])]
        }
        ParseError::UnrecognizedEof { location, expected } => {
            let expected_str = format!("Expected one of: {}", expected.join(", "));
            vec![Diagnostic::error()
                .with_message("Unexpected end of file")
                .with_labels(vec![
                    Label::primary(file_id, location..location).with_message(expected_str)
                ])]
        }
        ParseError::UnrecognizedToken { token, expected } => {
            let expected_str = format!("Expected one of: {}", expected.join(", "));
            vec![Diagnostic::error()
                .with_message("Unrecognized token")
                .with_labels(vec![
                    Label::primary(file_id, token.0..token.2).with_message(expected_str)
                ])]
        }
        ParseError::ExtraToken { token } => {
            vec![Diagnostic::error()
                .with_message("Extra token")
                .with_labels(vec![
                    Label::primary(file_id, token.0..token.2).with_message("Unexpected token")
                ])]
        }
        ParseError::User { error } => vec![Diagnostic::error().with_message(error.to_string())],
    }
}

pub fn parse(text: &str) -> Result<Box<nodes::Program>, Box<dyn Error>> {
    let text = preprocess::preremove_comments(text.to_string());
    let mut files = SimpleFiles::new();
    let file_id = files.add("main", text.clone());
    let parser = parser::ProgramParser::new();
    let res = parser.parse(&text);
    if res.is_err() {
        let errors = convert_error(file_id, res.clone().err().unwrap());
        let writer = StandardStream::stderr(term::termcolor::ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();

        for error in errors {
            term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
        }
    }
    let res = res.map_err(|e| e.to_string())?;
    typecheck::typecheck(*res.clone(), file_id)?;
    Ok(res)
}
