use std::error::Error;

use crate::parser;

pub mod decldata;
pub mod nodes;
pub mod preprocess;

pub type Span = codespan::Span;

pub fn parse(text: &str) -> Result<Box<nodes::Program>, Box<dyn Error>> {
    let text = preprocess::preremove_comments(text.to_string());
    let parser = parser::ProgramParser::new();
    let res = parser.parse(&text).map_err(|e| format!("{:?}", e))?;
    Ok(res)
}
