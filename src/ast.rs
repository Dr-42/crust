use std::error::Error;

use crate::parser;

pub mod decldata;
pub mod nodes;

pub type Span = codespan::Span;

pub fn preremove_comments(text: String) -> String {
    let mut result = String::new();
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '/' {
            if let Some(&'/') = chars.peek() {
                chars.next();
                for c in chars.by_ref() {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            } else if let Some(&'*') = chars.peek() {
                chars.next();
                while let Some(c) = chars.next() {
                    if c == '*' {
                        if let Some(&'/') = chars.peek() {
                            chars.next();
                            break;
                        }
                    }
                }
                continue;
            }
        }
        result.push(c);
    }
    result
}

pub fn parse(text: &str) -> Result<Box<nodes::Program>, Box<dyn Error>> {
    let text = preremove_comments(text.to_string());
    let parser = parser::ProgramParser::new();
    let res = parser.parse(&text).map_err(|e| format!("{:?}", e))?;
    Ok(res)
}
