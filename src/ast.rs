use crate::parser;

pub mod decldata;
pub mod nodes;
pub mod tych;

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

#[derive(Debug)]
pub struct MyparseError {
    pub span: Span,
    pub token: Option<String>,
    pub message: String,
}

pub fn parse(text: &str) -> Result<Box<nodes::Program>, MyparseError> {
    //let text = preremove_comments(text.to_string());
    let parser = parser::ProgramParser::new();
    let res = parser.parse(text);
    match res {
        Ok(program) => Ok(program),
        Err(e) => {
            let error = match e {
                lalrpop_util::ParseError::InvalidToken { location } => MyparseError {
                    span: Span::new(location as u32, location as u32),
                    token: None,
                    message: "Invalid token".to_string(),
                },
                lalrpop_util::ParseError::UnrecognizedToken { token, .. } => MyparseError {
                    span: Span::new(token.0 as u32, token.2 as u32),
                    token: Some(token.1.to_string()),
                    message: "Unrecognized token".to_string(),
                },
                _ => MyparseError {
                    span: Span::new(0, 0),
                    token: None,
                    message: "Unknown error".to_string(),
                },
            };
            Err(error)
        }
    }
}
