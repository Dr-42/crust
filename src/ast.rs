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

#[derive(Debug)]
pub struct MyparseError {
    pub span: Span,
    pub token: Option<String>,
    pub message: String,
}

pub fn parse(text: &str) -> Result<Box<nodes::Program>, MyparseError> {
    let text = preremove_comments(text.to_string());
    let parser = parser::ProgramParser::new();
    let res = parser.parse(&text);
    match res {
        Ok(program) => Ok(program),
        Err(e) => {
            let (span, token) = match e {
                lalrpop_util::ParseError::InvalidToken { location } => {
                    (Span::new(location as u32, location as u32 + 1), None)
                }
                lalrpop_util::ParseError::UnrecognizedToken { token, .. } => (
                    Span::new(token.0 as u32, token.2 as u32),
                    Some(token.1.to_string()),
                ),
                _ => (Span::new(0, 0), None),
            };
            Err(MyparseError {
                span,
                token,
                message: "Parse error".to_string(),
            })
        }
    }
}
