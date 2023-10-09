pub mod lexer_maps;
pub mod types;
use lexer_maps::LexerMaps;
use types::TokenType;

use std::{error::Error, io};

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
    pub column: u32,
}

pub struct Lexer {
    pub filename: String,
    pub line: usize,
    pub column: usize,
    pub index: usize,
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(filename: String) -> Self {
        Lexer {
            filename,
            line: 1,
            column: 1,
            index: 0,
            tokens: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<(), Box<dyn Error>> {
        let contents = std::fs::read_to_string(&self.filename)?;
        let mut chars = contents.chars().peekable();
        let lexer_map = LexerMaps::default();

        while let Some(ch) = chars.next() {
            match ch {
                // Whitespace
                ' ' | '\t' => {
                    self.column += 1;
                }
                // Newline
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                }
                // Preprocessor
                '#' => {
                    let mut preprocessor = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() {
                            preprocessor.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    if let Some(preprocess) =
                        lexer_map.preprocessor_directives.get(preprocessor.as_str())
                    {
                        self.tokens.push(Token {
                            token_type: TokenType::Preprocessor(preprocess.clone()),
                            line: self.line as u32,
                            column: self.column as u32,
                        });
                    }
                }

                // Identifier & keywords & data types
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut identifier = String::new();
                    identifier.push(ch);
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' {
                            identifier.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    if let Some(keyword) = lexer_map.keywords.get(identifier.as_str()) {
                        self.tokens.push(Token {
                            token_type: TokenType::Keyword(keyword.clone()),
                            line: self.line as u32,
                            column: self.column as u32,
                        });
                    } else if let Some(data_type) = lexer_map.datatypes.get(identifier.as_str()) {
                        self.tokens.push(Token {
                            token_type: TokenType::DataType(data_type.clone()),
                            line: self.line as u32,
                            column: self.column as u32,
                        });
                    } else {
                        self.tokens.push(Token {
                            token_type: TokenType::Identifier(identifier),
                            line: self.line as u32,
                            column: self.column as u32,
                        });
                    }
                }

                // Integer & float
                '0'..='9' => {
                    let mut number = String::new();
                    number.push(ch);
                    let mut encountered_dot = false;
                    while let Some(&ch) = chars.peek() {
                        if ch.is_numeric() || ch == '.' {
                            if ch == '.' {
                                if encountered_dot {
                                    return Err(Box::new(io::Error::new(
                                        io::ErrorKind::InvalidData,
                                        "Invalid float number",
                                    )));
                                } else {
                                    encountered_dot = true;
                                }
                            }
                            number.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    if encountered_dot {
                        self.tokens.push(Token {
                            token_type: TokenType::FloatNum(number),
                            line: self.line as u32,
                            column: self.column as u32,
                        });
                    } else {
                        self.tokens.push(Token {
                            token_type: TokenType::IntNum(number),
                            line: self.line as u32,
                            column: self.column as u32,
                        });
                    }
                }

                // String literal
                '"' => {
                    let mut string = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == '"' {
                            chars.next();
                            break;
                        } else {
                            string.push(chars.next().unwrap());
                        }
                    }

                    self.tokens.push(Token {
                        token_type: TokenType::StringLiteral(string),
                        line: self.line as u32,
                        column: self.column as u32,
                    });
                }

                // Char literal
                '\'' => {
                    let mut string = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == '\'' {
                            chars.next();
                            break;
                        } else {
                            string.push(chars.next().unwrap());
                        }
                    }

                    self.tokens.push(Token {
                        token_type: TokenType::CharLiteral(string),
                        line: self.line as u32,
                        column: self.column as u32,
                    });
                }

                // Punctuators
                '(' | ')' | '{' | '}' | '[' | ']' | ';' | ':' | '.' | '`' => {
                    let punctuator = lexer_map.punctuators.get(&ch).unwrap();
                    self.tokens.push(Token {
                        token_type: TokenType::Punctuator(punctuator.clone()),
                        line: self.line as u32,
                        column: self.column as u32,
                    });
                }

                // Operators
                '+' | '-' | '*' | '/' | '%' | '^' | '&' | '|' | '=' | '!' | '<' | '>' => {
                    let mut operator = String::new();
                    operator.push(ch);
                    while let Some(&ch) = chars.peek() {
                        if ch == '='
                            || ch == '>'
                            || ch == '<'
                            || ch == '&'
                            || ch == '|'
                            || ch == '+'
                            || ch == '-'
                        {
                            operator.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    if let Some(op) = lexer_map.operators.get(operator.as_str()) {
                        self.tokens.push(Token {
                            token_type: TokenType::Operator(op.clone()),
                            line: self.line as u32,
                            column: self.column as u32,
                        });
                    } else {
                        return Err(Box::new(io::Error::new(
                            io::ErrorKind::InvalidData,
                            "Invalid operator",
                        )));
                    }
                }
                // Comments
                '@' => {
                    let mut comment = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == '\n' {
                            break;
                        } else {
                            comment.push(chars.next().unwrap());
                        }
                    }

                    self.tokens.push(Token {
                        token_type: TokenType::Comment(comment),
                        line: self.line as u32,
                        column: self.column as u32,
                    });
                }
                _ => (),
            }
        }

        Ok(())
    }
}
