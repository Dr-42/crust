pub mod lexer_data;
pub mod lexer_maps;
pub mod token_types;
use lexer_maps::LexerMaps;
pub use token_types::{OperatorType, PreprocessorType, PunctuatorType, TokenType};

use std::{error::Error, io};

use self::token_types::KeywordType;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u64,
    pub column: u64,
}

pub struct Lexer {
    pub filename: String,
    pub line: usize,
    pub column: usize,
    pub index: usize,
    pub lexer_data: lexer_data::LexerData,
    pub tokens: Vec<Token>,
    fnc_paren_level: usize,
    fnc_encountered: bool,
}

impl Lexer {
    pub fn new(filename: String) -> Self {
        Lexer {
            filename,
            line: 1,
            column: 1,
            index: 0,
            lexer_data: lexer_data::LexerData::new(),
            tokens: Vec::new(),
            fnc_paren_level: 0,
            fnc_encountered: false,
        }
    }

    pub fn trim_comments(&mut self) {
        self.tokens.retain(|token| match token.token_type {
            TokenType::Comment(_) => false,
            _ => true,
        });
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

                    let mut data = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == '\n' {
                            break;
                        } else {
                            data.push(chars.next().unwrap());
                        }
                    }

                    if let Some(preprocess) =
                        lexer_map.preprocessor_directives.get(preprocessor.as_str())
                    {
                        let token_length = preprocessor.len() + data.len() + 1;
                        self.tokens.push(Token {
                            token_type: TokenType::Preprocessor {
                                typ: preprocess.clone(),
                                data,
                            },
                            line: self.line as u64,
                            column: self.column as u64,
                        });
                        self.column += token_length;
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

                    let token_length = identifier.len();

                    if let Some(keyword) = lexer_map.keywords.get(identifier.as_str()) {
                        if keyword == &KeywordType::Fnc {
                            self.fnc_encountered = true;
                        }
                        self.tokens.push(Token {
                            token_type: TokenType::Keyword(keyword.clone()),
                            line: self.line as u64,
                            column: self.column as u64,
                        });
                    } else if let Some(data_type) = lexer_map.datatypes.get(identifier.as_str()) {
                        self.tokens.push(Token {
                            token_type: TokenType::DataType(data_type.clone()),
                            line: self.line as u64,
                            column: self.column as u64,
                        });
                    } else {
                        // If the identifier is a new data type defined by the user
                        // New data types are created from structs, unions, enums.

                        // Parse generic data types too
                        match self.tokens.last().unwrap().token_type.clone() {
                            TokenType::Punctuator(PunctuatorType::Tick) => {
                                self.lexer_data
                                    .add_generic_data_type(identifier.to_string());
                                self.tokens.push(Token {
                                    token_type: TokenType::DataType(
                                        token_types::DataType::Generic(identifier.to_string()),
                                    ),
                                    line: self.line as u64,
                                    column: self.column as u64,
                                });
                                self.column += token_length;
                                while let Some(char) = chars.next() {
                                    if char == '`' {
                                        self.column += 1;
                                        self.tokens.push(Token {
                                            token_type: TokenType::Punctuator(PunctuatorType::Tick),
                                            line: self.line as u64,
                                            column: self.column as u64,
                                        });
                                        break;
                                    } else if char == ' ' {
                                        self.column += 1;
                                        continue;
                                    } else if char == '\n' {
                                        self.line += 1;
                                        self.column = 1;
                                    } else if char == ',' {
                                        self.tokens.push(Token {
                                            token_type: TokenType::Punctuator(
                                                PunctuatorType::Comma,
                                            ),
                                            line: self.line as u64,
                                            column: self.column as u64,
                                        });
                                        self.column += 1;
                                    } else if char.is_alphabetic() {
                                        let mut identifier = String::new();
                                        identifier.push(char);
                                        while let Some(&ch) = chars.peek() {
                                            if ch.is_alphanumeric() || ch == '_' {
                                                identifier.push(chars.next().unwrap());
                                            } else {
                                                break;
                                            }
                                        }
                                        let iden_len = identifier.len();
                                        self.lexer_data
                                            .add_generic_data_type(identifier.to_string());
                                        self.tokens.push(Token {
                                            token_type: TokenType::DataType(
                                                token_types::DataType::Generic(identifier),
                                            ),
                                            line: self.line as u64,
                                            column: self.column as u64,
                                        });
                                        self.column += iden_len;
                                    } else {
                                        return Err(Box::new(io::Error::new(
                                            io::ErrorKind::InvalidData,
                                            format!(
                                                "Invalid generic data type in {}:{}:{}",
                                                self.filename, self.line, self.column
                                            ),
                                        )));
                                    }
                                }
                                continue;
                            }
                            TokenType::Keyword(
                                KeywordType::Struct | KeywordType::Union | KeywordType::Enum,
                            ) => {
                                self.lexer_data.add_user_data_type(identifier.to_string());
                                self.tokens.push(Token {
                                    token_type: TokenType::DataType(
                                        token_types::DataType::UserDef(identifier),
                                    ),
                                    line: self.line as u64,
                                    column: self.column as u64,
                                });
                            }
                            _ => {
                                if self.lexer_data.get_user_data_types().contains(&identifier) {
                                    self.tokens.push(Token {
                                        token_type: TokenType::DataType(
                                            token_types::DataType::UserDef(identifier),
                                        ),
                                        line: self.line as u64,
                                        column: self.column as u64,
                                    });
                                } else if self
                                    .lexer_data
                                    .get_generic_data_types()
                                    .contains(&identifier)
                                {
                                    self.tokens.push(Token {
                                        token_type: TokenType::DataType(
                                            token_types::DataType::Generic(identifier),
                                        ),
                                        line: self.line as u64,
                                        column: self.column as u64,
                                    });
                                } else {
                                    self.tokens.push(Token {
                                        token_type: TokenType::Identifier(identifier),
                                        line: self.line as u64,
                                        column: self.column as u64,
                                    });
                                }
                            }
                        }
                    }
                    self.column += token_length;
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

                    let token_length = number.len();
                    if encountered_dot {
                        self.tokens.push(Token {
                            token_type: TokenType::FloatNum(number),
                            line: self.line as u64,
                            column: self.column as u64,
                        });
                    } else {
                        self.tokens.push(Token {
                            token_type: TokenType::IntNum(number),
                            line: self.line as u64,
                            column: self.column as u64,
                        });
                    }
                    self.column += token_length;
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

                    let token_length = string.len() + 2;
                    self.tokens.push(Token {
                        token_type: TokenType::StringLiteral(string),
                        line: self.line as u64,
                        column: self.column as u64,
                    });
                    self.column += token_length;
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

                    let token_length = string.len() + 2;
                    self.tokens.push(Token {
                        token_type: TokenType::CharLiteral(string),
                        line: self.line as u64,
                        column: self.column as u64,
                    });
                    self.column += token_length;
                }

                // Punctuators
                '(' | ')' | '{' | '}' | '[' | ']' | ';' | ':' | ',' | '.' | '`' => {
                    let punctuator = lexer_map.punctuators.get(&ch).unwrap();
                    let token_length = 1;
                    self.tokens.push(Token {
                        token_type: TokenType::Punctuator(punctuator.clone()),
                        line: self.line as u64,
                        column: self.column as u64,
                    });
                    if self.fnc_encountered {
                        if punctuator == &PunctuatorType::LeftBrace {
                            self.fnc_paren_level += 1;
                        } else if punctuator == &PunctuatorType::RightBrace {
                            self.fnc_paren_level -= 1;
                            if self.fnc_paren_level == 0 {
                                self.lexer_data.clear_generic_data_types();
                                self.fnc_encountered = false;
                            }
                        }
                        if (punctuator == &PunctuatorType::Semicolon) && (self.fnc_paren_level == 0)
                        {
                            self.lexer_data.clear_generic_data_types();
                            self.fnc_encountered = false;
                        }
                    }
                    self.column += token_length;
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

                    let token_length = operator.len();
                    if let Some(op) = lexer_map.operators.get(operator.as_str()) {
                        self.tokens.push(Token {
                            token_type: TokenType::Operator(op.clone()),
                            line: self.line as u64,
                            column: self.column as u64,
                        });
                    } else {
                        return Err(Box::new(io::Error::new(
                            io::ErrorKind::InvalidData,
                            "Invalid operator",
                        )));
                    }
                    self.column += token_length;
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

                    let token_length = comment.len();
                    self.tokens.push(Token {
                        token_type: TokenType::Comment(comment),
                        line: self.line as u64,
                        column: self.column as u64,
                    });
                    self.column += token_length;
                }
                _ => {
                    self.column += 1;
                }
            }
        }

        Ok(())
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        if self.index < self.tokens.len() {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }
}
