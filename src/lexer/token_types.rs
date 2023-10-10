use std::fmt::Display;

pub use crate::types::{DataType, KeywordType, OperatorType, PreprocessorType, PunctuatorType};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Keyword(KeywordType),
    DataType(DataType),
    Identifier(String),
    IntNum(String),
    FloatNum(String),
    Operator(OperatorType),
    Punctuator(PunctuatorType),
    StringLiteral(String),
    CharLiteral(String),
    Preprocessor { typ: PreprocessorType, data: String },
    Comment(String),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Keyword(keyword_type) => write!(f, "{}", keyword_type),
            TokenType::DataType(data_type) => write!(f, "{}", data_type),
            TokenType::Identifier(name) => write!(f, "{}", name),
            TokenType::IntNum(num) => write!(f, "{}", num),
            TokenType::FloatNum(num) => write!(f, "{}", num),
            TokenType::Operator(operator_type) => write!(f, "{}", operator_type),
            TokenType::Punctuator(punctuator_type) => write!(f, "{}", punctuator_type),
            TokenType::StringLiteral(string) => write!(f, "{}", string),
            TokenType::CharLiteral(char) => write!(f, "{}", char),
            TokenType::Preprocessor { typ, data } => write!(f, "{} {}", typ, data),
            TokenType::Comment(comment) => write!(f, "{}", comment),
        }
    }
}
