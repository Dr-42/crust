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
