use crate::types::*;

#[derive(Debug, Clone)]
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
