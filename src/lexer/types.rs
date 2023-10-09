#[derive(Debug, Clone)]
pub enum KeywordType {
    Fn,
    Const,
    Struct,
    Enum,
    Union,
    Break,
    Continue,
    If,
    Elif,
    Else,
    Do,
    While,
    For,
    Ret,
    Switch,
    Case,
    Default,
    As,
    Is,
    Static,
    Extern,
    True,
    False,
    Null,
    Ptr,
}

#[derive(Debug, Clone)]
pub enum DataType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Str,
    Chr,
    Bln,
    Void,
}

#[derive(Debug, Clone)]
pub enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    ShiftLeft,
    ShiftRight,
    Ternary,
    Increment,
    Decrement,
}

#[derive(Debug, Clone)]
pub enum PreprocessorType {
    Include,
    Define,
    If,
    Elif,
    Else,
    Endif,
    Undef,
    Error,
    Pragma,
}

#[derive(Debug, Clone)]
pub enum CommentType {
    SingleLine,
    MultiLine,
    Doc,
}

#[derive(Debug, Clone)]
pub enum PunctuatorType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Tick,
    Semicolon,
    Colon,
    Dot,
}

#[derive(Debug)]
pub enum TokenType {
    Keyword(KeywordType),
    DataType(DataType),
    Identifier(String),
    IntNum(String),
    FloatNum(String),
    Operator {
        operator_type: OperatorType,
        is_unary: bool,
    },
    Punctuator(PunctuatorType),
    StringLiteral(String),
    CharLiteral(String),
    Preprocessor(PreprocessorType),
    Comment(CommentType),
}
