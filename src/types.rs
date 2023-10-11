use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum KeywordType {
    Fnc,
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

#[derive(Debug, Clone, PartialEq)]
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
    UserDef(String),
    Generic(String),
    Ptr(Box<DataType>),
    FnPtr {
        return_type: Box<DataType>,
        args: Vec<DataType>,
    },
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum PreprocessorType {
    Include,
    Define,
    If,
    Elif,
    Else,
    Endif,
    Undef,
    Pragma,
}

#[derive(Debug, Clone, PartialEq)]
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
    Comma,
    Dot,
}

impl Display for KeywordType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeywordType::As => write!(f, "keyword: as"),
            KeywordType::Break => write!(f, "keyword: break"),
            KeywordType::Case => write!(f, "keyword: case"),
            KeywordType::Const => write!(f, "keyword: const"),
            KeywordType::Continue => write!(f, "keyword: continue"),
            KeywordType::Default => write!(f, "keyword: default"),
            KeywordType::Do => write!(f, "keyword: do"),
            KeywordType::Elif => write!(f, "keyword: elif"),
            KeywordType::Else => write!(f, "keyword: else"),
            KeywordType::Enum => write!(f, "keyword: enum"),
            KeywordType::Extern => write!(f, "keyword: extern"),
            KeywordType::False => write!(f, "keyword: false"),
            KeywordType::Fnc => write!(f, "keyword: fnc"),
            KeywordType::For => write!(f, "keyword: for"),
            KeywordType::If => write!(f, "keyword: if"),
            KeywordType::Is => write!(f, "keyword: is"),
            KeywordType::Null => write!(f, "keyword: null"),
            KeywordType::Ptr => write!(f, "keyword: ptr"),
            KeywordType::Ret => write!(f, "keyword: ret"),
            KeywordType::Static => write!(f, "keyword: static"),
            KeywordType::Struct => write!(f, "keyword: struct"),
            KeywordType::Switch => write!(f, "keyword: switch"),
            KeywordType::True => write!(f, "keyword: true"),
            KeywordType::Union => write!(f, "keyword: union"),
            KeywordType::While => write!(f, "keyword: while"),
        }
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DataType::*;
        match self {
            I8 => write!(f, "datatype: '{}'", "I8"),
            I16 => write!(f, "datatype: '{}'", "I16"),
            I32 => write!(f, "datatype: '{}'", "I32"),
            I64 => write!(f, "datatype: '{}'", "I64"),
            U8 => write!(f, "datatype: '{}'", "U8"),
            U16 => write!(f, "datatype: '{}'", "U16"),
            U32 => write!(f, "datatype: '{}'", "U32"),
            U64 => write!(f, "datatype: '{}'", "U64"),
            F32 => write!(f, "datatype: '{}'", "F32"),
            F64 => write!(f, "datatype: '{}'", "F64"),
            Str => write!(f, "datatype: '{}'", "Str"),
            Chr => write!(f, "datatype: '{}'", "Chr"),
            Bln => write!(f, "datatype: '{}'", "Bln"),
            Void => write!(f, "datatype: '{}'", "Void"),
            UserDef(name) => write!(f, "user datatype: '{}'", name),
            Generic(name) => write!(f, "generic datatype: '{}'", name),
            Ptr(datatype) => write!(f, "pointer datatype: '{}'", datatype),
            FnPtr { return_type, args } => write!(
                f,
                "function pointer datatype: '{}'",
                format!("{}({:?})", return_type, args)
            ),
        }
    }
}

impl Display for OperatorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OperatorType::*;
        match self {
            Add => write!(f, "operator: '{}'", "+"),
            Sub => write!(f, "operator: '{}'", "-"),
            Mul => write!(f, "operator: '{}'", "*"),
            Div => write!(f, "operator: '{}'", "/"),
            Mod => write!(f, "operator: '{}'", "%"),
            BitwiseAnd => write!(f, "operator: '{}'", "&"),
            BitwiseOr => write!(f, "operator: '{}'", "|"),
            BitwiseXor => write!(f, "operator: '{}'", "^"),
            BitwiseNot => write!(f, "operator: '{}'", "~"),
            LogicalAnd => write!(f, "operator: '{}'", "&&"),
            LogicalOr => write!(f, "operator: '{}'", "||"),
            LogicalNot => write!(f, "operator: '{}'", "!"),
            Assign => write!(f, "operator: '{}'", "="),
            AddAssign => write!(f, "operator: '{}'", "+="),
            SubAssign => write!(f, "operator: '{}'", "-="),
            MulAssign => write!(f, "operator: '{}'", "*="),
            DivAssign => write!(f, "operator: '{}'", "/="),
            ModAssign => write!(f, "operator: '{}'", "%="),
            Equal => write!(f, "operator: '{}'", "=="),
            NotEqual => write!(f, "operator: '{}'", "!="),
            GreaterThan => write!(f, "operator: '{}'", ">"),
            LessThan => write!(f, "operator: '{}'", "<"),
            GreaterThanOrEqual => write!(f, "operator: '{}'", ">="),
            LessThanOrEqual => write!(f, "operator: '{}'", "<="),
            ShiftLeft => write!(f, "operator: '{}'", "<<"),
            ShiftRight => write!(f, "operator: '{}'", ">>"),
            Ternary => write!(f, "operator: '{}'", "?"),
            Increment => write!(f, "operator: '{}'", "++"),
            Decrement => write!(f, "operator: '{}'", "--"),
        }
    }
}

impl Display for PunctuatorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use PunctuatorType::*;
        match self {
            LeftParen => write!(f, "punctuator: '{}'", "("),
            RightParen => write!(f, "punctuator: '{}'", ")"),
            LeftBrace => write!(f, "punctuator: '{}'", "{"),
            RightBrace => write!(f, "punctuator: '{}'", "}"),
            LeftBracket => write!(f, "punctuator: '{}'", "["),
            RightBracket => write!(f, "punctuator: '{}'", "]"),
            Tick => write!(f, "punctuator: '{}'", "`"),
            Semicolon => write!(f, "punctuator: '{}'", ";"),
            Colon => write!(f, "punctuator: '{}'", ":"),
            Comma => write!(f, "punctuator: '{}'", ","),
            Dot => write!(f, "punctuator: '{}'", "."),
        }
    }
}

impl Display for PreprocessorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use PreprocessorType::*;
        match self {
            Include => write!(f, "preprocessor: '{}'", "#include"),
            Define => write!(f, "preprocessor: '{}'", "#define"),
            If => write!(f, "preprocessor: '{}'", "#if"),
            Elif => write!(f, "preprocessor: '{}'", "#elif"),
            Else => write!(f, "preprocessor: '{}'", "#else"),
            Endif => write!(f, "preprocessor: '{}'", "#endif"),
            Undef => write!(f, "preprocessor: '{}'", "#undef"),
            Pragma => write!(f, "preprocessor: '{}'", "#pragma"),
        }
    }
}
