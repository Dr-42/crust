use crate::types::*;
use std::collections::HashMap;

pub struct LexerMaps {
    pub keywords: HashMap<&'static str, KeywordType>,
    pub datatypes: HashMap<&'static str, DataType>,
    pub punctuators: HashMap<char, PunctuatorType>,
    pub operators: HashMap<&'static str, OperatorType>,
    pub preprocessor_directives: HashMap<&'static str, PreprocessorType>,
}

impl LexerMaps {
    pub fn default() -> Self {
        let keywords: HashMap<&str, KeywordType> = {
            let mut map = HashMap::new();
            map.insert("fnc", KeywordType::Fnc);
            map.insert("const", KeywordType::Const);
            map.insert("struct", KeywordType::Struct);
            map.insert("enum", KeywordType::Enum);
            map.insert("union", KeywordType::Union);
            map.insert("break", KeywordType::Break);
            map.insert("continue", KeywordType::Continue);
            map.insert("if", KeywordType::If);
            map.insert("elif", KeywordType::Elif);
            map.insert("else", KeywordType::Else);
            map.insert("do", KeywordType::Do);
            map.insert("while", KeywordType::While);
            map.insert("for", KeywordType::For);
            map.insert("ret", KeywordType::Ret);
            map.insert("switch", KeywordType::Switch);
            map.insert("case", KeywordType::Case);
            map.insert("default", KeywordType::Default);
            map.insert("as", KeywordType::As);
            map.insert("is", KeywordType::Is);
            map.insert("static", KeywordType::Static);
            map.insert("extern", KeywordType::Extern);
            map.insert("true", KeywordType::True);
            map.insert("false", KeywordType::False);
            map.insert("null", KeywordType::Null);
            map.insert("ptr", KeywordType::Ptr);
            map
        };

        let datatypes: HashMap<&str, DataType> = {
            let mut map = HashMap::new();
            map.insert("i8", DataType::I8);
            map.insert("i16", DataType::I16);
            map.insert("i32", DataType::I32);
            map.insert("i64", DataType::I64);
            map.insert("u8", DataType::U8);
            map.insert("u16", DataType::U16);
            map.insert("u32", DataType::U32);
            map.insert("u64", DataType::U64);
            map.insert("f32", DataType::F32);
            map.insert("f64", DataType::F64);
            map.insert("str", DataType::Str);
            map.insert("char", DataType::Chr);
            map.insert("bln", DataType::Bln);
            map.insert("void", DataType::Void);
            map
        };

        let punctuators = {
            let mut map = HashMap::new();
            map.insert('(', PunctuatorType::LeftParen);
            map.insert(')', PunctuatorType::RightParen);
            map.insert('{', PunctuatorType::LeftBrace);
            map.insert('}', PunctuatorType::RightBrace);
            map.insert('[', PunctuatorType::LeftBracket);
            map.insert(']', PunctuatorType::RightBracket);
            map.insert('`', PunctuatorType::Tick);
            map.insert(';', PunctuatorType::Semicolon);
            map.insert(':', PunctuatorType::Colon);
            map.insert(',', PunctuatorType::Comma);
            map.insert('.', PunctuatorType::Dot);
            map
        };

        let operators = {
            let mut map = HashMap::new();
            map.insert("+", OperatorType::Add);
            map.insert("-", OperatorType::Sub);
            map.insert("*", OperatorType::Mul);
            map.insert("/", OperatorType::Div);
            map.insert("%", OperatorType::Mod);
            map.insert("&", OperatorType::BitwiseAnd);
            map.insert("|", OperatorType::BitwiseOr);
            map.insert("^", OperatorType::BitwiseXor);
            map.insert("~", OperatorType::BitwiseNot);
            map.insert("&&", OperatorType::LogicalAnd);
            map.insert("||", OperatorType::LogicalOr);
            map.insert("!", OperatorType::LogicalNot);
            map.insert("=", OperatorType::Assign);
            map.insert("+=", OperatorType::AddAssign);
            map.insert("-=", OperatorType::SubAssign);
            map.insert("*=", OperatorType::MulAssign);
            map.insert("/=", OperatorType::DivAssign);
            map.insert("%=", OperatorType::ModAssign);
            map.insert("==", OperatorType::Equal);
            map.insert("!=", OperatorType::NotEqual);
            map.insert(">", OperatorType::GreaterThan);
            map.insert("<", OperatorType::LessThan);
            map.insert(">=", OperatorType::GreaterThanOrEqual);
            map.insert("<=", OperatorType::LessThanOrEqual);
            map.insert("<<", OperatorType::ShiftLeft);
            map.insert(">>", OperatorType::ShiftRight);
            map.insert("?", OperatorType::Ternary);
            map.insert("++", OperatorType::Increment);
            map.insert("--", OperatorType::Decrement);
            map
        };

        let preprocessor_directives = {
            let mut map = HashMap::new();
            map.insert("#include", PreprocessorType::Include);
            map.insert("#define", PreprocessorType::Define);
            map.insert("#if", PreprocessorType::If);
            map.insert("#elif", PreprocessorType::Elif);
            map.insert("#else", PreprocessorType::Else);
            map.insert("#endif", PreprocessorType::Endif);
            map.insert("#undef", PreprocessorType::Undef);
            map.insert("#pragma", PreprocessorType::Pragma);
            map
        };

        Self {
            keywords,
            datatypes,
            punctuators,
            operators,
            preprocessor_directives,
        }
    }
}
