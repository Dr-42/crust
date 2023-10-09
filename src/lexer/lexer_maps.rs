use crate::lexer::types;
use std::collections::HashMap;

pub struct LexerMaps {
    pub keywords: HashMap<&'static str, types::KeywordType>,
    pub datatypes: HashMap<&'static str, types::DataType>,
    pub punctuators: HashMap<char, types::PunctuatorType>,
    pub operators: HashMap<&'static str, types::OperatorType>,
    pub preprocessor_directives: HashMap<&'static str, types::PreprocessorType>,
}

impl LexerMaps {
    pub fn default() -> Self {
        let keywords: HashMap<&str, types::KeywordType> = {
            let mut map = HashMap::new();
            map.insert("fnc", types::KeywordType::Fnc);
            map.insert("const", types::KeywordType::Const);
            map.insert("struct", types::KeywordType::Struct);
            map.insert("enum", types::KeywordType::Enum);
            map.insert("union", types::KeywordType::Union);
            map.insert("break", types::KeywordType::Break);
            map.insert("continue", types::KeywordType::Continue);
            map.insert("if", types::KeywordType::If);
            map.insert("elif", types::KeywordType::Elif);
            map.insert("else", types::KeywordType::Else);
            map.insert("do", types::KeywordType::Do);
            map.insert("while", types::KeywordType::While);
            map.insert("for", types::KeywordType::For);
            map.insert("ret", types::KeywordType::Ret);
            map.insert("switch", types::KeywordType::Switch);
            map.insert("case", types::KeywordType::Case);
            map.insert("default", types::KeywordType::Default);
            map.insert("as", types::KeywordType::As);
            map.insert("is", types::KeywordType::Is);
            map.insert("static", types::KeywordType::Static);
            map.insert("extern", types::KeywordType::Extern);
            map.insert("true", types::KeywordType::True);
            map.insert("false", types::KeywordType::False);
            map.insert("null", types::KeywordType::Null);
            map.insert("ptr", types::KeywordType::Ptr);
            map
        };

        let datatypes: HashMap<&str, types::DataType> = {
            let mut map = HashMap::new();
            map.insert("i8", types::DataType::I8);
            map.insert("i16", types::DataType::I16);
            map.insert("i32", types::DataType::I32);
            map.insert("i64", types::DataType::I64);
            map.insert("u8", types::DataType::U8);
            map.insert("u16", types::DataType::U16);
            map.insert("u32", types::DataType::U32);
            map.insert("u64", types::DataType::U64);
            map.insert("f32", types::DataType::F32);
            map.insert("f64", types::DataType::F64);
            map.insert("str", types::DataType::Str);
            map.insert("char", types::DataType::Chr);
            map.insert("bln", types::DataType::Bln);
            map.insert("void", types::DataType::Void);
            map
        };

        let punctuators = {
            let mut map = HashMap::new();
            map.insert('(', types::PunctuatorType::LeftParen);
            map.insert(')', types::PunctuatorType::RightParen);
            map.insert('{', types::PunctuatorType::LeftBrace);
            map.insert('}', types::PunctuatorType::RightBrace);
            map.insert('[', types::PunctuatorType::LeftBracket);
            map.insert(']', types::PunctuatorType::RightBracket);
            map.insert('`', types::PunctuatorType::Tick);
            map.insert(';', types::PunctuatorType::Semicolon);
            map.insert(':', types::PunctuatorType::Colon);
            map.insert('.', types::PunctuatorType::Dot);
            map
        };

        let operators = {
            let mut map = HashMap::new();
            map.insert("+", types::OperatorType::Add);
            map.insert("-", types::OperatorType::Sub);
            map.insert("*", types::OperatorType::Mul);
            map.insert("/", types::OperatorType::Div);
            map.insert("%", types::OperatorType::Mod);
            map.insert("&", types::OperatorType::BitwiseAnd);
            map.insert("|", types::OperatorType::BitwiseOr);
            map.insert("^", types::OperatorType::BitwiseXor);
            map.insert("~", types::OperatorType::BitwiseNot);
            map.insert("&&", types::OperatorType::LogicalAnd);
            map.insert("||", types::OperatorType::LogicalOr);
            map.insert("!", types::OperatorType::LogicalNot);
            map.insert("=", types::OperatorType::Assign);
            map.insert("+=", types::OperatorType::AddAssign);
            map.insert("-=", types::OperatorType::SubAssign);
            map.insert("*=", types::OperatorType::MulAssign);
            map.insert("/=", types::OperatorType::DivAssign);
            map.insert("%=", types::OperatorType::ModAssign);
            map.insert("==", types::OperatorType::Equal);
            map.insert("!=", types::OperatorType::NotEqual);
            map.insert(">", types::OperatorType::GreaterThan);
            map.insert("<", types::OperatorType::LessThan);
            map.insert(">=", types::OperatorType::GreaterThanOrEqual);
            map.insert("<=", types::OperatorType::LessThanOrEqual);
            map.insert("<<", types::OperatorType::ShiftLeft);
            map.insert(">>", types::OperatorType::ShiftRight);
            map.insert("?", types::OperatorType::Ternary);
            map.insert("++", types::OperatorType::Increment);
            map.insert("--", types::OperatorType::Decrement);
            map
        };

        let preprocessor_directives = {
            let mut map = HashMap::new();
            map.insert("#include", types::PreprocessorType::Include);
            map.insert("#define", types::PreprocessorType::Define);
            map.insert("#if", types::PreprocessorType::If);
            map.insert("#elif", types::PreprocessorType::Elif);
            map.insert("#else", types::PreprocessorType::Else);
            map.insert("#endif", types::PreprocessorType::Endif);
            map.insert("#undef", types::PreprocessorType::Undef);
            map.insert("#error", types::PreprocessorType::Error);
            map.insert("#pragma", types::PreprocessorType::Pragma);
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
