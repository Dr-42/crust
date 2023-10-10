use crate::lexer::token_types::DataType;

pub struct AstData {
    pub variables: Vec<VariableData>,
    pub functions: Vec<FunctionData>,
    pub structs: Vec<StructData>,
    pub enums: Vec<EnumData>,
}

pub struct VariableData {
    pub name: String,
    pub type_: DataType,
}

pub struct FunctionData {
    pub name: String,
    pub return_type: DataType,
    pub arguments: Vec<VariableData>,
}

pub struct StructData {
    pub name: String,
    pub fields: Vec<VariableData>,
}

pub struct EnumData {
    pub name: String,
    pub variants: Vec<String>,
}

impl AstData {
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
        }
    }
}
