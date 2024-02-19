pub enum BuilinType {
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
    Void,
    Chr,
    Bln,
    Str,
}

pub enum Type {
    Builtin(BuilinType),
    Pointer(Box<Type>),
    Array {
        base: Box<Type>,
        length: usize,
    },
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },
    Function {
        args: Vec<Type>,
        ret: Box<Type>,
    },
    Enum {
        name: String,
        variants: Vec<String>,
    },
    Union {
        name: String,
        fields: Vec<(String, Type)>,
    },
}

#[derive(PartialEq, Eq)]
pub enum UnaryOp {
    BitNot,
    Not,
    Neg,
    Pos,
    Deref,
    Ref,
    PreInc,
    PreDec,
}

#[derive(PartialEq, Eq)]
pub enum BinaryOp {
    ModAssign,
    DivAssign,
    MulAssign,
    SubAssign,
    AddAssign,
    Assign,
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    Neq,
    Eq,
    Gte,
    Gt,
    Lte,
    Lt,
    Shr,
    Shl,
    Sub,
    Add,
    Mod,
    Div,
    Mul,
}

pub enum Expr {
    Numeric(i64),
    Strng(String),
    Float(f64),
    Char(char),
    Bool(bool),
    Var {
        name: String,
        ty: Type,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
        ty: Type,
    },
    BinaryOp {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
        ty: Type,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        ty: Type,
    },
    Index {
        name: String,
        index: Box<Expr>,
        ty: Type,
    },
    Member {
        name: String,
        member: String,
        ty: Type,
    },
    Cast {
        expr: Box<Expr>,
        from: Type,
        to: Type,
    },
    SizeOf {
        expr: Box<Expr>,
    },
}

pub enum Stmt {
    Expr(Expr),
    If {
        cond: Expr,
        then: Box<Stmt>,
        elifs: Vec<(Expr, Box<Stmt>)>,
        els: Option<Box<Stmt>>,
    },
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
    For {
        init: Option<Expr>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Box<Stmt>,
    },
    Return(Option<Expr>),
    Block(Vec<Stmt>),
    VarDecl {
        name: String,
        ty: Type,
        value: Option<Expr>,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, Type)>,
    },
    EnumDecl {
        name: String,
        variants: Vec<String>,
    },
    UnionDecl {
        name: String,
        fields: Vec<(String, Type)>,
    },
    FunctionDecl {
        name: String,
        args: Vec<(String, Type)>,
        ret: Type,
        body: Box<Stmt>,
    },
}

pub struct Program {
    pub stmts: Vec<Stmt>,
}
