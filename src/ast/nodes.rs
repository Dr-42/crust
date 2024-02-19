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
    Generic {
        name: String,
        restrictions: Option<Vec<Type>>,
    },
    Array {
        base: Box<Type>,
        length: usize,
    },
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
        generics: Vec<Type>,
    },
    Function {
        args: Vec<Type>,
        generics: Option<Vec<Type>>,
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
    Flt(f64),
    Chr(char),
    Bln(bool),
    Var(String),
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    BinaryOp {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        generics: Vec<Type>,
    },
    Index {
        name: String,
        index: Box<Expr>,
    },
    StructInit {
        ty: String,
        name: String,
        fields: Vec<(String, Expr)>,
    },
    ArrayInit {
        elements: Vec<Expr>,
    },
    MemberAccess {
        name: String,
        member: String,
    },
    Cast {
        expr: Box<Expr>,
        to: Type,
    },
    SizeOf {
        expr: Box<Expr>,
    },
    AlignOf {
        member: String,
        ty: Type,
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
        generics: Option<Vec<Type>>,
    },
    ImplDecl {
        ty: Type,
        methods: Vec<Stmt>,
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
        generics: Option<Vec<Type>>,
    },
}

pub struct Program {
    pub stmts: Vec<Stmt>,
}
