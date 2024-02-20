#[derive(Debug)]
pub enum BuiltinType {
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

#[derive(Debug)]
pub struct GenericType {
    pub name: String,
    pub constraints: Option<Vec<Box<Type>>>,
}

impl GenericType {
    pub fn new(name: String, constraints: Option<Vec<Box<Type>>>) -> Self {
        Self { name, constraints }
    }
}

#[derive(Debug)]
pub enum Type {
    Builtin(BuiltinType),
    Pointer(Box<Type>),
    UserDefined {
        name: String,
        generic_args: Option<Vec<Box<Type>>>,
    },
    Array {
        base: Box<Type>,
        length: usize,
    },
}

#[derive(PartialEq, Eq, Debug)]
pub enum UnaryOp {
    Dec,
    Inc,
    Ref,
    Deref,
    Pos,
    Neg,
    Not,
    BitNot,
}

#[derive(PartialEq, Eq, Debug)]
pub enum BinaryOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
}

#[derive(Debug)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug)]
pub enum Expr {
    Numeric(i64),
    Strng(String),
    Flt(f64),
    Chr(char),
    Bln(bool),
    Iden(String),
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
        args: Vec<Box<Expr>>,
        generics: Option<Vec<Box<Type>>>,
    },
    Index {
        name: String,
        indices: Vec<Box<Expr>>,
    },
    MemberAccess {
        name: Box<Expr>,
        member: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Box<Expr>),
    If {
        cond: Box<Expr>,
        body: Box<Stmt>,
        els: Option<Box<Stmt>>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Stmt>,
    },
    For {
        init: Box<Stmt>,
        cond: Box<Expr>,
        step: Box<Stmt>,
        body: Box<Stmt>,
    },
    Return(Option<Box<Expr>>),
    Block(Vec<Box<Stmt>>),
    VarDecl {
        name: String,
        ty: Box<Type>,
        value: Option<Box<Expr>>,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, Type)>,
        generics: Option<Vec<GenericType>>,
    },
    ImplDecl {
        ty: Type,
        methods: Vec<Box<Stmt>>,
    },
    TraitDecl {
        name: String,
        for_ty: Box<Type>,
        methods: Vec<Box<Stmt>>,
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
        generics: Option<Vec<GenericType>>,
    },
    VarAssign {
        name: String,
        value: Expr,
        op: AssignOp,
    },
    StructAssign {
        name: String,
        field: String,
        value: Expr,
    },
    StructMemberAssign {
        name: String,
        member: String,
        value: Expr,
        op: AssignOp,
    },
    UnionAssign {
        name: String,
        field: String,
        value: Expr,
    },
    Break,
    Continue,
}

#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Box<Stmt>>,
}

impl Program {
    pub fn new(stmts: Vec<Box<Stmt>>) -> Self {
        Self { stmts }
    }
}
