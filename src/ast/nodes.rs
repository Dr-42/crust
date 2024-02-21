use super::Span;

pub enum Comment {
    SingleLine(String),
    MultiLine(String),
    Doc(String),
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct GenericType {
    pub name: String,
    pub constraints: Option<Vec<Box<Type>>>,
}

impl GenericType {
    pub fn new(name: String, constraints: Option<Vec<Box<Type>>>) -> Self {
        Self { name, constraints }
    }
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Numeric {
        val: i64,
        span: Span,
    },
    Strng {
        val: String,
        span: Span,
    },
    Flt {
        val: f64,
        span: Span,
    },
    Chr {
        val: char,
        span: Span,
    },
    Bln {
        val: bool,
        span: Span,
    },
    Iden {
        val: String,
        span: Span,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    BinaryOp {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<Box<Expr>>,
        generics: Option<Vec<Box<Type>>>,
        span: Span,
    },
    Index {
        name: String,
        indices: Vec<Box<Expr>>,
        span: Span,
    },
    MemberAccess {
        name: Box<Expr>,
        member: Box<Expr>,
        span: Span,
    },
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Box<Expr>),
    If {
        cond: Box<Expr>,
        body: Box<Stmt>,
        els: Option<Box<Stmt>>,
        span: Span,
    },
    While {
        cond: Box<Expr>,
        body: Box<Stmt>,
        span: Span,
    },
    For {
        init: Box<Stmt>,
        cond: Box<Expr>,
        step: Box<Stmt>,
        body: Box<Stmt>,
        span: Span,
    },
    Return(Option<Box<Expr>>),
    Block(Vec<Box<Stmt>>),
    VarDecl {
        name: String,
        ty: Box<Type>,
        value: Option<Box<Expr>>,
        span: Span,
    },
    StructDecl {
        name: String,
        fields: Vec<Box<Stmt>>,
        generics: Option<Vec<Box<GenericType>>>,
        span: Span,
    },
    ImplDecl {
        ty: Type,
        methods: Vec<Box<Stmt>>,
        span: Span,
    },
    TraitDecl {
        name: String,
        for_ty: Box<Type>,
        methods: Vec<Box<Stmt>>,
        span: Span,
    },
    EnumDecl {
        name: String,
        variants: Vec<String>,
        span: Span,
    },
    UnionDecl {
        name: String,
        fields: Vec<(String, Type)>,
        span: Span,
    },
    FunctionDecl {
        name: String,
        args: Vec<Box<Stmt>>,
        ret: Box<Type>,
        body: Option<Box<Stmt>>,
        generics: Option<Vec<Box<GenericType>>>,
        isvararg: bool,
        span: Span,
    },
    VarAssign {
        name: String,
        value: Box<Expr>,
        op: AssignOp,
        span: Span,
    },
    DerefAssign {
        value: Box<Expr>,
        expr: Box<Expr>,
        op: AssignOp,
        span: Span,
    },
    StructAssign {
        name: String,
        sname: String,
        fields: Vec<(String, Box<Stmt>)>,
        span: Span,
    },
    StructMemberAssign {
        name: Box<Expr>,
        value: Box<Expr>,
        op: AssignOp,
        span: Span,
    },
    ArrayAssign {
        name: String,
        value: Vec<Box<Expr>>,
        span: Span,
    },
    ArrayMemberAssign {
        element: Box<Expr>,
        value: Box<Expr>,
        op: AssignOp,
        span: Span,
    },
    Break,
    Continue,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub stmts: Vec<Box<Stmt>>,
}

impl Program {
    pub fn new(stmts: Vec<Box<Stmt>>) -> Self {
        Self { stmts }
    }
}
