use super::Span;

pub enum Comment {
    SingleLine(String),
    MultiLine(String),
    Doc(String),
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct GenericType {
    pub name: Box<Expr>,
    pub constraints: Option<Vec<Box<Type>>>,
}

impl GenericType {
    pub fn new(name: Box<Expr>, constraints: Option<Vec<Box<Type>>>) -> Self {
        Self { name, constraints }
    }
}

#[derive(Debug, PartialEq)]
pub enum UserDefinedType {
    Struct,
    Enum,
    Union,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Builtin(BuiltinType),
    Pointer(Box<Type>),
    UserDefined {
        name: Box<Expr>,
        generic_args: Option<Vec<Box<Type>>>,
        variant: Option<UserDefinedType>,
    },
    Array {
        base: Box<Type>,
        length: usize,
    },
}

#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, Debug)]
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

#[derive(Debug, PartialEq)]
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
        ty: Option<Box<Type>>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
        ty: Option<Box<Type>>,
    },
    BinaryOp {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
        span: Span,
        ty: Option<Box<Type>>,
    },
    Call {
        name: Box<Expr>,
        args: Vec<Box<Expr>>,
        generics: Option<Vec<Box<Type>>>,
        span: Span,
        ty: Option<Box<Type>>,
    },
    Index {
        name: Box<Expr>,
        indices: Vec<Box<Expr>>,
        span: Span,
        ty: Option<Box<Type>>,
    },
    MemberAccess {
        name: Box<Expr>,
        member: Box<Expr>,
        span: Span,
        ty: Option<Box<Type>>,
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
        name: Box<Expr>,
        ty: Box<Type>,
        value: Option<Box<Expr>>,
        span: Span,
    },
    StructDecl {
        name: Box<Expr>,
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
        name: Box<Expr>,
        for_ty: Box<Type>,
        methods: Vec<Box<Stmt>>,
        span: Span,
    },
    EnumDecl {
        name: Box<Expr>,
        variants: Vec<String>,
        span: Span,
    },
    UnionDecl {
        name: Box<Expr>,
        fields: Vec<(Box<Expr>, Type)>,
        span: Span,
    },
    FunctionDecl {
        name: Box<Expr>,
        args: Vec<Box<Stmt>>,
        ret: Box<Type>,
        body: Option<Box<Stmt>>,
        generics: Option<Vec<Box<GenericType>>>,
        isvararg: bool,
        span: Span,
    },
    VarAssign {
        name: Box<Expr>,
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
        name: Box<Expr>,
        sname: Box<Expr>,
        fields: Vec<(Box<Expr>, Box<Stmt>)>,
        span: Span,
    },
    StructMemberAssign {
        name: Box<Expr>,
        value: Box<Expr>,
        op: AssignOp,
        span: Span,
    },
    ArrayAssign {
        name: Box<Expr>,
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
