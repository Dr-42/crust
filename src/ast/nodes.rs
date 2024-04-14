use std::error::Error;

use super::Span;

pub enum Comment {
    SingleLine(String),
    MultiLine(String),
    Doc(String),
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct GenericType {
    pub name: Box<Expr>,
    pub constraints: Option<Vec<Box<Type>>>,
}

impl GenericType {
    pub fn new(name: Box<Expr>, constraints: Option<Vec<Box<Type>>>) -> Self {
        Self { name, constraints }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UserDefinedType {
    Struct,
    Enum,
    Union,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Builtin(BuiltinType),
    Pointer(Box<Type>),
    UserDefined {
        name: Box<Expr>,
        generic_args: Option<Vec<Box<Type>>>,
    },
    Array {
        base: Box<Type>,
        lengths: Vec<usize>,
    },
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::Builtin(BuiltinType::I8)
                | Type::Builtin(BuiltinType::I16)
                | Type::Builtin(BuiltinType::I32)
                | Type::Builtin(BuiltinType::I64)
                | Type::Builtin(BuiltinType::U8)
                | Type::Builtin(BuiltinType::U16)
                | Type::Builtin(BuiltinType::U32)
                | Type::Builtin(BuiltinType::U64)
                | Type::Builtin(BuiltinType::F32)
                | Type::Builtin(BuiltinType::F64)
        )
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_))
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Type::Builtin(BuiltinType::I8)
                | Type::Builtin(BuiltinType::I16)
                | Type::Builtin(BuiltinType::I32)
                | Type::Builtin(BuiltinType::I64)
                | Type::Builtin(BuiltinType::U8)
                | Type::Builtin(BuiltinType::U16)
                | Type::Builtin(BuiltinType::U32)
                | Type::Builtin(BuiltinType::U64)
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(
            self,
            Type::Builtin(BuiltinType::F32) | Type::Builtin(BuiltinType::F64)
        )
    }

    pub fn is_user_defined(&self) -> bool {
        matches!(self, Type::UserDefined { .. })
    }

    pub fn super_type(t1: &Type, t2: &Type) -> Result<Type, Box<dyn Error>> {
        if t1 == t2 {
            return Ok(t1.clone());
        }

        if t1.is_numeric() && t2.is_numeric() {
            if t1.is_float() || t2.is_float() {
                return match (t1, t2) {
                    (Type::Builtin(BuiltinType::F32), _) => Ok(t1.clone()),
                    (_, Type::Builtin(BuiltinType::F32)) => Ok(t2.clone()),
                    (Type::Builtin(BuiltinType::F64), _) => Ok(t1.clone()),
                    (_, Type::Builtin(BuiltinType::F64)) => Ok(t2.clone()),
                    _ => Ok(Type::Builtin(BuiltinType::F64)),
                };
            }
            return match (t1, t2) {
                (Type::Builtin(BuiltinType::I64), _) => Ok(t1.clone()),
                (_, Type::Builtin(BuiltinType::I64)) => Ok(t2.clone()),
                (Type::Builtin(BuiltinType::I32), _) => Ok(t1.clone()),
                (_, Type::Builtin(BuiltinType::I32)) => Ok(t2.clone()),
                (Type::Builtin(BuiltinType::I16), _) => Ok(t1.clone()),
                (_, Type::Builtin(BuiltinType::I16)) => Ok(t2.clone()),
                (Type::Builtin(BuiltinType::I8), _) => Ok(t1.clone()),
                (_, Type::Builtin(BuiltinType::I8)) => Ok(t2.clone()),
                _ => Ok(Type::Builtin(BuiltinType::I64)),
            };
        }

        Err("No super type found".into())
    }
}

#[derive(PartialEq, Clone, Debug)]
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

#[derive(PartialEq, Clone, Debug)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug, PartialEq, Clone)]
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
        name: Box<Expr>,
        args: Vec<Box<Expr>>,
        generics: Option<Vec<Box<Type>>>,
        span: Span,
    },
    Index {
        name: Box<Expr>,
        indices: Vec<Box<Expr>>,
        span: Span,
    },
    MemberAccess {
        name: Box<Expr>,
        member: Box<Expr>,
        span: Span,
    },
    ArrayExpr {
        elements: Vec<Box<Expr>>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Numeric { span, .. } => *span,
            Expr::Strng { span, .. } => *span,
            Expr::Flt { span, .. } => *span,
            Expr::Chr { span, .. } => *span,
            Expr::Bln { span, .. } => *span,
            Expr::Iden { span, .. } => *span,
            Expr::UnaryOp { span, .. } => *span,
            Expr::BinaryOp { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::MemberAccess { span, .. } => *span,
            Expr::ArrayExpr { span, .. } => *span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr {
        expr: Box<Expr>,
        span: Span,
    },
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
    Block {
        stmts: Vec<Box<Stmt>>,
    },
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
    ArrayMemberAssign {
        element: Box<Expr>,
        value: Box<Expr>,
        op: AssignOp,
        span: Span,
    },
    TraitAssign {
        name: Box<Expr>,
        for_ty: Box<Type>,
        methods: Vec<Box<Stmt>>,
        span: Span,
    },
    Break,
    Continue,
}

impl Stmt {
    pub fn add_block(stmts: Vec<Box<Stmt>>) -> Self {
        Stmt::Block { stmts }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub stmts: Vec<Box<Stmt>>,
}

impl Program {
    pub fn new(stmts: Vec<Box<Stmt>>) -> Self {
        Self { stmts }
    }
}
