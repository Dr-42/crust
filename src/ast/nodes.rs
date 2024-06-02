use super::{decldata::DeclData, Span};

#[derive(Debug, PartialEq, Clone)]
pub struct QualifierSet {
    pub is_static: bool,
    pub is_const: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Qualifier {
    Static,
    Const,
}

#[derive(Debug, PartialEq, Clone)]
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
    Slf,
}

#[derive(Debug, Clone)]
pub enum Type {
    Builtin(BuiltinType),
    Pointer(Box<Type>),
    FnPtr {
        args: Vec<Box<Type>>,
        ret: Box<Type>,
    },
    UserDefined {
        name: Box<Expr>,
    },
    TraitType {
        traits: Vec<Box<Expr>>,
    },
    Array {
        base: Box<Type>,
        lens: Vec<usize>,
    },
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Builtin(l0), Self::Builtin(r0)) => l0 == r0,
            (Self::Pointer(l0), Self::Pointer(r0)) => l0 == r0,
            (
                Self::FnPtr {
                    args: l_args,
                    ret: l_ret,
                },
                Self::FnPtr {
                    args: r_args,
                    ret: r_ret,
                },
            ) => l_args == r_args && l_ret == r_ret,
            (Self::UserDefined { name: l_name }, Self::UserDefined { name: r_name }) => {
                let l_name_val = match l_name.as_ref() {
                    Expr::Iden { val, .. } => val,
                    _ => unreachable!(),
                };
                let r_name_val = match r_name.as_ref() {
                    Expr::Iden { val, .. } => val,
                    _ => unreachable!(),
                };
                l_name_val == r_name_val
            }
            (
                Self::Array {
                    base: l_base,
                    lens: l_lens,
                },
                Self::Array {
                    base: r_base,
                    lens: r_lens,
                },
            ) => l_base == r_base && l_lens == r_lens,
            (Self::TraitType { traits: l_traits }, Self::TraitType { traits: r_traits }) => {
                l_traits == r_traits
            }
            _ => false,
        }
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
pub struct MatchCase {
    pub pattern: Box<Expr>,
    pub body: Box<Stmt>,
    pub span: Span,
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
        span: Span,
    },
    Index {
        name: Box<Expr>,
        indices: Vec<Box<Expr>>,
        span: Span,
    },
    MemberAccess {
        strct: Box<Expr>,
        member: Box<Expr>,
        span: Span,
    },
    ArrayLiteral {
        vals: Vec<Box<Expr>>,
        span: Span,
    },
    EnumLiteral {
        name: Box<Expr>,
        variant: Box<Expr>,
        span: Span,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Box<Expr>),
    TypeAlias {
        name: Box<Expr>,
        ty: Box<Type>,
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
    Return {
        expr: Option<Box<Expr>>,
        span: Span,
    },
    Block {
        stmts: Vec<Box<Stmt>>,
    },
    VarDecl {
        name: Box<Expr>,
        qualifiers: Option<QualifierSet>,
        ty: Box<Type>,
        value: Option<Box<Expr>>,
        span: Span,
    },
    StructDecl {
        name: Box<Expr>,
        fields: Vec<Box<Stmt>>,
        span: Span,
    },
    ImplDecl {
        name: Box<Expr>,
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
        variants: Vec<Box<Expr>>,
        span: Span,
    },
    UnionDecl {
        name: Box<Expr>,
        fields: Vec<Box<Stmt>>,
        span: Span,
    },
    FunctionDecl {
        name: Box<Expr>,
        qualifiers: Option<QualifierSet>,
        args: Vec<Box<Stmt>>,
        ret: Box<Type>,
        body: Option<Box<Stmt>>,
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
        qualifiers: Option<QualifierSet>,
        ty: Option<Box<Type>>,
        fields: Vec<(Box<Expr>, Box<Expr>)>,
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
    Match {
        expr: Box<Expr>,
        cases: Vec<Box<MatchCase>>,
        span: Span,
    },
    Break(Span),
    Continue(Span),
    Comment(Span),
}

impl Stmt {
    pub fn add_block(stmts: Vec<Box<Stmt>>) -> Self {
        Stmt::Block { stmts }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub stmts: Vec<Box<Stmt>>,
    pub decl_data: DeclData,
}

impl Program {
    pub fn new(stmts: Vec<Box<Stmt>>) -> Self {
        let mut decl_data = DeclData::new();
        for stmt in &stmts {
            decl_data.add(stmt);
        }
        Self { stmts, decl_data }
    }
}
