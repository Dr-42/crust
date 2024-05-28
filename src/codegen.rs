use inkwell::context::Context;

use crate::ast::nodes::{Expr, Program, Stmt, Type};

struct Codegen<'a> {
    context: Context,
    module: inkwell::module::Module<'a>,
    builder: inkwell::builder::Builder<'a>,
}

impl<'a> Codegen<'a> {
    pub fn new(file_name: &str) -> Self {
        let context = Context::create();
        let module = context.create_module(file_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
        }
    }

    pub fn codegen(&self, program: Program) {
        for stmt in program.stmts {
            match *stmt {
                Stmt::Expr(expr) => {
                    todo!()
                }
                Stmt::TypeAlias { name, ty, span } => {
                    todo!()
                }
                Stmt::If {
                    cond,
                    body,
                    els,
                    span,
                } => {
                    todo!()
                }
                Stmt::While { cond, body, span } => {
                    todo!()
                }
                Stmt::For {
                    init,
                    cond,
                    step,
                    body,
                    span,
                } => {
                    todo!()
                }
                Stmt::Return(value) => {
                    todo!()
                }
                Stmt::Block { stmts, decl_data } => {
                    todo!()
                }
                Stmt::VarDecl {
                    name,
                    qualifiers,
                    ty,
                    value,
                    span,
                } => {
                    todo!()
                }
                Stmt::StructDecl {
                    name,
                    fields,
                    generics,
                    span,
                } => {
                    todo!()
                }
                Stmt::ImplDecl { ty, methods, span } => {
                    todo!()
                }
                Stmt::TraitDecl {
                    name,
                    methods,
                    span,
                } => {
                    todo!()
                }
                Stmt::EnumDecl {
                    name,
                    variants,
                    span,
                } => {
                    todo!()
                }
                Stmt::UnionDecl { name, fields, span } => {
                    todo!()
                }
                Stmt::FunctionDecl {
                    name,
                    qualifiers,
                    args,
                    ret,
                    body,
                    generics,
                    isvararg,
                    span,
                } => {
                    todo!()
                }
                Stmt::VarAssign {
                    name,
                    value,
                    op,
                    span,
                } => {
                    todo!()
                }
                Stmt::DerefAssign {
                    value,
                    expr,
                    op,
                    span,
                } => {
                    todo!()
                }
                Stmt::StructAssign {
                    name,
                    qualifiers,
                    ty,
                    fields,
                    span,
                } => {
                    todo!()
                }
                Stmt::StructMemberAssign {
                    name,
                    value,
                    op,
                    span,
                } => {
                    todo!()
                }
                Stmt::ArrayMemberAssign {
                    element,
                    value,
                    op,
                    span,
                } => {
                    todo!()
                }
                Stmt::TraitAssign {
                    name,
                    for_ty,
                    methods,
                    span,
                } => {
                    todo!()
                }
                Stmt::Match { expr, cases, span } => {
                    todo!()
                }
                Stmt::Break => {
                    todo!()
                }
                Stmt::Continue => {
                    todo!()
                }
            }
        }
    }

    pub fn codegen_expr(&self, expr: Expr) {
        match expr {
            Expr::Numeric { val, span } => {
                todo!()
            }
            Expr::Strng { val, span } => {
                todo!()
            }
            Expr::Flt { val, span } => {
                todo!()
            }
            Expr::Chr { val, span } => {
                todo!()
            }
            Expr::Bln { val, span } => {
                todo!()
            }
            Expr::Iden { val, span } => {
                todo!()
            }
            Expr::UnaryOp { op, expr, span } => {
                todo!()
            }
            Expr::BinaryOp { lhs, op, rhs, span } => {
                todo!()
            }
            Expr::Call {
                name,
                args,
                generics,
                span,
            } => {
                todo!()
            }
            Expr::Index {
                name,
                indices,
                span,
            } => {
                todo!()
            }
            Expr::MemberAccess { name, member, span } => {
                todo!()
            }
            Expr::ArrayLiteral { vals, span } => {
                todo!()
            }
            Expr::EnumLiteral {
                name,
                variant,
                span,
            } => {
                todo!()
            }
        }
    }
}
