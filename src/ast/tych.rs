#![allow(dead_code, unused)]
use std::error::Error;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use super::{
    decldata::DeclData,
    nodes::{BuiltinType, Expr, Program, Stmt, Type},
    Span,
};

pub struct TychContext {
    decl_data: DeclData,
    file_id: usize,
    current_fn_return_type: Option<Type>,
    in_loop: bool,
}

impl TychContext {
    pub fn new(file_id: usize) -> Self {
        Self {
            decl_data: DeclData::new(),
            file_id,
            current_fn_return_type: None,
            in_loop: false,
        }
    }

    pub fn create_error(&self, msg: &str, span: Span) -> Diagnostic<usize> {
        eprintln!("Error detected during type checking: {}", msg);
        let diag = Diagnostic::error()
            .with_message(msg)
            .with_labels(vec![Label::primary(self.file_id, span).with_message("here")]);

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        diag
    }

    pub fn tych_program(
        &mut self,
        file_id: usize,
        program: Program,
    ) -> Result<(), Diagnostic<usize>> {
        for stmt in program.stmts {
            self.decl_data.add(&stmt);
            self.tych_stmt(*stmt)?;
        }
        Ok(())
    }

    pub fn tych_stmt(&mut self, stmt: Stmt) -> Result<(), Diagnostic<usize>> {
        match stmt {
            Stmt::Expr(expr) => {
                let _ = self.tych_expr(*expr)?;
                return Ok(());
            }
            Stmt::TypeAlias { name, ty, span } => {
                return Ok(());
            }
            Stmt::If {
                cond,
                body,
                els,
                span,
            } => {
                let cond_type = self.tych_expr(*cond)?;
                if cond_type != Type::Builtin(BuiltinType::Bln) {
                    return Err(
                        self.create_error("Expected boolean expression in if statement", span)
                    );
                }
                self.tych_stmt(*body)?;
                if let Some(els) = els {
                    self.tych_stmt(*els)?;
                }
            }
            Stmt::While { cond, body, span } => {
                let cond_type = self.tych_expr(*cond)?;
                if cond_type != Type::Builtin(BuiltinType::Bln) {
                    return Err(
                        self.create_error("Expected boolean expression in while statement", span)
                    );
                }
                self.in_loop = true;
                self.tych_stmt(*body)?;
                self.in_loop = false;
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
                span,
            } => {
                self.decl_data.push_scope();
                self.tych_stmt(*init)?;
                let cond_type = self.tych_expr(*cond)?;
                if cond_type != Type::Builtin(BuiltinType::Bln) {
                    return Err(
                        self.create_error("Expected boolean expression in for statement", span)
                    );
                }
                self.tych_stmt(*step)?;
                self.in_loop = true;
                self.tych_stmt(*body)?;
                self.in_loop = false;
                self.decl_data.pop_scope();
            }
            Stmt::Return { expr, span } => {
                if self.current_fn_return_type.is_none() {
                    return Err(self.create_error("Return statement outside of function", span));
                }
                if let Some(ret_ty) = expr {
                    let expr_ty = self.tych_expr(*ret_ty)?;
                    if self.current_fn_return_type.clone().unwrap() != expr_ty {
                        return Err(self.create_error("Return type mismatch", span));
                    }
                }
                if self.current_fn_return_type.clone().unwrap() != Type::Builtin(BuiltinType::Void)
                {
                    return Err(self.create_error(
                        "Returning nothing from a function with declared type",
                        span,
                    ));
                }
                return Ok(());
            }
            Stmt::Block { stmts, decl_data } => {
                self.decl_data.push_scope();
                for stmt in stmts {
                    self.tych_stmt(*stmt)?;
                }
                self.decl_data.pop_scope();
            }
            Stmt::VarDecl {
                name,
                qualifiers,
                ty,
                value,
                span,
            } => match *name {
                Expr::Iden { val, span } => {
                    let var_name = val;
                    if let Some(value) = value {
                        let value_ty = self.tych_expr(*value)?;
                        if *ty != value_ty {
                            return Err(
                                self.create_error("Type mismatch in variable assignment", span)
                            );
                        }
                    }
                }
                _ => return Err(self.create_error("Expected identifier", span)),
            },
            Stmt::StructDecl {
                name,
                fields,
                generics,
                span,
            } => match *name {
                Expr::Iden { val, span } => {
                    let struct_name = val;
                    for field in fields {
                        self.tych_stmt(*field)?;
                    }
                }
                _ => return Err(self.create_error("Expected identifier", span)),
            },
            Stmt::ImplDecl { ty, methods, span } => {
                let struct_data = self.decl_data.struct_.clone();
                let ty_name = match ty {
                    Type::UserDefined { name, .. } => match *name {
                        Expr::Iden { val, span } => val,
                        _ => {
                            return Err(self.create_error("Expected identifier", span));
                        }
                    },
                    _ => {
                        return Err(
                            self.create_error("Expected user defined type to impl methods", span)
                        )
                    }
                };
                if struct_data.iter().any(|s| s.name == ty_name) {
                    return Err(self.create_error("Type not found for declaring impl", span));
                }
                for method in methods {
                    self.tych_stmt(*method)?;
                }
            }
            Stmt::TraitDecl {
                name,
                methods,
                span,
            } => {
                let trait_name = match *name {
                    Expr::Iden { val, span } => val,
                    _ => return Err(self.create_error("Expected identifier for trait name", span)),
                };
                for method in methods {
                    self.tych_stmt(*method)?;
                }
            }
            Stmt::EnumDecl {
                name,
                variants,
                span,
            } => {
                let enum_name = match *name {
                    Expr::Iden { val, span } => val,
                    _ => {
                        return Err(self.create_error("Expected identifier for an enum name", span))
                    }
                };
                for variant in variants {
                    match *variant {
                        Expr::Iden { val, span } => {
                            let variant_name = val;
                        }
                        _ => {
                            return Err(
                                self.create_error("Expected identifier in enum variants", span)
                            )
                        }
                    }
                }
            }
            Stmt::UnionDecl {
                name,
                fields,
                generics,
                span,
            } => {
                let union_name = match *name {
                    Expr::Iden { val, span } => val,
                    _ => {
                        return Err(self.create_error("Expected identifier for a union name", span))
                    }
                };
                for field in fields {
                    self.tych_stmt(*field)?;
                }
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
                let fn_name = match *name {
                    Expr::Iden { val, span } => val,
                    _ => {
                        return Err(self.create_error("Expected identifier for function name", span))
                    }
                };
                self.current_fn_return_type = Some(*ret);
                for arg in args {
                    self.tych_stmt(*arg)?;
                }
                if let Some(body) = body {
                    self.tych_stmt(*body)?;
                }
            }
            Stmt::VarAssign {
                name,
                value,
                op,
                span,
            } => {
                let var_name = match *name {
                    Expr::Iden { val, span } => val,
                    _ => {
                        return Err(
                            self.create_error("Expected identifier for variable assignment", span)
                        )
                    }
                };
                let var_data = self.decl_data.var.iter().find(|v| v.name == var_name);
                if let Some(var_data) = var_data {
                    let var_ty = *var_data.ty.clone();
                    let value_ty = self.tych_expr(*value)?;
                    if var_ty != value_ty {
                        return Err(self.create_error("Type mismatch in variable assignment", span));
                    }
                } else {
                    return Err(self.create_error("Variable not found", span));
                }
            }
            Stmt::DerefAssign {
                value,
                expr,
                op,
                span,
            } => {
                let value_ty = self.tych_expr(*value)?;
                let expr_ty = self.tych_expr(*expr)?;
                if value_ty != Type::Pointer(Box::new(expr_ty)) {
                    return Err(self.create_error("Type mismatch in dereference assignment", span));
                }
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
            } => todo!(),
            Stmt::ArrayMemberAssign {
                element,
                value,
                op,
                span,
            } => todo!(),
            Stmt::TraitAssign {
                name,
                for_ty,
                methods,
                span,
            } => todo!(),
            Stmt::Match { expr, cases, span } => todo!(),
            Stmt::Break(span) => {
                if !self.in_loop {
                    return Err(self.create_error("Break statement outside of loop", span));
                }
            }
            Stmt::Continue(span) => {
                if !self.in_loop {
                    return Err(self.create_error("Continue statement outside of loop", span));
                }
            }
        };
        Ok(())
    }

    pub fn tych_expr(&mut self, expr: Expr) -> Result<Type, Diagnostic<usize>> {
        match expr {
            Expr::Numeric { val, span } => Err(self.create_error("Not yet handled", span)),
            Expr::Strng { val, span } => Err(self.create_error("Not yet handled", span)),
            Expr::Flt { val, span } => Err(self.create_error("Not yet handled", span)),
            Expr::Chr { val, span } => Err(self.create_error("Not yet handled", span)),
            Expr::Bln { val, span } => Err(self.create_error("Not yet handled", span)),
            Expr::Iden { val, span } => Err(self.create_error("Not yet handled", span)),
            Expr::UnaryOp { op, expr, span } => Err(self.create_error("Not yet handled", span)),
            Expr::BinaryOp { lhs, op, rhs, span } => {
                Err(self.create_error("Not yet handled", span))
            }
            Expr::Call {
                name,
                args,
                generics,
                span,
            } => Err(self.create_error("Not yet handled", span)),
            Expr::Index {
                name,
                indices,
                span,
            } => Err(self.create_error("Not yet handled", span)),
            Expr::MemberAccess { name, member, span } => {
                Err(self.create_error("Not yet handled", span))
            }
            Expr::ArrayLiteral { vals, span } => Err(self.create_error("Not yet handled", span)),
            Expr::EnumLiteral {
                name,
                variant,
                span,
            } => Err(self.create_error("Not yet handled", span)),
        }
    }
}
