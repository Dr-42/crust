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
}

impl TychContext {
    pub fn new(file_id: usize) -> Self {
        Self {
            decl_data: DeclData::new(),
            file_id,
            current_fn_return_type: None,
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
                self.tych_stmt(*body)?;
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
                span,
            } => {
                self.tych_stmt(*init)?;
                let cond_type = self.tych_expr(*cond)?;
                if cond_type != Type::Builtin(BuiltinType::Bln) {
                    return Err(
                        self.create_error("Expected boolean expression in for statement", span)
                    );
                }
                self.tych_stmt(*step)?;
                self.tych_stmt(*body)?;
            }
            Stmt::Return { expr, span } => {
                if self.current_fn_return_type.is_none() {
                    return Err(self.create_error("Return statement outside of function", span));
                }
                if let Some(ret_ty) = expr {
                    let expr_ty = self.tych_expr(ret_ty)?;
                    if self.current_fn_return_type.unwrap() != expr_ty {
                        return Err(self.create_error("Return type mismatch", span));
                    }
                }
                if self.current_fn_return_type.unwrap() != Type::Builtin(BuiltinType::Void) {
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
            } => todo!(),
            Stmt::StructDecl {
                name,
                fields,
                generics,
                span,
            } => todo!(),
            Stmt::ImplDecl { ty, methods, span } => todo!(),
            Stmt::TraitDecl {
                name,
                methods,
                span,
            } => todo!(),
            Stmt::EnumDecl {
                name,
                variants,
                span,
            } => todo!(),
            Stmt::UnionDecl { name, fields, span } => todo!(),
            Stmt::FunctionDecl {
                name,
                qualifiers,
                args,
                ret,
                body,
                generics,
                isvararg,
                span,
            } => todo!(),
            Stmt::VarAssign {
                name,
                value,
                op,
                span,
            } => todo!(),
            Stmt::DerefAssign {
                value,
                expr,
                op,
                span,
            } => todo!(),
            Stmt::StructAssign {
                name,
                qualifiers,
                ty,
                fields,
                span,
            } => todo!(),
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
            Stmt::Break => todo!(),
            Stmt::Continue => todo!(),
        };
        Ok(())
    }

    pub fn tych_expr(&mut self, expr: Expr) -> Result<Type, Diagnostic> {
        todo!()
    }
}
