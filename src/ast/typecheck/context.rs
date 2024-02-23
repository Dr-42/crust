use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::ast::{
    decldata::DeclData,
    nodes::{AssignOp, BuiltinType, Expr, GenericType, Program, Stmt, Type},
    Span,
};

use std::error::Error;

pub struct TypecheckContext {
    pub errors: Vec<Diagnostic<usize>>,
    pub decls: DeclData,
    func_ret_type: Option<Type>,
    in_loop: bool,
    file_id: usize,
}

impl TypecheckContext {
    pub fn new(file_id: usize) -> TypecheckContext {
        TypecheckContext {
            errors: Vec::new(),
            decls: DeclData::new(),
            func_ret_type: None,
            in_loop: false,
            file_id,
        }
    }

    pub fn typecheck_program(&mut self, prog: Program) -> Result<(), Box<dyn Error>> {
        for stmt in prog.stmts {
            self.typecheck_stmt(*stmt)?;
        }
        Ok(())
    }

    pub fn typecheck_stmt(&mut self, stmt: Stmt) -> Result<(), Box<dyn Error>> {
        match stmt {
            Stmt::Expr { expr, span } => self.typecheck_expr_stmt(*expr, span)?,
            Stmt::If {
                cond,
                body,
                els,
                span,
            } => {
                self.typecheck_if(*cond, *body, els, span)?;
            }
            Stmt::While { cond, body, span } => {
                self.typecheck_while(*cond, *body, span)?;
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
                span,
            } => {
                self.typecheck_for(*init, *cond, *step, *body, span)?;
            }
            Stmt::Return(expr) => {
                self.typecheck_return(expr)?;
            }
            Stmt::Block { stmts, .. } => {
                self.typecheck_block(stmts)?;
            }
            Stmt::VarDecl {
                name,
                ty,
                value,
                span,
            } => {
                self.typecheck_var_decl(*name, *ty, value, span)?;
            }
            Stmt::StructDecl {
                name,
                fields,
                generics,
                span,
            } => {
                self.typecheck_struct_decl(*name, fields, generics, span)?;
            }
            Stmt::ImplDecl { ty, methods, span } => {
                self.typecheck_impl_decl(ty, methods, span)?;
            }
            Stmt::TraitDecl {
                name,
                methods,
                span,
            } => {
                self.typecheck_trait_decl(*name, methods, span)?;
            }
            Stmt::EnumDecl {
                name,
                variants,
                span,
            } => {
                self.typecheck_enum_decl(*name, variants, span)?;
            }
            Stmt::UnionDecl { name, fields, span } => {
                self.typecheck_union_decl(*name, fields, span)?;
            }
            Stmt::FunctionDecl {
                name,
                args,
                ret,
                body,
                generics,
                isvararg,
                span,
            } => {
                self.typecheck_function_decl(*name, args, *ret, body, generics, isvararg, span)?;
            }
            Stmt::VarAssign {
                name,
                value,
                op,
                span,
            } => {
                self.typecheck_var_assign(*name, *value, op, span)?;
            }
            Stmt::DerefAssign {
                value,
                expr,
                op,
                span,
            } => {
                self.typecheck_deref_assign(*value, *expr, op, span)?;
            }
            Stmt::StructAssign {
                name,
                sname,
                fields,
                span,
            } => {
                self.typecheck_struct_assign(*name, *sname, fields, span)?;
            }
            Stmt::StructMemberAssign {
                name,
                value,
                op,
                span,
            } => {
                self.typecheck_struct_member_assign(*name, *value, op, span)?;
            }
            Stmt::ArrayAssign { name, value, span } => {
                self.typecheck_array_assign(*name, value, span)?;
            }
            Stmt::ArrayMemberAssign {
                element,
                value,
                op,
                span,
            } => {
                self.typecheck_array_member_assign(*element, *value, op, span)?;
            }
            Stmt::TraitAssign {
                name,
                for_ty,
                methods,
                span,
            } => {
                self.typecheck_trait_assign(*name, *for_ty, methods, span)?;
            }
            Stmt::Break => {
                self.typecheck_break()?;
            }
            Stmt::Continue => {
                self.typecheck_continue()?;
            }
        }
        Ok(())
    }

    fn typecheck_expr_stmt(&mut self, expr: Expr, span: Span) -> Result<(), Box<dyn Error>> {
        if self.func_ret_type.is_none() {
            self.errors.push(
                Diagnostic::error()
                    .with_message(
                        "Statements with only expressions are not allowed outside functions",
                    )
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("This expression statement is not allowed here")]),
            );
        }
        self.typecheck_expr(expr)?;
        Ok(())
    }

    fn typecheck_if(
        &mut self,
        cond: Expr,
        body: Stmt,
        els: Option<Box<Stmt>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        let cond_type = self.typecheck_expr(cond)?;
        if cond_type != Type::Builtin(BuiltinType::Bln) {
            self.errors.push(
                Diagnostic::error()
                    .with_message("Expected boolean expression")
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("The condition must be a boolean expression")]),
            );
        }
        self.decls.checkpoint();
        self.typecheck_stmt(body)?;
        self.decls.rollback();
        if let Some(els) = els {
            self.decls.checkpoint();
            self.typecheck_stmt(*els)?;
            self.decls.rollback();
        }
        Ok(())
    }

    fn typecheck_while(
        &mut self,
        cond: Expr,
        body: Stmt,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        let cond_type = self.typecheck_expr(cond)?;
        if cond_type != Type::Builtin(BuiltinType::Bln) {
            self.errors.push(
                Diagnostic::error()
                    .with_message("Expected boolean expression")
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("The condition must be a boolean expression")]),
            );
        }
        self.decls.checkpoint();
        self.in_loop = true;
        self.typecheck_stmt(body)?;
        self.in_loop = false;
        self.decls.rollback();
        Ok(())
    }

    fn typecheck_for(
        &mut self,
        init: Stmt,
        cond: Expr,
        step: Stmt,
        body: Stmt,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        self.decls.checkpoint();
        self.typecheck_stmt(init)?;
        let cond_type = self.typecheck_expr(cond)?;
        if cond_type != Type::Builtin(BuiltinType::Bln) {
            self.errors.push(
                Diagnostic::error()
                    .with_message("Expected boolean expression")
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("The condition must be a boolean expression")]),
            );
        }
        self.typecheck_stmt(step)?;
        self.in_loop = true;
        self.typecheck_stmt(body)?;
        self.in_loop = false;
        self.decls.rollback();
        Ok(())
    }

    fn typecheck_return(&mut self, expr: Option<Box<Expr>>) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_block(&mut self, stmts: Vec<Box<Stmt>>) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_var_decl(
        &mut self,
        name: Expr,
        ty: Type,
        value: Option<Box<Expr>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_struct_decl(
        &mut self,
        name: Expr,
        fields: Vec<Box<Stmt>>,
        generics: Option<Vec<Box<GenericType>>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_impl_decl(
        &mut self,
        ty: Type,
        methods: Vec<Box<Stmt>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_trait_decl(
        &mut self,
        name: Expr,
        methods: Vec<Box<Stmt>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_enum_decl(
        &mut self,
        name: Expr,
        variants: Vec<String>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_union_decl(
        &mut self,
        name: Expr,
        fields: Vec<(Box<Expr>, Type)>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_function_decl(
        &mut self,
        name: Expr,
        args: Vec<Box<Stmt>>,
        ret: Type,
        body: Option<Box<Stmt>>,
        generics: Option<Vec<Box<GenericType>>>,
        isvararg: bool,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_var_assign(
        &mut self,
        name: Expr,
        value: Expr,
        op: AssignOp,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_deref_assign(
        &mut self,
        value: Expr,
        expr: Expr,
        op: AssignOp,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_struct_assign(
        &mut self,
        name: Expr,
        sname: Expr,
        fields: Vec<(Box<Expr>, Box<Stmt>)>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_struct_member_assign(
        &mut self,
        name: Expr,
        value: Expr,
        op: AssignOp,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_array_assign(
        &mut self,
        name: Expr,
        value: Vec<Box<Expr>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_array_member_assign(
        &mut self,
        element: Expr,
        value: Expr,
        op: AssignOp,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_trait_assign(
        &mut self,
        name: Expr,
        for_ty: Type,
        methods: Vec<Box<Stmt>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_break(&mut self) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_continue(&mut self) -> Result<(), Box<dyn Error>> {
        todo!()
    }

    fn typecheck_expr(&mut self, expr: Expr) -> Result<Type, Box<dyn Error>> {
        todo!()
    }
}
