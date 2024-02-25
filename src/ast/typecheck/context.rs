use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::ast::{
    decldata::{DeclData, VarDeclData},
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
        if self.func_ret_type.is_none() {
            self.errors.push(
                Diagnostic::error()
                    .with_message("if statements are not allowed outside functions")
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("This statement is not allowed here")]),
            );
        }
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
        if self.func_ret_type.is_none() {
            self.errors.push(
                Diagnostic::error()
                    .with_message("while loops are not allowed outside functions")
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("This statement is not allowed here")]),
            );
        }
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
        if self.func_ret_type.is_none() {
            self.errors.push(
                Diagnostic::error()
                    .with_message("for loops are not allowed outside functions")
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("This statement is not allowed here")]),
            );
        }
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
        if self.func_ret_type.is_none() {
            self.errors.push(
                Diagnostic::error()
                    .with_message("return statements are not allowed outside functions")
                    .with_labels(vec![Label::primary(
                        self.file_id,
                        expr.as_ref().unwrap().span(),
                    )
                    .with_message("This statement is not allowed here")]),
            );
        }

        if let Some(expr) = expr {
            let expr_type = self.typecheck_expr(*expr.clone())?;
            if let Some(ret_type) = &self.func_ret_type {
                if expr_type != *ret_type {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Expected return type to match function return type")
                            .with_labels(vec![Label::primary(self.file_id, expr.span())
                                .with_message(format!(
                                    "Expected return type to be {:?}, found {:?}",
                                    ret_type, expr_type
                                ))]),
                    );
                }
            }
        } else if let Some(ret_type) = &self.func_ret_type {
            if ret_type != &Type::Builtin(BuiltinType::Void) {
                self.errors.push(
                    Diagnostic::error()
                        .with_message("Expected return type to match function return type")
                        .with_labels(vec![Label::primary(
                            self.file_id,
                            expr.as_ref().unwrap().span(),
                        )
                        .with_message(format!(
                            "Expected return type to be {:?}, found nothing",
                            ret_type
                        ))]),
                );
            }
        }
        Ok(())
    }

    fn typecheck_block(&mut self, stmts: Vec<Box<Stmt>>) -> Result<(), Box<dyn Error>> {
        self.decls.checkpoint();
        for stmt in stmts {
            self.typecheck_stmt(*stmt)?;
        }
        self.decls.rollback();
        Ok(())
    }

    fn typecheck_var_decl(
        &mut self,
        name: Expr,
        ty: Type,
        value: Option<Box<Expr>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        let stmt_span = span;
        match name {
            Expr::Iden { val, span } => {
                if self.decls.is_declared(&val) {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Variable already declared")
                            .with_labels(vec![
                                Label::primary(self.file_id, stmt_span)
                                    .with_message(format!("Variable {} is already declared", val)),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_var_span(&val).unwrap(),
                                )
                                .with_message("First declaration here"),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_var_span(&val).unwrap(),
                                ),
                                Label::secondary(self.file_id, span)
                                    .with_message("Second declaration here"),
                            ]),
                    );
                }
                if let Some(value) = value {
                    let value_type = self.typecheck_expr(*value.clone())?;
                    if value_type != ty {
                        self.errors.push(
                            Diagnostic::error()
                                .with_message("Expected value type to match variable type")
                                .with_labels(vec![Label::primary(self.file_id, value.span())
                                    .with_message(format!(
                                        "Expected value type to be {:?}, found {:?}",
                                        ty, value_type
                                    ))]),
                        );
                    }
                }
                self.decls.add_var(val, Box::new(ty), span);
            }
            _ => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message("Expected identifier")
                        .with_labels(vec![Label::primary(self.file_id, span)
                            .with_message("Expected an identifier for variable declaration")]),
                );
            }
        }
        Ok(())
    }

    fn typecheck_struct_decl(
        &mut self,
        name: Expr,
        fields: Vec<Box<Stmt>>,
        generics: Option<Vec<Box<GenericType>>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        match name {
            Expr::Iden { val, span } => {
                if self.decls.is_declared(&val) {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Struct already declared")
                            .with_labels(vec![
                                Label::primary(self.file_id, span)
                                    .with_message(format!("Struct {} is already declared", val)),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_struct_span(&val).unwrap(),
                                )
                                .with_message("First declaration here"),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_struct_span(&val).unwrap(),
                                ),
                                Label::secondary(self.file_id, span)
                                    .with_message("Second declaration here"),
                            ]),
                    );
                }
                let generics = generics
                    .unwrap_or_default()
                    .iter()
                    .map(|g| *g.clone())
                    .collect::<Vec<_>>();
                let generics = if generics.is_empty() {
                    None
                } else {
                    Some(generics)
                };
                let mut flds = Vec::new();
                let mut field_names = Vec::new();

                for field in fields {
                    match *field {
                        Stmt::VarDecl {
                            name,
                            ty,
                            value,
                            span,
                        } => {
                            match *name {
                                Expr::Iden { val, span } => {
                                    if field_names.contains(&val) {
                                        self.errors.push(
                                            Diagnostic::error()
                                                .with_message("Struct field already declared")
                                                .with_labels(vec![
                                                    Label::primary(self.file_id, span)
                                                        .with_message(format!(
                                                            "Struct field {} is already declared",
                                                            val
                                                        )),
                                                    Label::secondary(
                                                        self.file_id,
                                                        self.decls.get_var_span(&val).unwrap(),
                                                    )
                                                    .with_message("First declaration here"),
                                                    Label::secondary(
                                                        self.file_id,
                                                        self.decls.get_var_span(&val).unwrap(),
                                                    ),
                                                    Label::secondary(self.file_id, span)
                                                        .with_message("Second declaration here"),
                                                ]),
                                        );
                                    }
                                    if let Some(value) = value {
                                        let value_type = self.typecheck_expr(*value.clone())?;
                                        if value_type != *ty {
                                            self.errors.push(
                                                Diagnostic::error()
                                                    .with_message("Expected value type to match field type")
                                                    .with_labels(vec![Label::primary(
                                                        self.file_id,
                                                        value.span(),
                                                    )
                                                    .with_message(format!(
                                                        "Expected value type to be {:?}, found {:?}",
                                                        ty, value_type
                                                    ))]),
                                            );
                                        }
                                    }
                                    flds.push(VarDeclData {
                                        name: val.clone(),
                                        ty: ty.clone(),
                                        span,
                                    });
                                    field_names.push(val);
                                }
                                _ => {
                                    self.errors.push(
                                        Diagnostic::error()
                                            .with_message("Expected identifier")
                                            .with_labels(vec![Label::primary(self.file_id, span)
                                            .with_message(
                                            "Expected an identifier for struct field declaration",
                                        )]),
                                    );
                                }
                            }
                        }
                        _ => {
                            self.errors.push(
                                Diagnostic::error()
                                    .with_message("Unexpected struct field declaration")
                                    .with_labels(vec![Label::primary(self.file_id, span)
                                        .with_message(
                                            "Expected a variable type pair with optional initial value for struct field",
                                        )]),
                            );
                        }
                    }
                }

                self.decls.add_struct(val, flds, generics, span);
            }
            _ => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message("Expected identifier")
                        .with_labels(vec![Label::primary(self.file_id, span)
                            .with_message("Expected an identifier for struct declaration")]),
                );
            }
        }
        Ok(())
    }

    fn typecheck_impl_decl(
        &mut self,
        ty: Type,
        methods: Vec<Box<Stmt>>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        if self.func_ret_type.is_some() {
            self.errors.push(
                Diagnostic::error()
                    .with_message("impl blocks are only allowed at the top level")
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("This statement is not allowed here")]),
            );
        }
        match ty {
            Type::UserDefined { name, .. } => {
                todo!()
            }
            _ => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message("Expected user defined type")
                        .with_labels(vec![Label::primary(self.file_id, span)
                            .with_message("Expected a user defined type for impl block")]),
                );
            }
        }
        Ok(())
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
        if self.func_ret_type.is_some() {
            self.errors.push(
                Diagnostic::error()
                    .with_message("enum blocks are only allowed at the top level")
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("This statement is not allowed here")]),
            );
        }
        match name {
            Expr::Iden { val, span } => {
                if self.decls.is_declared(&val) {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Enum already declared")
                            .with_labels(vec![
                                Label::primary(self.file_id, span)
                                    .with_message(format!("Enum {} is already declared", val)),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_enum_span(&val).unwrap(),
                                )
                                .with_message("First declaration here"),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_enum_span(&val).unwrap(),
                                ),
                                Label::secondary(self.file_id, span)
                                    .with_message("Second declaration here"),
                            ]),
                    );
                }
                let variants = variants.iter().map(|v| v.clone()).collect::<Vec<_>>();
                if variants.is_empty() {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Enum has no variants")
                            .with_labels(vec![Label::primary(self.file_id, span).with_message(
                                "Expected at least one variant for enum declaration",
                            )]),
                    );
                }
                // Check for duplicate variants
                let mut seen = Vec::new();
                for variant in &variants {
                    if seen.contains(variant) {
                        self.errors.push(
                            Diagnostic::error()
                                .with_message("Duplicate enum variant")
                                .with_labels(vec![Label::primary(self.file_id, span)
                                    .with_message(format!(
                                        "Enum variant {} is already declared",
                                        variant
                                    ))]),
                        );
                    }
                    seen.push(variant.to_string());
                }
                self.decls.add_enum(val, variants, span);
            }
            _ => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message("Expected identifier")
                        .with_labels(vec![Label::primary(self.file_id, span)
                            .with_message("Expected an identifier for enum declaration")]),
                );
            }
        }
        Ok(())
    }

    fn typecheck_union_decl(
        &mut self,
        name: Expr,
        fields: Vec<(Box<Expr>, Type)>,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        match name {
            Expr::Iden { val, span } => {
                if self.decls.is_declared(&val) {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Union already declared")
                            .with_labels(vec![
                                Label::primary(self.file_id, span)
                                    .with_message(format!("Union {} is already declared", val)),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_union_span(&val).unwrap(),
                                )
                                .with_message("First declaration here"),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_union_span(&val).unwrap(),
                                ),
                                Label::secondary(self.file_id, span)
                                    .with_message("Second declaration here"),
                            ]),
                    );
                }
                let mut flds = Vec::new();
                let mut field_names = Vec::new();

                for (field, ty) in fields {
                    match *field {
                        Expr::Iden { val, span } => {
                            if field_names.contains(&val) {
                                self.errors.push(
                                    Diagnostic::error()
                                        .with_message("Union field already declared")
                                        .with_labels(vec![
                                            Label::primary(self.file_id, span).with_message(
                                                format!("Union field {} is already declared", val),
                                            ),
                                            Label::secondary(
                                                self.file_id,
                                                self.decls.get_var_span(&val).unwrap(),
                                            )
                                            .with_message("First declaration here"),
                                            Label::secondary(
                                                self.file_id,
                                                self.decls.get_var_span(&val).unwrap(),
                                            ),
                                            Label::secondary(self.file_id, span)
                                                .with_message("Second declaration here"),
                                        ]),
                                );
                            }
                            flds.push(VarDeclData {
                                name: val.clone(),
                                ty: Box::new(ty.clone()),
                                span,
                            });
                            field_names.push(val);
                        }
                        _ => {
                            self.errors.push(
                                Diagnostic::error()
                                    .with_message("Expected identifier")
                                    .with_labels(vec![Label::primary(self.file_id, span)
                                        .with_message(
                                            "Expected an identifier for union field declaration",
                                        )]),
                            );
                        }
                    }
                }
                self.decls.add_union(val, flds, span);
            }
            _ => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message("Expected identifier")
                        .with_labels(vec![Label::primary(self.file_id, span)
                            .with_message("Expected an identifier for union declaration")]),
                );
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
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
        if self.func_ret_type.is_some() {
            self.errors.push(
                Diagnostic::error()
                    .with_message("Function declarations are only allowed at the top level")
                    .with_labels(vec![Label::primary(self.file_id, span)
                        .with_message("This statement is not allowed here")]),
            );
        }
        match name {
            Expr::Iden { val, span } => {
                if self.decls.is_declared(&val) {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Function already declared")
                            .with_labels(vec![
                                Label::primary(self.file_id, span)
                                    .with_message(format!("Function {} is already declared", val)),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_function_span(&val).unwrap(),
                                )
                                .with_message("First declaration here"),
                                Label::secondary(
                                    self.file_id,
                                    self.decls.get_function_span(&val).unwrap(),
                                ),
                                Label::secondary(self.file_id, span)
                                    .with_message("Second declaration here"),
                            ]),
                    );
                }
                let generics = generics
                    .unwrap_or_default()
                    .iter()
                    .map(|g| *g.clone())
                    .collect::<Vec<_>>();
                let generics = if generics.is_empty() {
                    None
                } else {
                    Some(generics)
                };

                let mut arg_names = Vec::new();
                let mut arg_types = Vec::new();
                let mut arg_arrays = Vec::new();

                for arg in args {
                    match *arg {
                        Stmt::VarDecl {
                            name,
                            ty,
                            value,
                            span,
                        } => {
                            match *name {
                                Expr::Iden { val, span } => {
                                    if arg_names.contains(&val) {
                                        self.errors.push(
                                            Diagnostic::error()
                                                .with_message("Function argument already declared")
                                                .with_labels(vec![
                                                    Label::primary(self.file_id, span)
                                                        .with_message(format!(
                                                        "Function argument {} is already declared",
                                                        val
                                                    )),
                                                    Label::secondary(
                                                        self.file_id,
                                                        self.decls.get_var_span(&val).unwrap(),
                                                    )
                                                    .with_message("First declaration here"),
                                                    Label::secondary(
                                                        self.file_id,
                                                        self.decls.get_var_span(&val).unwrap(),
                                                    ),
                                                    Label::secondary(self.file_id, span)
                                                        .with_message("Second declaration here"),
                                                ]),
                                        );
                                    }
                                    if let Some(value) = value {
                                        let value_type = self.typecheck_expr(*value.clone())?;
                                        if value_type != *ty {
                                            self.errors.push(
                                                Diagnostic::error()
                                                    .with_message("Expected value type to match argument type")
                                                    .with_labels(vec![Label::primary(
                                                        self.file_id,
                                                        value.span(),
                                                    )
                                                    .with_message(format!(
                                                        "Expected value type to be {:?}, found {:?}",
                                                        ty, value_type
                                                    ))]),
                                            );
                                        }
                                    }
                                    arg_names.push(val);
                                    arg_types.push(ty);
                                    arg_arrays.push(VarDeclData {
                                        name: val.clone(),
                                        ty: ty.clone(),
                                        span,
                                    });
                                }
                                _ => {
                                    self.errors.push(
                                        Diagnostic::error()
                                            .with_message("Expected identifier")
                                            .with_labels(vec![Label::primary(self.file_id, span)
                                            .with_message(
                                            "Expected an identifier for function argument declaration",
                                        )]),
                                    );
                                }
                            }
                        }
                        _ => {
                            self.errors.push(
                                Diagnostic::error()
                                    .with_message("Unexpected function argument declaration")
                                    .with_labels(vec![Label::primary(self.file_id, span)
                                        .with_message(
                                            "Expected a variable type pair with optional initial value for function argument",
                                        )]),
                            );
                        }
                    }
                }

                if let Some(body) = body {
                    self.func_ret_type = Some(ret.clone());
                    self.decls.add_function(
                        val,
                        generics,
                        arg_arrays,
                        Box::new(ret),
                        isvararg,
                        span,
                    );
                    self.decls.checkpoint();
                    self.typecheck_stmt(*body)?;
                    self.decls.rollback();
                    self.func_ret_type = None;
                } else {
                    self.decls.add_function(
                        val,
                        generics,
                        arg_arrays,
                        Box::new(ret),
                        isvararg,
                        span,
                    );
                }
            }
            _ => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message("Expected identifier")
                        .with_labels(vec![Label::primary(self.file_id, span)
                            .with_message("Expected an identifier for function declaration")]),
                );
            }
        }
        Ok(())
    }

    fn typecheck_var_assign(
        &mut self,
        name: Expr,
        value: Expr,
        _op: AssignOp,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        match name {
            Expr::Iden { val, span } => {
                if !self.decls.is_declared(&val) {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Variable not declared")
                            .with_labels(vec![Label::primary(self.file_id, span)
                                .with_message(format!("Variable {} is not declared", val))]),
                    );
                }
                let var_type = self.decls.get_var_type(&val).unwrap();
                let value_type = self.typecheck_expr(value.clone())?;

                if value_type != *var_type {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Expected value type to match variable type")
                            .with_labels(vec![Label::primary(self.file_id, value.span())
                                .with_message(format!(
                                    "Expected value type to be {:?}, found {:?}",
                                    var_type, value_type
                                ))]),
                    );
                }
            }
            _ => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message("Expected identifier")
                        .with_labels(vec![Label::primary(self.file_id, span)
                            .with_message("Expected an identifier for variable assignment")]),
                );
            }
        }
        Ok(())
    }

    fn typecheck_deref_assign(
        &mut self,
        value: Expr,
        expr: Expr,
        _op: AssignOp,
        span: Span,
    ) -> Result<(), Box<dyn Error>> {
        match value {
            Expr::Iden { val, span } => {
                if !self.decls.is_declared(&val) {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Variable not declared")
                            .with_labels(vec![Label::primary(self.file_id, span)
                                .with_message(format!("Variable {} is not declared", val))]),
                    );
                }
                let var_type = self.decls.get_var_type(&val).unwrap();
                let expr_type = self.typecheck_expr(expr.clone())?;

                if *var_type != Type::Pointer(Box::new(expr_type.clone())) {
                    self.errors.push(
                        Diagnostic::error()
                            .with_message("Expected value type to match variable type")
                            .with_labels(vec![Label::primary(self.file_id, expr.span())
                                .with_message(format!(
                                    "Expected value type to be {:?}, found {:?}",
                                    var_type, expr_type
                                ))]),
                    );
                }
            }
            _ => {
                self.errors.push(
                    Diagnostic::error()
                        .with_message("Expected identifier")
                        .with_labels(vec![Label::primary(self.file_id, span)
                            .with_message("Expected an identifier for dereference assignment")]),
                );
            }
        }
        Ok(())
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
        if !self.in_loop {
            self.errors.push(
                Diagnostic::error()
                    .with_message("break statements are only allowed inside loops")
                    .with_labels(vec![Label::primary(self.file_id, 0..0)
                        .with_message("This statement is not allowed here")]),
            );
        }
        Ok(())
    }

    fn typecheck_continue(&mut self) -> Result<(), Box<dyn Error>> {
        if !self.in_loop {
            self.errors.push(
                Diagnostic::error()
                    .with_message("continue statements are only allowed inside loops")
                    .with_labels(vec![Label::primary(self.file_id, 0..0)
                        .with_message("This statement is not allowed here")]),
            );
        }
        Ok(())
    }

    fn typecheck_expr(&mut self, expr: Expr) -> Result<Type, Box<dyn Error>> {
        todo!()
    }
}
