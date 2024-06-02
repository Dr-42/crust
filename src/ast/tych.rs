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
    decldata::{DeclData, FunctionDeclData, StructDeclData, VarDeclData},
    nodes::{BinaryOp, BuiltinType, Expr, Program, Stmt, Type, UnaryOp},
    Span,
};

pub struct TychContext {
    decl_data: DeclData,
    file_id: usize,
    current_fn_return_type: Option<Type>,
    in_loop: bool,
    impl_type: Option<StructDeclData>,
}

impl TychContext {
    pub fn new(file_id: usize) -> Self {
        Self {
            decl_data: DeclData::new(),
            file_id,
            current_fn_return_type: None,
            in_loop: false,
            impl_type: None,
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

    pub fn tych_program(&mut self, program: Program) -> Result<(), Diagnostic<usize>> {
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
                } else if self.current_fn_return_type.clone().unwrap()
                    != Type::Builtin(BuiltinType::Void)
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
                    self.decl_data.add(&stmt);
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
                            eprintln!("---\n{:?}\n {:?}\n---\n", ty, value_ty);
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
            Stmt::ImplDecl {
                name,
                methods,
                span,
            } => {
                let struct_data = self.decl_data.struct_.clone();
                let ty_name = match *name {
                    Expr::Iden { val, span } => val,
                    _ => {
                        return Err(
                            self.create_error("Expected identifier for impl declaration", span)
                        )
                    }
                };
                if let Some(struct_type) = struct_data.iter().find(|s| s.name == ty_name) {
                    self.impl_type = Some(struct_type.clone());
                } else {
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
                self.decl_data.push_scope();
                for arg in args {
                    self.tych_stmt(*arg.clone())?;
                    match *arg.clone() {
                        Stmt::VarDecl {
                            name,
                            qualifiers,
                            ty,
                            value,
                            span,
                        } => {
                            let var_name = match *name {
                                Expr::Iden { val, span } => val,
                                _ => {
                                    return Err(self.create_error(
                                        "Expected identifier for function arguments",
                                        span,
                                    ))
                                }
                            };
                            let var_ty = *ty;
                            self.decl_data.add(&arg.clone())
                        }
                        _ => {
                            return Err(self.create_error(
                                "Expected variable declaration for function arguments",
                                span,
                            ))
                        }
                    }
                }
                if let Some(body) = body {
                    self.tych_stmt(*body)?;
                }
                self.decl_data.pop_scope();
                self.current_fn_return_type = None;
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
                if value_ty != expr_ty {
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
                let struct_data = if let Some(ty) = ty {
                    match *ty {
                        Type::UserDefined { name, .. } => match *name {
                            Expr::Iden { val, span } => {
                                self.decl_data.struct_.iter().find(|s| s.name == val)
                            }
                            _ => {
                                return Err(self.create_error(
                                    "Expected identifier for struct assignment",
                                    span,
                                ));
                            }
                        },
                        Type::Builtin(BuiltinType::Slf) => {
                            if let Some(struct_data) = self.impl_type.as_ref() {
                                Some(struct_data)
                            } else {
                                return Err(self.create_error("No struct type found", span));
                            }
                        }
                        _ => {
                            return Err(self.create_error(
                                "Expected user defined type for struct assignment",
                                span,
                            ));
                        }
                    }
                } else {
                    match *name {
                        Expr::Iden { val, span } => {
                            self.decl_data.struct_.iter().find(|s| s.name == val)
                        }
                        _ => {
                            return Err(self
                                .create_error("Expected identifier for struct assignment", span));
                        }
                    }
                };
                if let Some(struct_data) = struct_data {
                    for field in fields {
                        let field_name = *field.0;
                        let field_value = *field.1;

                        let (field_name, span) = match field_name {
                            Expr::Iden { val, span } => (val, span),
                            _ => {
                                return Err(
                                    self.create_error("Expected identifier for struct field", span)
                                )
                            }
                        };

                        let field_data = struct_data.fields.iter().find(|f| f.name == field_name);
                        if let Some(field_data) = field_data {
                            let field_ty = field_data.clone().ty;
                            let field_value_ty = self.tych_expr(field_value)?;
                            if *field_ty != field_value_ty {
                                return Err(
                                    self.create_error("Type mismatch in struct assignment", span)
                                );
                            }
                        } else {
                            return Err(self.create_error("Field not found", span));
                        }
                    }
                } else {
                    return Err(self.create_error("Struct not found", span));
                }
            }
            Stmt::StructMemberAssign {
                name,
                value,
                op,
                span,
            } => match *name {
                Expr::MemberAccess { .. } => {
                    let member_ty = self.tych_expr(*name)?;
                    let value_ty = self.tych_expr(*value)?;
                    if member_ty != value_ty {
                        return Err(
                            self.create_error("Type mismatch in struct member assignment", span)
                        );
                    }
                }
                _ => {
                    return Err(
                        self.create_error("Expected member access for struct assignment", span)
                    )
                }
            },
            Stmt::ArrayMemberAssign {
                element,
                value,
                op,
                span,
            } => {
                let element_ty = self.tych_expr(*element)?;
                let value_ty = self.tych_expr(*value)?;
                if element_ty != value_ty {
                    return Err(self.create_error("Type mismatch in array member assignment", span));
                }
            }
            Stmt::TraitAssign {
                name,
                for_ty,
                methods,
                span,
            } => {
                let trait_name = match *name {
                    Expr::Iden { val, span } => val,
                    _ => {
                        return Err(
                            self.create_error("Expected identifier for trait assignment", span)
                        )
                    }
                };
                let trait_data = self.decl_data.trait_.iter().find(|t| t.name == trait_name);
                if let Some(trait_data) = trait_data {
                    match *for_ty {
                        Type::UserDefined { name, .. } => match *name {
                            Expr::Iden { val, span } => {
                                let struct_name = val;
                                let struct_data = self
                                    .decl_data
                                    .struct_
                                    .iter()
                                    .find(|s| s.name == struct_name);
                                if let Some(struct_data) = struct_data {
                                    self.impl_type = Some(struct_data.clone());
                                } else {
                                    return Err(self.create_error("Struct not found", span));
                                }
                            }
                            _ => {
                                return Err(
                                    self.create_error("Expected identifier for struct", span)
                                )
                            }
                        },
                        _ => {
                            return Err(self.create_error("Expected user defined type", span));
                        }
                    }
                    for method in methods {
                        self.tych_stmt(*method)?;
                    }
                } else {
                    return Err(self.create_error("Trait not found", span));
                }
            }
            Stmt::Match { expr, cases, span } => {
                let expr_ty = self.tych_expr(*expr)?;
                for case in cases {
                    let case = *case;
                    let pat = case.pattern;
                    let body = case.body;
                    let pat_type = self.tych_expr(*pat)?;
                    if pat_type != expr_ty {
                        return Err(self.create_error("Type mismatch in match pattern", span));
                    }
                    self.tych_stmt(*body)?;
                }
            }
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
            Stmt::Comment(span) => {}
        };
        Ok(())
    }

    pub fn tych_expr(&self, expr: Expr) -> Result<Type, Diagnostic<usize>> {
        match expr {
            Expr::Numeric { val, span } => Ok(Type::Builtin(BuiltinType::I32)),
            Expr::Strng { val, span } => Ok(Type::Builtin(BuiltinType::Str)),
            Expr::Flt { val, span } => Ok(Type::Builtin(BuiltinType::F32)),
            Expr::Chr { val, span } => Ok(Type::Builtin(BuiltinType::Chr)),
            Expr::Bln { val, span } => Ok(Type::Builtin(BuiltinType::Bln)),
            Expr::Iden { val, span } => {
                let var_data = self.decl_data.var.iter().find(|v| v.name == val);
                if let Some(var_data) = var_data {
                    Ok(var_data.ty.as_ref().clone())
                } else if let Some(fn_data) = self.decl_data.function.iter().find(|f| f.name == val)
                {
                    let fn_args = fn_data.args.clone().iter().map(|a| a.ty.clone()).collect();
                    Ok(Type::FnPtr {
                        args: fn_args,
                        ret: Box::new(*fn_data.ret.clone()),
                    })
                } else {
                    Err(self.create_error("Variable not found", span))
                }
            }
            Expr::UnaryOp { op, expr, span } => {
                let expr_ty = self.tych_expr(*expr)?;
                match op {
                    UnaryOp::Ref => Ok(Type::Pointer(Box::new(expr_ty))),
                    UnaryOp::Deref => match expr_ty {
                        Type::Pointer(ty) => Ok(*ty),
                        _ => Err(self.create_error("Expected pointer type", span)),
                    },
                    _ => Ok(expr_ty),
                }
            }
            Expr::BinaryOp { lhs, op, rhs, span } => {
                let lhs_ty = self.tych_expr(*lhs)?;
                let rhs_ty = self.tych_expr(*rhs)?;
                self.check_compatible_type(lhs_ty, rhs_ty, op, span)
            }
            Expr::Call {
                name,
                args,
                generics,
                span,
            } => {
                let fn_name = match *name {
                    Expr::Iden { val, span } => val,
                    _ => {
                        return Err(self.create_error("Expected identifier for function call", span))
                    }
                };
                if let Some(fn_data) = self.decl_data.function.iter().find(|f| f.name == fn_name) {
                    let mut arg_types = vec![];
                    for arg in args {
                        let arg_ty = self.tych_expr(*arg)?;
                        arg_types.push(arg_ty);
                    }
                    if fn_data.variadic {
                        if fn_data.args.len() > arg_types.len() {
                            return Err(self.create_error(
                                "Argument count mismatch in variadic function",
                                span,
                            ));
                        }
                    } else if fn_data.args.len() != arg_types.len() {
                        return Err(self.create_error("Argument count mismatch", span));
                    }
                    for (arg_ty, fn_arg_ty) in arg_types.iter().zip(fn_data.args.iter()) {
                        if *arg_ty != *fn_arg_ty.ty {
                            return Err(self.create_error("Argument type mismatch", span));
                        }
                    }
                    Ok(fn_data.ret.as_ref().clone())
                } else if let Some(fn_ptr_data) = self
                    .decl_data
                    .var
                    .iter()
                    .find(|v| matches!(*v.ty, Type::FnPtr { .. }))
                {
                    let fn_ptr_ty = *fn_ptr_data.ty.clone();
                    match fn_ptr_ty {
                        Type::FnPtr {
                            args: ptr_args,
                            ret,
                        } => {
                            let mut arg_types = vec![];
                            for arg in args.clone() {
                                let arg_ty = self.tych_expr(*arg)?;
                                arg_types.push(arg_ty);
                            }
                            if args.len() != arg_types.len() {
                                return Err(self.create_error("Argument count mismatch", span));
                            }
                            for (arg_ty, fn_arg_ty) in arg_types.iter().zip(ptr_args.iter()) {
                                if *arg_ty != **fn_arg_ty {
                                    return Err(self.create_error("Argument type mismatch", span));
                                }
                            }
                            Ok(*ret)
                        }
                        _ => Err(self.create_error("Expected function pointer", span)),
                    }
                } else {
                    Err(self.create_error("Function not found", span))
                }
            }
            Expr::Index {
                name,
                indices,
                span,
            } => match *name {
                Expr::Iden { val, span } => {
                    if let Some(var_data) = self.decl_data.var.iter().find(|v| v.name == val) {
                        let var_ty = var_data.ty.clone();
                        match *var_ty {
                            Type::Array { base, lens } => {
                                if lens.len() == indices.len() {
                                    Ok(*base)
                                } else {
                                    Err(self.create_error("Trying to assign sublevel array", span))
                                }
                            }
                            Type::Pointer(base) => {
                                let mut indices = indices.clone();
                                indices.reverse();
                                let mut var_ty = base.clone();
                                let mut base = base;
                                while let Some(index) = indices.pop() {
                                    let index_ty = self.tych_expr(*index)?;
                                    if !Self::is_int(index_ty) {
                                        return Err(
                                            self.create_error("Expected integer index", span)
                                        );
                                    }
                                    if indices.is_empty() {
                                        return Ok(*base);
                                    }
                                    match *base {
                                        Type::Pointer(bs) => {
                                            base = bs;
                                        }
                                        _ => {
                                            return Err(self.create_error(
                                                "Trying to index beyond pointer depth",
                                                span,
                                            ))
                                        }
                                    }
                                }
                                Err(self.create_error(
                                    "Pointer depth is less than amount indexed",
                                    span,
                                ))
                            }
                            _ => Err(self.create_error("Expected array or pointer type", span)),
                        }
                    } else {
                        Err(self.create_error("Variable not found", span))
                    }
                }
                _ => Err(self.create_error("Expected identifier for index access", span)),
            },
            Expr::MemberAccess {
                strct,
                member,
                span,
            } => {
                let struct_data = match *strct {
                    Expr::Iden { val, span } => {
                        if val == "self" {
                            if let Some(struct_data) = self.impl_type.as_ref() {
                                struct_data
                            } else {
                                return Err(self.create_error("No struct type found", span));
                            }
                        } else if let Some(struct_var) =
                            self.decl_data.var.iter().find(|v| v.name == val)
                        {
                            match *struct_var.ty.clone() {
                                Type::UserDefined { name, .. } => {
                                    let struct_name = match *name {
                                        Expr::Iden { val, span } => val,
                                        _ => {
                                            return Err(self.create_error(
                                                "Expected identifier for struct access",
                                                span,
                                            ))
                                        }
                                    };
                                    let struct_find = self
                                        .decl_data
                                        .struct_
                                        .iter()
                                        .find(|s| s.name == struct_name);
                                    if let Some(struct_data) = struct_find {
                                        struct_data
                                    } else {
                                        return Err(self.create_error("Struct not found", span));
                                    }
                                }
                                Type::Builtin(BuiltinType::Slf) => {
                                    if let Some(struct_data) = self.impl_type.as_ref() {
                                        struct_data
                                    } else {
                                        return Err(self.create_error("No struct type found", span));
                                    }
                                }
                                _ => {
                                    return Err(self.create_error(
                                        "Expected user defined type for struct access",
                                        span,
                                    ));
                                }
                            }
                        } else {
                            return Err(self.create_error("Struct not found", span));
                        }
                    }
                    Expr::MemberAccess { .. } => {
                        let str_ty = self.tych_expr(*strct.clone())?;
                        match str_ty {
                            Type::UserDefined { name, .. } => {
                                let struct_name = match *name {
                                    Expr::Iden { val, span } => val,
                                    _ => {
                                        return Err(self.create_error(
                                            "Expected identifier for struct access",
                                            span,
                                        ))
                                    }
                                };
                                let struct_find = self
                                    .decl_data
                                    .struct_
                                    .iter()
                                    .find(|s| s.name == struct_name);
                                if let Some(struct_data) = struct_find {
                                    struct_data
                                } else {
                                    return Err(self.create_error("Struct not found", span));
                                }
                            }
                            _ => {
                                return Err(self.create_error(
                                    "Expected user defined type for struct access",
                                    span,
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(self.create_error(
                            "Expected identifier or memeber access for struct access",
                            span,
                        ));
                    }
                };
                match *member {
                    Expr::Iden { val, span } => {
                        let member_name = val;
                        let member_data = struct_data.fields.iter().find(|f| f.name == member_name);
                        if let Some(member_data) = member_data {
                            Ok(*member_data.ty.clone())
                        } else {
                            Err(self.create_error("Member not found", span))
                        }
                    }
                    Expr::Call {
                        name,
                        args,
                        generics,
                        span,
                    } => {
                        let fn_name = match *name {
                            Expr::Iden { val, span } => val,
                            _ => {
                                return Err(self.create_error(
                                    "Expected identifier for member function call",
                                    span,
                                ))
                            }
                        };
                        if let Some(mut fn_data) =
                            struct_data.methods.iter().find(|m| m.name == fn_name)
                        {
                            let mut fn_data = fn_data.clone();
                            let box_built_in = Box::new(Type::Builtin(BuiltinType::Slf));
                            let ptr_built_in = Type::Pointer(box_built_in);
                            if let Some(first_arg) = fn_data.args.first() {
                                if matches!(&first_arg.ty, ptr_built_in) {
                                    fn_data.args.remove(0);
                                } else {
                                    return Err(
                                        self.create_error("Expected self as first argument", span)
                                    );
                                }
                            } else {
                                return Err(
                                    self.create_error("Expected self as first argument", span)
                                );
                            }
                            let mut arg_types = vec![];
                            for arg in args {
                                let arg_ty = self.tych_expr(*arg)?;
                                arg_types.push(arg_ty);
                            }
                            if fn_data.variadic {
                                if fn_data.args.len() > arg_types.len() {
                                    return Err(self.create_error(
                                        "Argument count mismatch in variadic function",
                                        span,
                                    ));
                                }
                            } else if fn_data.args.len() != arg_types.len() {
                                return Err(self.create_error("Argument count mismatch", span));
                            }
                            for (arg_ty, fn_arg_ty) in arg_types.iter().zip(fn_data.args.iter()) {
                                if *arg_ty != *fn_arg_ty.ty {
                                    return Err(self.create_error("Argument type mismatch", span));
                                }
                            }
                            Ok(fn_data.ret.as_ref().clone())
                        } else {
                            Err(self.create_error("Function not found", span))
                        }
                    }
                    _ => Err(self.create_error("Expected identifier for member access", span)),
                }
            }
            Expr::ArrayLiteral { vals, span } => {
                let mut single_val_type = None;
                for val in vals.clone() {
                    let val_ty = self.tych_expr(*val)?;
                    if single_val_type.is_none() {
                        single_val_type = Some(val_ty);
                    } else if single_val_type != Some(val_ty) {
                        return Err(self.create_error("Array literal not uniform in type", span));
                    }
                }
                match single_val_type.clone().unwrap() {
                    Type::Array { base, lens } => {
                        let mut new_lens = lens.clone();
                        new_lens.insert(0, vals.len());
                        Ok(Type::Array {
                            base,
                            lens: new_lens,
                        })
                    }
                    _ => Ok(Type::Array {
                        base: Box::new(single_val_type.unwrap()),
                        lens: vec![vals.len()],
                    }),
                }
            }
            Expr::EnumLiteral {
                name,
                variant,
                span,
            } => {
                let enum_name = match *name {
                    Expr::Iden { val, span } => val,
                    _ => {
                        return Err(self.create_error("Expected identifier for enum literal", span))
                    }
                };
                let enum_data = self.decl_data.enum_.iter().find(|e| e.name == enum_name);
                if let Some(enum_data) = enum_data {
                    let variant_name = match *variant {
                        Expr::Iden { val, span } => val,
                        _ => {
                            return Err(
                                self.create_error("Expected identifier for enum variant", span)
                            )
                        }
                    };
                    let variant_data = enum_data.variants.iter().find(|v| **v == variant_name);
                    if let Some(variant_data) = variant_data {
                        Ok(Type::UserDefined {
                            name: Box::new(Expr::Iden {
                                val: enum_name,
                                span,
                            }),
                            generic_args: None,
                        })
                    } else {
                        Err(self.create_error("Variant not found", span))
                    }
                } else {
                    Err(self.create_error("Enum not found", span))
                }
            }
        }
    }

    fn is_int(ty: Type) -> bool {
        matches!(
            ty,
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
    fn is_float(ty: Type) -> bool {
        matches!(
            ty,
            Type::Builtin(BuiltinType::F32) | Type::Builtin(BuiltinType::F64)
        )
    }
    fn is_pointer(ty: Type) -> bool {
        matches!(ty, Type::Pointer(_))
    }

    fn is_bool(ty: Type) -> bool {
        matches!(ty, Type::Builtin(BuiltinType::Bln))
    }

    pub fn check_compatible_type(
        &self,
        ty1: Type,
        ty2: Type,
        bin_op: BinaryOp,
        span: Span,
    ) -> Result<Type, Diagnostic<usize>> {
        if matches!(ty1, Type::UserDefined { .. }) && matches!(ty2, Type::UserDefined { .. }) {
            // TODO: Quick hack for passing this. Need to parse traits and impls
            return Ok(ty1);
        }
        match bin_op {
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Add | BinaryOp::Sub => {
                if Self::is_int(ty1.clone()) && Self::is_int(ty2.clone())
                    || Self::is_float(ty1.clone()) && Self::is_float(ty2.clone())
                    || Self::is_float(ty1.clone()) && Self::is_int(ty2.clone())
                {
                    Ok(ty1)
                } else if Self::is_int(ty1.clone()) && Self::is_float(ty2.clone()) {
                    Ok(ty2)
                } else {
                    let op_str = match bin_op {
                        BinaryOp::Mul => "*",
                        BinaryOp::Div => "/",
                        BinaryOp::Add => "+",
                        BinaryOp::Sub => "-",
                        _ => "",
                    };
                    let err_msg = format!(
                        "Type mismatch when doing {} for types: {:?} and {:?}",
                        op_str, ty1, ty2
                    );
                    Err(self.create_error(&err_msg, span))
                }
            }
            BinaryOp::Mod | BinaryOp::BitXor | BinaryOp::BitAnd | BinaryOp::BitOr => {
                if Self::is_int(ty1.clone()) && Self::is_int(ty2.clone()) {
                    Ok(ty1)
                } else {
                    let op_str = match bin_op {
                        BinaryOp::Mod => "%",
                        BinaryOp::BitXor => "^",
                        BinaryOp::BitAnd => "&",
                        BinaryOp::BitOr => "|",
                        _ => "",
                    };
                    let err_msg = format!(
                        "Type mismatch when doing {} for types: {:?} and {:?}",
                        op_str, ty1, ty2
                    );
                    Err(self.create_error(&err_msg, span))
                }
            }
            BinaryOp::Shl | BinaryOp::Shr => {
                if Self::is_int(ty1.clone()) && Self::is_int(ty2.clone()) {
                    Ok(ty1)
                } else {
                    let op_str = match bin_op {
                        BinaryOp::Shl => "<<",
                        BinaryOp::Shr => ">>",
                        _ => "",
                    };
                    let err_msg = format!(
                        "Type mismatch when doing {} for types: {:?} and {:?}",
                        op_str, ty1, ty2
                    );
                    Err(self.create_error(&err_msg, span))
                }
            }
            BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Lte | BinaryOp::Gte => {
                if Self::is_int(ty1.clone()) && Self::is_int(ty2.clone())
                    || Self::is_float(ty1.clone()) && Self::is_float(ty2.clone())
                {
                    Ok(Type::Builtin(BuiltinType::Bln))
                } else {
                    let op_str = match bin_op {
                        BinaryOp::Lt => "<",
                        BinaryOp::Gt => ">",
                        BinaryOp::Lte => "<=",
                        BinaryOp::Gte => ">=",
                        _ => "",
                    };
                    let err_msg = format!(
                        "Type mismatch when doing {} for types: {:?} and {:?}",
                        op_str, ty1, ty2
                    );
                    Err(self.create_error(&err_msg, span))
                }
            }
            BinaryOp::Eq | BinaryOp::Neq => {
                if Self::is_int(ty1.clone()) && Self::is_int(ty2.clone())
                    || Self::is_pointer(ty1.clone()) && Self::is_pointer(ty2.clone())
                    || Self::is_bool(ty1.clone()) && Self::is_bool(ty2.clone())
                {
                    Ok(Type::Builtin(BuiltinType::Bln))
                } else {
                    let op_str = match bin_op {
                        BinaryOp::Eq => "==",
                        BinaryOp::Neq => "!=",
                        _ => "",
                    };
                    let err_msg = format!(
                        "Type mismatch when doing {} for types: {:?} and {:?}",
                        op_str, ty1, ty2
                    );
                    Err(self.create_error(&err_msg, span))
                }
            }
            BinaryOp::And | BinaryOp::Or => {
                if Self::is_bool(ty1.clone()) && Self::is_bool(ty2.clone()) {
                    Ok(Type::Builtin(BuiltinType::Bln))
                } else {
                    let op_str = match bin_op {
                        BinaryOp::And => "&&",
                        BinaryOp::Or => "||",
                        _ => "",
                    };
                    let err_msg = format!(
                        "Type mismatch when doing {} for types: {:?} and {:?}",
                        op_str, ty1, ty2
                    );
                    Err(self.create_error(&err_msg, span))
                }
            }
        }
    }
}
