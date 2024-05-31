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
    decldata::{DeclData, StructDeclData, VarDeclData},
    nodes::{BinaryOp, BuiltinType, Expr, Program, Stmt, Type},
    Span,
};

pub struct TychContext {
    decl_data: DeclData,
    file_id: usize,
    current_fn_return_type: Option<Type>,
    current_fn_args: Vec<VarDeclData>,
    in_loop: bool,
    impl_type: Option<StructDeclData>,
}

impl TychContext {
    pub fn new(file_id: usize) -> Self {
        Self {
            decl_data: DeclData::new(),
            file_id,
            current_fn_return_type: None,
            current_fn_args: vec![],
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
                for arg in args {
                    self.tych_stmt(*arg.clone())?;
                    match *arg {
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
                            self.current_fn_args.push(VarDeclData {
                                name: var_name,
                                ty: Box::new(var_ty),
                            });
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
                self.current_fn_return_type = None;
                self.current_fn_args = vec![];
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
                Expr::MemberAccess {
                    name,
                    members,
                    span,
                } => match *name {
                    Expr::Iden { val, span } => {
                        if let Some(var_data) = self.decl_data.var.iter().find(|v| v.name == val) {
                            let struct_type = match *var_data.clone().ty {
                                Type::UserDefined { name, .. } => match *name {
                                    Expr::Iden { val, span } => val,
                                    _ => return Err(self.create_error("Expected identifier", span)),
                                },
                                _ => return Err(self.create_error("Expected struct type", span)),
                            };
                            let struct_data = self
                                .decl_data
                                .struct_
                                .iter()
                                .find(|s| s.name == struct_type);
                            if let Some(mut struct_data_1) = struct_data {
                                let mut members = members.clone();
                                members.reverse();
                                while let Some(member) = members.pop() {
                                    let mut span_new;
                                    let member_name = match *member {
                                        Expr::Iden { val, span } => {
                                            span_new = span;
                                            val
                                        }
                                        _ => {
                                            return Err(self.create_error(
                                                "Expected identifier for member",
                                                span,
                                            ))
                                        }
                                    };
                                    let member_data =
                                        struct_data_1.fields.iter().find(|f| f.name == member_name);
                                    if let Some(member_data) = member_data {
                                        let member_ty = member_data.clone().ty;
                                        // Check if this is the last member
                                        if members.is_empty() {
                                            let value_ty = self.tych_expr(*value.clone())?;
                                            if *member_ty != value_ty {
                                                return Err(self.create_error(
                                                    "Type mismatch in struct member assignment",
                                                    span,
                                                ));
                                            }
                                        } else {
                                            // Find the field in the struct
                                            let field_data = struct_data_1
                                                .fields
                                                .iter()
                                                .find(|f| f.name == member_name);
                                            if let Some(field_data) = field_data {
                                                let field_ty = field_data.clone().ty;
                                                match *field_ty {
                                                    Type::UserDefined { name, .. } => match *name {
                                                        Expr::Iden { val, span } => {
                                                            let struct_name = val;
                                                            let new_struct_data =
                                                                self.decl_data.struct_.iter().find(
                                                                    |s| s.name == struct_name,
                                                                );
                                                            if let Some(new_struct_data) =
                                                                new_struct_data
                                                            {
                                                                struct_data_1 = new_struct_data;
                                                                continue;
                                                            } else {
                                                                return Err(self.create_error(
                                                                    "Struct not found",
                                                                    span,
                                                                ));
                                                            }
                                                        }
                                                        _ => {
                                                            return Err(self.create_error(
                                                                    "Expected identifier for struct member assignment",
                                                                    span,
                                                                ));
                                                        }
                                                    },
                                                    _ => {
                                                        return Err(self.create_error(
                                                            "Expected struct type",
                                                            span,
                                                        ));
                                                    }
                                                }
                                            } else {
                                                return Err(self.create_error(
                                                    "Member not found in struct",
                                                    span,
                                                ));
                                            }
                                        }
                                    } else {
                                        let err_msg = format!(
                                            "Member {} not found for struct {}",
                                            member_name, struct_data_1.name
                                        );
                                        return Err(self.create_error(&err_msg, span_new));
                                    }
                                }
                            } else {
                                return Err(self.create_error("Struct not found", span));
                            }
                        } else {
                            return Err(self.create_error("Variable not found", span));
                        }
                    }
                    _ => {
                        return Err(self.create_error(
                            "Expected identifier for struct member assignment",
                            span,
                        ))
                    }
                },
                _ => {
                    return Err(self
                        .create_error("Expected member access for struct member assignment", span))
                }
            },
            Stmt::ArrayMemberAssign {
                element,
                value,
                op,
                span,
            } => {
                return Err(self.create_error("Not yet handled", span));
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
                todo!();
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
                } else {
                    let fn_arg = self
                        .current_fn_args
                        .iter()
                        .find(|a| a.name == val)
                        .map(|a| a.ty.clone());
                    if let Some(fn_arg) = fn_arg {
                        Ok(*fn_arg)
                    } else {
                        Err(self.create_error("Variable not found", span))
                    }
                }
            }
            Expr::UnaryOp { op, expr, span } => Err(self.create_error("Not yet handled", span)),
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
                let fn_data = self.decl_data.function.iter().find(|f| f.name == fn_name);
                if let Some(fn_data) = fn_data {
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
            Expr::Index {
                name,
                indices,
                span,
            } => Err(self.create_error("Not yet handled", span)),
            Expr::MemberAccess {
                name,
                members,
                span,
            } => Err(self.create_error("Not yet handled", span)),
            Expr::ArrayLiteral { vals, span } => Err(self.create_error("Not yet handled", span)),
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
                    Err(self.create_error("Type mismatch", span))
                }
            }
            BinaryOp::Mod | BinaryOp::BitXor | BinaryOp::BitAnd | BinaryOp::BitOr => {
                if Self::is_int(ty1.clone()) && Self::is_int(ty2.clone()) {
                    Ok(ty1)
                } else {
                    Err(self.create_error("Type mismatch", span))
                }
            }
            BinaryOp::Shl | BinaryOp::Shr => {
                if Self::is_int(ty1.clone()) && Self::is_int(ty2.clone()) {
                    Ok(ty1)
                } else {
                    Err(self.create_error("Type mismatch", span))
                }
            }
            BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Lte | BinaryOp::Gte => {
                if Self::is_int(ty1.clone()) && Self::is_int(ty2.clone())
                    || Self::is_float(ty1.clone()) && Self::is_float(ty2.clone())
                {
                    Ok(Type::Builtin(BuiltinType::Bln))
                } else {
                    Err(self.create_error("Type mismatch", span))
                }
            }
            BinaryOp::Eq | BinaryOp::Neq => {
                if Self::is_int(ty1.clone()) && Self::is_int(ty2.clone())
                    || Self::is_pointer(ty1.clone()) && Self::is_pointer(ty2.clone())
                    || Self::is_bool(ty1.clone()) && Self::is_bool(ty2.clone())
                {
                    Ok(Type::Builtin(BuiltinType::Bln))
                } else {
                    Err(self.create_error("Type mismatch", span))
                }
            }
            BinaryOp::And | BinaryOp::Or => {
                if Self::is_bool(ty1.clone()) && Self::is_bool(ty2.clone()) {
                    Ok(Type::Builtin(BuiltinType::Bln))
                } else {
                    Err(self.create_error("Type mismatch", span))
                }
            }
        }
    }
}
