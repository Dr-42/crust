#![allow(dead_code, unused)]
use codespan_reporting::diagnostic::Diagnostic;
use inkwell::{
    builder::Builder,
    context::{self, Context},
    execution_engine::ExecutionEngine,
    module::Module,
    types::{AnyType, AnyTypeEnum, ArrayType, BasicMetadataTypeEnum, BasicType},
    AddressSpace,
};

use crate::ast::nodes::{Expr, Program, Type};

pub struct CodegenContext<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodegenContext<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();
        let execution_engine = module.create_execution_engine().unwrap();
        Self {
            context,
            module,
            builder,
            execution_engine,
        }
    }

    pub fn get_inkwell_type(&self, ty: &Type) -> Box<dyn AnyType<'ctx> + 'ctx> {
        match ty {
            Type::Builtin(builtin_ty) => match builtin_ty {
                crate::ast::nodes::BuiltinType::I8 => Box::new(self.context.i8_type()),
                crate::ast::nodes::BuiltinType::I16 => Box::new(self.context.i16_type()),
                crate::ast::nodes::BuiltinType::I32 => Box::new(self.context.i32_type()),
                crate::ast::nodes::BuiltinType::I64 => Box::new(self.context.i64_type()),
                crate::ast::nodes::BuiltinType::U8 => Box::new(self.context.i8_type()),
                crate::ast::nodes::BuiltinType::U16 => Box::new(self.context.i16_type()),
                crate::ast::nodes::BuiltinType::U32 => Box::new(self.context.i32_type()),
                crate::ast::nodes::BuiltinType::U64 => Box::new(self.context.i64_type()),
                crate::ast::nodes::BuiltinType::F32 => Box::new(self.context.f32_type()),
                crate::ast::nodes::BuiltinType::F64 => Box::new(self.context.f64_type()),
                crate::ast::nodes::BuiltinType::Void => Box::new(self.context.void_type()),
                crate::ast::nodes::BuiltinType::Chr => Box::new(self.context.i8_type()),
                crate::ast::nodes::BuiltinType::Bln => Box::new(self.context.bool_type()),
                crate::ast::nodes::BuiltinType::Str => {
                    Box::new(self.context.i8_type().ptr_type(AddressSpace::default()))
                }
                crate::ast::nodes::BuiltinType::Slf => todo!(),
            },
            Type::Pointer(base_ty) => {
                let base_inkwell_ty = self.get_inkwell_type(base_ty).as_any_type_enum();
                match base_inkwell_ty {
                    AnyTypeEnum::ArrayType(base) => {
                        Box::new(base.ptr_type(AddressSpace::default()))
                    }
                    AnyTypeEnum::FloatType(base) => {
                        Box::new(base.ptr_type(AddressSpace::default()))
                    }
                    AnyTypeEnum::FunctionType(base) => {
                        Box::new(base.ptr_type(AddressSpace::default()))
                    }
                    AnyTypeEnum::IntType(base) => Box::new(base.ptr_type(AddressSpace::default())),
                    AnyTypeEnum::PointerType(base) => {
                        Box::new(base.ptr_type(AddressSpace::default()))
                    }
                    AnyTypeEnum::StructType(base) => {
                        Box::new(base.ptr_type(AddressSpace::default()))
                    }
                    AnyTypeEnum::VectorType(base) => {
                        Box::new(base.ptr_type(AddressSpace::default()))
                    }
                    AnyTypeEnum::VoidType(base) => {
                        Box::new(self.context.i8_type().ptr_type(AddressSpace::default()))
                    }
                }
            }
            Type::FnPtr { args, ret } => {
                let mut arg_types = Vec::new();
                for arg in args {
                    let arg_inkwell_ty = self.get_inkwell_type(arg).as_any_type_enum();
                    let arg_meta = match arg_inkwell_ty.as_any_type_enum() {
                        AnyTypeEnum::ArrayType(arg_ty) => BasicMetadataTypeEnum::from(arg_ty),
                        AnyTypeEnum::FloatType(arg_ty) => BasicMetadataTypeEnum::from(arg_ty),
                        AnyTypeEnum::FunctionType(arg_ty) => todo!(),
                        AnyTypeEnum::IntType(arg_ty) => BasicMetadataTypeEnum::from(arg_ty),
                        AnyTypeEnum::PointerType(arg_ty) => BasicMetadataTypeEnum::from(arg_ty),
                        AnyTypeEnum::StructType(arg_ty) => BasicMetadataTypeEnum::from(arg_ty),
                        AnyTypeEnum::VectorType(arg_ty) => BasicMetadataTypeEnum::from(arg_ty),
                        AnyTypeEnum::VoidType(arg_ty) => todo!(),
                    };
                    arg_types.push(arg_meta);
                }
                let ret_inkwell_ty = self.get_inkwell_type(ret).as_any_type_enum();
                Box::new(
                    self.context
                        .i8_type()
                        .fn_type(&arg_types, false)
                        .ptr_type(AddressSpace::default()),
                )
            }
            Type::UserDefined { name, generic_args } => todo!(),
            Type::Array { base, lens } => {
                let mut current_ty = self.get_inkwell_type(base).as_any_type_enum();
                for len in lens {
                    match current_ty {
                        AnyTypeEnum::ArrayType(ty) => {
                            current_ty = AnyTypeEnum::ArrayType(ty.array_type(*len as u32));
                        }
                        AnyTypeEnum::FloatType(ty) => {
                            current_ty = AnyTypeEnum::ArrayType(ty.array_type(*len as u32));
                        }
                        AnyTypeEnum::FunctionType(ty) => {
                            todo!()
                        }
                        AnyTypeEnum::IntType(ty) => {
                            current_ty = AnyTypeEnum::ArrayType(ty.array_type(*len as u32));
                        }
                        AnyTypeEnum::PointerType(ty) => {
                            current_ty = AnyTypeEnum::ArrayType(ty.array_type(*len as u32));
                        }
                        AnyTypeEnum::StructType(ty) => {
                            current_ty = AnyTypeEnum::ArrayType(ty.array_type(*len as u32));
                        }
                        AnyTypeEnum::VectorType(ty) => {
                            current_ty = AnyTypeEnum::ArrayType(ty.array_type(*len as u32));
                        }
                        AnyTypeEnum::VoidType(ty) => todo!(),
                    }
                }
                Box::new(current_ty)
            }
        }
    }

    pub fn codegen_program(&self, program: Program) -> Result<(), Diagnostic<usize>> {
        for statement in program.stmts {
            match *statement {
                crate::ast::nodes::Stmt::Expr(expr) => {
                    let _ = self.codegen_expr(*expr);
                }
                crate::ast::nodes::Stmt::TypeAlias { name, ty, span } => todo!(),
                crate::ast::nodes::Stmt::If {
                    cond,
                    body,
                    els,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::While { cond, body, span } => todo!(),
                crate::ast::nodes::Stmt::For {
                    init,
                    cond,
                    step,
                    body,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::Return { expr, span } => todo!(),
                crate::ast::nodes::Stmt::Block { stmts, decl_data } => todo!(),
                crate::ast::nodes::Stmt::VarDecl {
                    name,
                    qualifiers,
                    ty,
                    value,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::StructDecl {
                    name,
                    fields,
                    generics,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::ImplDecl {
                    name,
                    methods,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::TraitDecl {
                    name,
                    methods,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::EnumDecl {
                    name,
                    variants,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::UnionDecl {
                    name,
                    fields,
                    generics,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::FunctionDecl {
                    name,
                    qualifiers,
                    args,
                    ret,
                    body,
                    generics,
                    isvararg,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::VarAssign {
                    name,
                    value,
                    op,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::DerefAssign {
                    value,
                    expr,
                    op,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::StructAssign {
                    name,
                    qualifiers,
                    ty,
                    fields,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::StructMemberAssign {
                    name,
                    value,
                    op,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::ArrayMemberAssign {
                    element,
                    value,
                    op,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::TraitAssign {
                    name,
                    for_ty,
                    methods,
                    span,
                } => todo!(),
                crate::ast::nodes::Stmt::Match { expr, cases, span } => todo!(),
                crate::ast::nodes::Stmt::Break(_) => todo!(),
                crate::ast::nodes::Stmt::Continue(_) => todo!(),
                crate::ast::nodes::Stmt::Comment(_) => todo!(),
            }
        }
        Ok(())
    }

    pub fn codegen_expr(&self, expr: Expr) -> Result<(), Diagnostic<usize>> {
        todo!()
    }
}
