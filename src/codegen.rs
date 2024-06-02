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
            Type::UserDefined { name } => todo!(),
            Type::TraitType { traits } => todo!(),
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
            todo!()
        }
        Ok(())
    }

    pub fn codegen_expr(&self, expr: Expr) -> Result<(), Diagnostic<usize>> {
        todo!()
    }
}
