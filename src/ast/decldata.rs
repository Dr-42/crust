use super::nodes::{Expr, GenericType, Stmt, Type};

#[derive(Debug, PartialEq, Clone)]
pub struct VarDeclData {
    pub name: String,
    pub ty: Box<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclData {
    pub name: String,
    pub fields: Vec<VarDeclData>,
    pub generics: Option<Vec<GenericType>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclData {
    pub name: String,
    pub args: Vec<VarDeclData>,
    pub ret: Box<Type>,
    pub generics: Option<Vec<GenericType>>,
    pub variadic: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumDeclData {
    pub name: String,
    pub variants: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnionDeclData {
    pub name: String,
    pub fields: Vec<VarDeclData>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeclData {
    pub var: Vec<VarDeclData>,
    pub struct_: Vec<StructDeclData>,
    pub function: Vec<FunctionDeclData>,
    pub enum_: Vec<EnumDeclData>,
    pub union: Vec<UnionDeclData>,
}

impl Default for DeclData {
    fn default() -> DeclData {
        DeclData::new()
    }
}

impl DeclData {
    pub fn new() -> DeclData {
        DeclData {
            var: Vec::new(),
            struct_: Vec::new(),
            function: Vec::new(),
            enum_: Vec::new(),
            union: Vec::new(),
        }
    }

    pub fn add(&mut self, stmt: &Stmt) {
        match stmt.clone() {
            Stmt::VarDecl { name, ty, .. } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for variable declaration"),
                };
                self.var.push(VarDeclData {
                    name: nm,
                    ty: ty.clone(),
                });
            }
            Stmt::StructDecl {
                name,
                fields,
                generics,
                ..
            } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for struct declaration"),
                };
                let mut flds = Vec::new();
                for fld in fields.iter() {
                    match *fld.clone() {
                        Stmt::VarDecl { name, ty, .. } => {
                            let nm = match *name {
                                Expr::Iden { val, .. } => val.clone(),
                                _ => panic!("Invalid name for struct field"),
                            };
                            flds.push(VarDeclData {
                                name: nm,
                                ty: ty.clone(),
                            });
                        }
                        _ => {
                            panic!("Invalid field in struct declaration");
                        }
                    };
                }
                let generics = match generics {
                    Some(ref g) => {
                        let mut res = Vec::new();
                        for gen in g.iter() {
                            res.push(*gen.clone());
                        }
                        Some(res)
                    }
                    None => None,
                };
                self.struct_.push(StructDeclData {
                    name: nm,
                    fields: flds,
                    generics,
                });
            }
            Stmt::FunctionDecl {
                name,
                args,
                ret,
                generics,
                isvararg,
                ..
            } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for function declaration"),
                };
                let mut arg = Vec::new();
                for a in args.iter() {
                    match *a.clone() {
                        Stmt::VarDecl { name, ty, .. } => {
                            let nm = match *name {
                                Expr::Iden { val, .. } => val.clone(),
                                _ => panic!("Invalid name for function argument"),
                            };
                            arg.push(VarDeclData {
                                name: nm,
                                ty: ty.clone(),
                            });
                        }
                        _ => {
                            panic!("Invalid argument in function declaration");
                        }
                    };
                }
                let generics = match generics {
                    Some(ref g) => {
                        let mut res = Vec::new();
                        for gen in g.iter() {
                            res.push(*gen.clone());
                        }
                        Some(res)
                    }
                    None => None,
                };
                self.function.push(FunctionDeclData {
                    name: nm,
                    args: arg,
                    ret: ret.clone(),
                    generics,
                    variadic: isvararg,
                });
            }
            Stmt::EnumDecl { name, variants, .. } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for enum declaration"),
                };
                self.enum_.push(EnumDeclData {
                    name: nm,
                    variants: variants.clone(),
                });
            }
            Stmt::UnionDecl { name, fields, .. } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for union declaration"),
                };
                let mut flds = Vec::new();
                for fld in fields.iter() {
                    let nm1 = fld.0.clone();
                    let ty = Box::new(fld.1.clone());
                    let nm = match *nm1 {
                        Expr::Iden { val, .. } => val.clone(),
                        _ => panic!("Invalid name for union field"),
                    };
                    flds.push(VarDeclData { name: nm, ty });
                }
                self.union.push(UnionDeclData {
                    name: nm,
                    fields: flds,
                });
            }
            _ => {}
        }
    }
}
