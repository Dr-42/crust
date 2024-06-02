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
    pub methods: Vec<FunctionDeclData>,
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
    pub generics: Option<Vec<GenericType>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TraitDeclData {
    pub name: String,
    pub methods: Vec<FunctionDeclData>,
    pub generics: Option<Vec<GenericType>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeAliasData {
    pub name: String,
    pub ty: Box<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Checkpoint {
    var: usize,
    struct_: usize,
    function: usize,
    enum_: usize,
    union: usize,
    trait_: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeclData {
    pub var: Vec<VarDeclData>,
    pub struct_: Vec<StructDeclData>,
    pub function: Vec<FunctionDeclData>,
    pub enum_: Vec<EnumDeclData>,
    pub union: Vec<UnionDeclData>,
    pub trait_: Vec<TraitDeclData>,
    pub checkpoints: Vec<Checkpoint>,
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
            trait_: Vec::new(),
            checkpoints: Vec::new(),
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
                    methods: Vec::new(),
                    generics,
                });
            }
            Stmt::ImplDecl { name, methods, .. } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for struct declaration"),
                };
                let mut meths = Vec::new();
                for m in methods.iter() {
                    match *m.clone() {
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
                            meths.push(FunctionDeclData {
                                name: nm,
                                args: arg,
                                ret: ret.clone(),
                                generics,
                                variadic: isvararg,
                            });
                        }
                        _ => {
                            panic!("Invalid method in struct declaration");
                        }
                    };
                }
                let mut found = false;
                for s in self.struct_.iter_mut() {
                    if s.name == nm {
                        s.methods = meths;
                        found = true;
                        break;
                    }
                }
                if !found {
                    panic!("Struct not found for impl declaration");
                }
            }
            Stmt::TraitAssign {
                name,
                for_ty,
                methods,
                ..
            } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for struct declaration"),
                };
                let mut meths = Vec::new();
                for m in methods.iter() {
                    match *m.clone() {
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
                            meths.push(FunctionDeclData {
                                name: nm,
                                args: arg,
                                ret: ret.clone(),
                                generics,
                                variadic: isvararg,
                            });
                        }
                        _ => {
                            panic!("Invalid method in struct declaration");
                        }
                    };
                }
                let struct_name = match *for_ty {
                    Type::UserDefined { name, .. } => match *name {
                        Expr::Iden { val, .. } => val.clone(),
                        _ => panic!("Invalid name for struct declaration"),
                    },
                    _ => panic!("Invalid name for struct declaration"),
                };
                let mut found = false;
                for s in self.struct_.iter_mut() {
                    if s.name == struct_name {
                        s.methods = meths;
                        found = true;
                        break;
                    }
                }
                if !found {
                    panic!("Struct not found for trait assignment");
                }
            }
            Stmt::StructAssign {
                name, ty: Some(ty), ..
            } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for struct declaration"),
                };
                self.var.push(VarDeclData {
                    name: nm,
                    ty: ty.clone(),
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
                let mut vars: Vec<String> = Vec::new();
                for v in variants {
                    match *v {
                        Expr::Iden { val, .. } => {
                            vars.push(val.clone());
                        }
                        _ => {
                            panic!("Invalid variant in enum declaration");
                        }
                    };
                }
                self.enum_.push(EnumDeclData {
                    name: nm,
                    variants: vars,
                });
            }
            Stmt::UnionDecl {
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
                self.union.push(UnionDeclData {
                    name: nm,
                    fields: flds,
                    generics,
                });
            }
            Stmt::TraitDecl { name, methods, .. } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for trait declaration"),
                };
                let mut meths = Vec::new();
                for m in methods.iter() {
                    match *m.clone() {
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
                            meths.push(FunctionDeclData {
                                name: nm,
                                args: arg,
                                ret: ret.clone(),
                                generics,
                                variadic: isvararg,
                            });
                        }
                        _ => {
                            panic!("Invalid method in trait declaration");
                        }
                    };
                }
                self.trait_.push(TraitDeclData {
                    name: nm,
                    methods: meths,
                    generics: None,
                });
            }
            Stmt::TypeAlias { name, ty, .. } => {
                let nm = match *name {
                    Expr::Iden { val, .. } => val.clone(),
                    _ => panic!("Invalid name for type alias"),
                };
                self.var.push(VarDeclData {
                    name: nm,
                    ty: ty.clone(),
                });
            }

            _ => {}
        }
    }

    pub fn push_scope(&mut self) {
        self.checkpoints.push(Checkpoint {
            var: self.var.len(),
            struct_: self.struct_.len(),
            function: self.function.len(),
            enum_: self.enum_.len(),
            union: self.union.len(),
            trait_: self.trait_.len(),
        });
    }

    pub fn pop_scope(&mut self) {
        if let Some(cp) = self.checkpoints.pop() {
            self.var.truncate(cp.var);
            self.struct_.truncate(cp.struct_);
            self.function.truncate(cp.function);
            self.enum_.truncate(cp.enum_);
            self.union.truncate(cp.union);
            self.trait_.truncate(cp.trait_);
        }
    }
}
