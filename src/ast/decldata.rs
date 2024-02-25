use super::{
    nodes::{GenericType, Type},
    Span,
};

#[derive(Debug, PartialEq, Clone)]
pub struct VarDeclData {
    pub name: String,
    pub ty: Box<Type>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclData {
    pub name: String,
    pub fields: Vec<VarDeclData>,
    pub generics: Option<Vec<GenericType>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclData {
    pub name: String,
    pub args: Vec<VarDeclData>,
    pub ret: Box<Type>,
    pub generics: Option<Vec<GenericType>>,
    pub variadic: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumDeclData {
    pub name: String,
    pub variants: Vec<String>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnionDeclData {
    pub name: String,
    pub fields: Vec<VarDeclData>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TraitDeclData {
    pub name: String,
    pub methods: Vec<FunctionDeclData>,
    pub generics: Option<Vec<GenericType>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayDeclData {
    pub name: String,
    pub ty: Box<Type>,
    pub len: usize,
}

#[derive(Debug, PartialEq, Clone)]
struct DeclDataCheckpoint {
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
    pub array: Vec<ArrayDeclData>,
    checkpoint: Vec<DeclDataCheckpoint>,
}

impl Default for DeclData {
    fn default() -> DeclData {
        DeclData::new()
    }
}

impl DeclData {
    pub fn new() -> DeclData {
        let mut res = DeclData {
            var: Vec::new(),
            struct_: Vec::new(),
            function: Vec::new(),
            enum_: Vec::new(),
            union: Vec::new(),
            trait_: Vec::new(),
            checkpoint: Vec::new(),
            array: Vec::new(),
        };
        res.checkpoint();
        res
    }

    pub fn checkpoint(&mut self) {
        self.checkpoint.push(DeclDataCheckpoint {
            var: 0,
            struct_: 0,
            function: 0,
            enum_: 0,
            union: 0,
            trait_: 0,
        });
    }

    pub fn rollback(&mut self) {
        let cp = self.checkpoint.pop().unwrap();
        self.var.truncate(self.var.len() - cp.var);
        self.struct_.truncate(self.struct_.len() - cp.struct_);
        self.function.truncate(self.function.len() - cp.function);
        self.enum_.truncate(self.enum_.len() - cp.enum_);
        self.union.truncate(self.union.len() - cp.union);
        self.trait_.truncate(self.trait_.len() - cp.trait_);
    }

    pub fn is_declared(&self, name: &str) -> bool {
        for v in self.var.iter() {
            if v.name == name {
                return true;
            }
        }
        for s in self.struct_.iter() {
            if s.name == name {
                return true;
            }
        }
        for f in self.function.iter() {
            if f.name == name {
                return true;
            }
        }
        for e in self.enum_.iter() {
            if e.name == name {
                return true;
            }
        }
        for u in self.union.iter() {
            if u.name == name {
                return true;
            }
        }
        for t in self.trait_.iter() {
            if t.name == name {
                return true;
            }
        }
        for a in self.array.iter() {
            if a.name == name {
                return true;
            }
        }
        false
    }

    pub fn is_var_declared(&self, name: &str) -> bool {
        for v in self.var.iter() {
            if v.name == name {
                return true;
            }
        }
        false
    }

    pub fn is_struct_declared(&self, name: &str) -> bool {
        for s in self.struct_.iter() {
            if s.name == name {
                return true;
            }
        }
        false
    }

    pub fn is_function_declared(&self, name: &str) -> bool {
        for f in self.function.iter() {
            if f.name == name {
                return true;
            }
        }
        false
    }

    pub fn is_enum_declared(&self, name: &str) -> bool {
        for e in self.enum_.iter() {
            if e.name == name {
                return true;
            }
        }
        false
    }

    pub fn is_union_declared(&self, name: &str) -> bool {
        for u in self.union.iter() {
            if u.name == name {
                return true;
            }
        }
        false
    }

    pub fn is_trait_declared(&self, name: &str) -> bool {
        for t in self.trait_.iter() {
            if t.name == name {
                return true;
            }
        }
        false
    }

    pub fn is_array_declared(&self, name: &str) -> bool {
        for a in self.array.iter() {
            if a.name == name {
                return true;
            }
        }
        false
    }

    pub fn add_var(&mut self, name: String, ty: Box<Type>, span: Span) {
        self.var.push(VarDeclData { name, ty, span });
    }

    pub fn add_struct(
        &mut self,
        name: String,
        fields: Vec<VarDeclData>,
        generics: Option<Vec<GenericType>>,
        span: Span,
    ) {
        self.struct_.push(StructDeclData {
            name,
            fields,
            generics,
            span,
        });
    }

    pub fn add_function(
        &mut self,
        name: String,
        generics: Option<Vec<GenericType>>,
        args: Vec<VarDeclData>,
        ret: Box<Type>,
        variadic: bool,
        span: Span,
    ) {
        self.function.push(FunctionDeclData {
            name,
            args,
            ret,
            generics,
            variadic,
            span,
        });
    }

    pub fn add_enum(&mut self, name: String, variants: Vec<String>, span: Span) {
        self.enum_.push(EnumDeclData {
            name,
            variants,
            span,
        });
    }

    pub fn add_union(&mut self, name: String, fields: Vec<VarDeclData>, span: Span) {
        self.union.push(UnionDeclData { name, fields, span });
    }

    pub fn add_trait(
        &mut self,
        name: String,
        generics: Option<Vec<GenericType>>,
        methods: Vec<FunctionDeclData>,
        span: Span,
    ) {
        self.trait_.push(TraitDeclData {
            name,
            methods,
            generics,
            span,
        });
    }

    pub fn add_array(&mut self, name: String, ty: Box<Type>, len: usize) {
        self.array.push(ArrayDeclData { name, ty, len });
    }

    pub fn get_var_span(&self, name: &str) -> Option<Span> {
        for v in self.var.iter() {
            if v.name == name {
                return Some(v.span);
            }
        }
        None
    }

    pub fn get_struct_span(&self, name: &str) -> Option<Span> {
        for s in self.struct_.iter() {
            if s.name == name {
                return Some(s.span);
            }
        }
        None
    }

    pub fn get_function_span(&self, name: &str) -> Option<Span> {
        for f in self.function.iter() {
            if f.name == name {
                return Some(f.span);
            }
        }
        None
    }

    pub fn get_enum_span(&self, name: &str) -> Option<Span> {
        for e in self.enum_.iter() {
            if e.name == name {
                return Some(e.span);
            }
        }
        None
    }

    pub fn get_union_span(&self, name: &str) -> Option<Span> {
        for u in self.union.iter() {
            if u.name == name {
                return Some(u.span);
            }
        }
        None
    }

    pub fn get_trait_span(&self, name: &str) -> Option<Span> {
        for t in self.trait_.iter() {
            if t.name == name {
                return Some(t.span);
            }
        }
        None
    }

    pub fn get_array_span(&self, name: &str) -> Option<Span> {
        for a in self.array.iter() {
            if a.name == name {
                return Some(Span::new(0, 0));
            }
        }
        None
    }

    pub fn get_var_type(&self, name: &str) -> Option<Box<Type>> {
        for v in self.var.iter() {
            if v.name == name {
                return Some(v.ty.clone());
            }
        }
        None
    }

    pub fn get_struct_fields(&self, name: &str) -> Option<Vec<VarDeclData>> {
        for s in self.struct_.iter() {
            if s.name == name {
                return Some(s.fields.clone());
            }
        }
        None
    }

    pub fn get_function_args(&self, name: &str) -> Option<Vec<VarDeclData>> {
        for f in self.function.iter() {
            if f.name == name {
                return Some(f.args.clone());
            }
        }
        None
    }

    pub fn get_function_ret(&self, name: &str) -> Option<Box<Type>> {
        for f in self.function.iter() {
            if f.name == name {
                return Some(f.ret.clone());
            }
        }
        None
    }

    pub fn get_function_generics(&self, name: &str) -> Option<Vec<GenericType>> {
        for f in self.function.iter() {
            if f.name == name {
                return f.generics.clone();
            }
        }
        None
    }

    pub fn get_enum_variants(&self, name: &str) -> Option<Vec<String>> {
        for e in self.enum_.iter() {
            if e.name == name {
                return Some(e.variants.clone());
            }
        }
        None
    }

    pub fn get_union_fields(&self, name: &str) -> Option<Vec<VarDeclData>> {
        for u in self.union.iter() {
            if u.name == name {
                return Some(u.fields.clone());
            }
        }
        None
    }

    pub fn get_trait_methods(&self, name: &str) -> Option<Vec<FunctionDeclData>> {
        for t in self.trait_.iter() {
            if t.name == name {
                return Some(t.methods.clone());
            }
        }
        None
    }

    pub fn get_trait_generics(&self, name: &str) -> Option<Vec<GenericType>> {
        for t in self.trait_.iter() {
            if t.name == name {
                return t.generics.clone();
            }
        }
        None
    }

    pub fn get_var(&self, name: &str) -> Option<VarDeclData> {
        for v in self.var.iter() {
            if v.name == name {
                return Some(v.clone());
            }
        }
        None
    }

    pub fn get_struct(&self, name: &str) -> Option<StructDeclData> {
        for s in self.struct_.iter() {
            if s.name == name {
                return Some(s.clone());
            }
        }
        None
    }

    pub fn get_function(&self, name: &str) -> Option<FunctionDeclData> {
        for f in self.function.iter() {
            if f.name == name {
                return Some(f.clone());
            }
        }
        None
    }

    pub fn get_enum(&self, name: &str) -> Option<EnumDeclData> {
        for e in self.enum_.iter() {
            if e.name == name {
                return Some(e.clone());
            }
        }
        None
    }

    pub fn get_union(&self, name: &str) -> Option<UnionDeclData> {
        for u in self.union.iter() {
            if u.name == name {
                return Some(u.clone());
            }
        }
        None
    }

    pub fn get_trait(&self, name: &str) -> Option<TraitDeclData> {
        for t in self.trait_.iter() {
            if t.name == name {
                return Some(t.clone());
            }
        }
        None
    }

    pub fn get_array(&self, name: &str) -> Option<ArrayDeclData> {
        for a in self.array.iter() {
            if a.name == name {
                return Some(a.clone());
            }
        }
        None
    }
}

impl StructDeclData {
    pub fn find_member_type(&self, name: &str) -> Option<Box<Type>> {
        for f in self.fields.iter() {
            if f.name == name {
                return Some(f.ty.clone());
            }
        }
        None
    }
}
