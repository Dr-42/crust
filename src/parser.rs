pub mod node;
pub mod node_types;

use node::{Node, NodeType};
use std::{error::Error, iter::Peekable};

use crate::{
    lexer::{self, token_types::TokenType},
    node,
    types::KeywordType,
};

pub struct Ast {
    pub root_node: Option<Node>,
    lexer: Peekable<lexer::Lexer>,
}

impl Ast {
    pub fn new(mut lexer: lexer::Lexer) -> Result<Self, Box<dyn Error>> {
        lexer.tokenize()?;
        lexer.trim_comments();
        Ok(Self {
            root_node: None,
            lexer: lexer.peekable(),
        })
    }

    pub fn parse(&mut self) -> Result<(), Box<dyn Error>> {
        self.root_node = Some(self.visit_program()?);
        Ok(())
    }

    fn visit_program(&mut self) -> Result<Node, Box<dyn Error>> {
        let mut program_node = node!(NodeType::Program);
        while let Some(_) = self.lexer.peek() {
            program_node.add_child(self.visit_statement()?);
        }
        Ok(program_node)
    }

    fn visit_statement(&mut self) -> Result<Node, Box<dyn Error>> {
        if let Some(token) = self.lexer.next() {
            match token.token_type {
                TokenType::Keyword(keyword_type) => match keyword_type {
                    KeywordType::Fnc => self.visit_function_declaration(),
                    KeywordType::Const => self.visit_constant_declaration(),
                    KeywordType::Struct => self.visit_struct_declaration(),
                    KeywordType::Enum => self.visit_enum_declaration(),
                    KeywordType::Union => self.visit_union_declaration(),
                    KeywordType::Static => self.visit_static_declaration(),
                    KeywordType::Extern => self.visit_extern_declaration(),
                    _ => Err("Unexpected toplevel keyword".into()),
                },
                TokenType::Preprocessor { typ, data } => Ok(Node {
                    node_type: NodeType::PreprocessorStatement(typ),
                    data,
                    line: token.line,
                    column: token.column,
                    children: Vec::new(),
                }),
                TokenType::Identifier(name) => self.visit_identifier(name),
                _ => Err("Unexpected toplevel token".into()),
            }
        } else {
            Err("Unexpected end of file".into())
        }
    }

    fn visit_function_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    fn visit_constant_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    fn visit_struct_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    fn visit_enum_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    fn visit_union_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    fn visit_static_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    fn visit_extern_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    fn visit_identifier(&mut self, _name: String) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
}
