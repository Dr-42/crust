mod ast_data;
pub mod node;
pub mod node_types;
mod visitors;

use crate::parser::ast_data::AstData;
use crate::parser::node::Node;
use peekmore::{PeekMore, PeekMoreIterator};
use std::error::Error;

use crate::{
    lexer::{self, token_types::TokenType},
    types::KeywordType,
};

pub struct Ast {
    pub root_node: Option<Node>,
    pub filename: String,
    pub ast_data: AstData,
    lexer: PeekMoreIterator<lexer::Lexer>,
}

impl Ast {
    pub fn new(mut lexer: lexer::Lexer) -> Result<Self, Box<dyn Error>> {
        lexer.tokenize()?;
        lexer.trim_comments();
        Ok(Self {
            root_node: None,
            filename: lexer.filename.clone(),
            ast_data: AstData::new(),
            lexer: lexer.peekmore(),
        })
    }

    pub fn parse(&mut self) -> Result<(), Box<dyn Error>> {
        self.root_node = Some(self.visit_program()?);
        Ok(())
    }

    fn visit_program(&mut self) -> Result<Node, Box<dyn Error>> {
        let mut program_node = Node::new(node_types::NodeType::Program, 0, 0, None);
        while let Some(_) = self.lexer.peek() {
            program_node.add_child(self.visit_statement()?);
        }
        Ok(program_node)
    }

    fn visit_statement(&mut self) -> Result<Node, Box<dyn Error>> {
        if let Some(token) = self.lexer.peek() {
            match &token.token_type {
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
                TokenType::Preprocessor { .. } => self.visit_preprocessor_statement(),
                TokenType::Identifier(_) => self.visit_identifier(),
                _ => Err("Unexpected toplevel token".into()),
            }
        } else {
            Err("Unexpected end of file".into())
        }
    }
}
