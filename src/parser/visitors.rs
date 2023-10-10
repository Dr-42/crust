use super::{
    node::{Node, NodeType},
    Ast,
};
use crate::{
    lexer::{
        token_types::{KeywordType, PunctuatorType},
        TokenType,
    },
    parser::node::NodeData,
};

use std::error::Error;

macro_rules! err {
    ($msg:expr) => {
        return Err($msg.into())
    };
    ($msg:expr, $token:expr) => {
        return Err(($msg.to_string()
            + &format!(
                "got token of type {:?} at line {}, column {}",
                $token.token_type, $token.line, $token.column
            ))
            .into())
    };
}

impl Ast {
    pub fn visit_function_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        if let Some(token) = self.lexer.next() {
            if token.token_type != TokenType::Keyword(KeywordType::Fnc) {
                err!("Expected keyword 'fnc' ", token)
            }
            let mut function_node = Node::new(
                NodeType::FunctionDeclaration,
                token.line,
                token.column,
                None,
            );
            if let Some(token) = self.lexer.next() {
                match token.token_type {
                    TokenType::Identifier(name) => {
                        function_node.add_child(Node::new(
                            NodeType::Identifier,
                            token.line,
                            token.column,
                            Some(NodeData::Identifier(name)),
                        ));
                    }
                    _ => err!("Expected identifier ", token),
                }
            } else {
                err!("Unexpected EOF")
            }
            if let Some(token) = self.lexer.peek() {
                if token.token_type != TokenType::Punctuator(PunctuatorType::LeftParen) {
                    err!("Expected '(' ", token)
                } else {
                    let args_node = self.visit_fn_decl_arg()?;
                    function_node.add_child(args_node);
                }
            } else {
                err!("Unexpected EOF")
            }
            if let Some(token) = self.lexer.peek() {
                match token.token_type {
                    TokenType::Punctuator(PunctuatorType::Colon) => {
                        self.lexer.next();
                    }
                    _ => err!("Expected ':' ", token),
                }
            }

            if let Some(token) = self.lexer.next() {
                match token.token_type {
                    TokenType::DataType(data_type) => {
                        function_node.add_child(Node::new(
                            NodeType::Type,
                            token.line,
                            token.column,
                            Some(NodeData::Type(data_type)),
                        ));
                    }
                    _ => err!("Expected data type ", token),
                }
            } else {
                err!("Unexpected EOF")
            }

            if let Some(token) = self.lexer.peek() {
                match token.token_type {
                    TokenType::Punctuator(PunctuatorType::Semicolon) => {
                        self.lexer.next();
                    }
                    TokenType::Punctuator(PunctuatorType::LeftBrace) => {
                        let scope_node = self.visit_scope()?;
                        function_node.add_child(scope_node);
                    }
                    _ => err!("Expected ';' or '{{' ", token),
                }
            } else {
                err!("Unexpected EOF")
            }
            Ok(function_node)
        } else {
            err!("Unexpected EOF")
        }
    }

    pub fn visit_fn_decl_arg(&mut self) -> Result<Node, Box<dyn Error>> {
        if let Some(token) = self.lexer.next() {
            match token.token_type {
                TokenType::Punctuator(PunctuatorType::LeftParen) => {}
                _ => err!("Expected '(' ", token),
            }
        }
        let mut args_node = Node::new(NodeType::FunctionDeclarationArguments, 0, 0, None);
        while let Some(token) = self.lexer.peek() {
            let mut arg_node = Node::new(
                NodeType::FunctionDeclarationArgument,
                token.line,
                token.column,
                None,
            );
            match &token.token_type {
                TokenType::Punctuator(PunctuatorType::RightParen) => {
                    self.lexer.next();
                    break;
                }
                TokenType::Identifier(_) => {
                    if let Some(name_token) = self.lexer.next() {
                        match name_token.token_type {
                            TokenType::Identifier(name) => {
                                arg_node.add_child(Node::new(
                                    NodeType::Identifier,
                                    name_token.line,
                                    name_token.column,
                                    Some(NodeData::Identifier(name)),
                                ));
                            }
                            _ => err!("Expected identifier ", name_token),
                        }
                    } else {
                        err!("Unexpected EOF")
                    }

                    if let Some(token) = self.lexer.peek() {
                        match token.token_type {
                            TokenType::Punctuator(PunctuatorType::Colon) => {
                                self.lexer.next();
                            }
                            _ => err!("Expected ':' ", token),
                        }
                    } else {
                        err!("Unexpected EOF")
                    }

                    if let Some(type_token) = self.lexer.next() {
                        match type_token.token_type {
                            TokenType::DataType(data_type) => {
                                arg_node.add_child(Node::new(
                                    NodeType::Type,
                                    type_token.line,
                                    type_token.column,
                                    Some(NodeData::Type(data_type)),
                                ));
                            }
                            _ => err!("Expected data type ", type_token),
                        }
                    } else {
                        err!("Unexpected EOF")
                    }

                    if let Some(token) = self.lexer.peek() {
                        match token.token_type {
                            TokenType::Punctuator(PunctuatorType::Comma) => {
                                self.lexer.next();
                            }
                            TokenType::Punctuator(PunctuatorType::RightParen) => {}
                            _ => err!("Expected ',' or ')' ", token),
                        }
                    } else {
                        err!("Unexpected EOF")
                    }
                }
                _ => err!("Expected identifier ", token),
            }
            args_node.add_child(arg_node);
        }
        Ok(args_node)
    }

    pub fn visit_scope(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    pub fn visit_preprocessor_statement(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    pub fn visit_constant_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    pub fn visit_struct_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    pub fn visit_enum_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    pub fn visit_union_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    pub fn visit_static_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    pub fn visit_extern_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
    pub fn visit_identifier(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!()
    }
}
