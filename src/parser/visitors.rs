use super::{
    node::{Node, NodeType},
    Ast,
};
use crate::{
    lexer::{
        token_types::{DataType, KeywordType, PunctuatorType},
        OperatorType, TokenType,
    },
    parser::node::NodeData,
};

use std::error::Error;

#[macro_export]
macro_rules! err {
    ($self:expr, $msg:expr) => {
        return Err($msg.into())
    };
    ($self:expr, $msg:expr, $token:expr) => {
        return Err(($msg.to_string()
            + &format!(
                "got token of type {} at {}:{}:{}",
                $token.token_type, $self.filename, $token.line, $token.column
            ))
            .into())
    };
}

impl Ast {
    pub fn visit_function_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        if let Some(token) = self.lexer.next() {
            if token.token_type != TokenType::Keyword(KeywordType::Fnc) {
                err!(self, "Expected keyword 'fnc' ", token)
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
                    _ => err!(self, "Expected identifier ", token),
                }
            } else {
                err!(self, "Unexpected EOF")
            }
            if let Some(token) = self.lexer.peek() {
                // Generic data types enclosed in '`'
                if token.token_type == TokenType::Punctuator(PunctuatorType::Tick) {
                    //self.lexer.next();
                    let mut tick_encountered = false;
                    let mut generic_node =
                        Node::new(NodeType::FunctionGenerics, token.line, token.column, None);
                    while let Some(token) = self.lexer.next() {
                        match token.token_type {
                            TokenType::Punctuator(PunctuatorType::Tick) => {
                                if tick_encountered {
                                    function_node.add_child(generic_node);
                                    break;
                                } else {
                                    tick_encountered = true;
                                }
                            }
                            TokenType::DataType(DataType::Generic(data_type)) => {
                                generic_node.add_child(Node::new(
                                    NodeType::Type,
                                    token.line,
                                    token.column,
                                    Some(NodeData::Type(DataType::Generic(data_type))),
                                ));
                            }
                            TokenType::Punctuator(PunctuatorType::Comma) => {}
                            _ => err!(self, "Expected generic data type ", token),
                        }
                    }
                }
            } else {
                err!(self, "Unexpected EOF")
            }
            if let Some(token) = self.lexer.peek() {
                if token.token_type != TokenType::Punctuator(PunctuatorType::LeftParen) {
                    err!(self, "Expected '(' ", token)
                } else {
                    let args_node = self.visit_fn_decl_arg()?;
                    function_node.add_child(args_node);
                }
            } else {
                err!(self, "Unexpected EOF")
            }
            if let Some(token) = self.lexer.peek() {
                match token.token_type {
                    TokenType::Punctuator(PunctuatorType::Colon) => {
                        self.lexer.next();
                    }
                    _ => err!(self, "Expected ':' ", token),
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
                    _ => err!(self, "Expected data type ", token),
                }
            } else {
                err!(self, "Unexpected EOF")
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
                    _ => err!(self, "Expected ';' or '{' ", token),
                }
            } else {
                err!(self, "Unexpected EOF")
            }
            Ok(function_node)
        } else {
            err!(self, "Unexpected EOF")
        }
    }

    pub fn visit_fn_decl_arg(&mut self) -> Result<Node, Box<dyn Error>> {
        if let Some(token) = self.lexer.next() {
            match token.token_type {
                TokenType::Punctuator(PunctuatorType::LeftParen) => {}
                _ => err!(self, "Expected '(' ", token),
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
                            _ => err!(self, "Expected identifier ", name_token),
                        }
                    } else {
                        err!(self, "Unexpected EOF")
                    }

                    if let Some(token) = self.lexer.peek() {
                        match token.token_type {
                            TokenType::Punctuator(PunctuatorType::Colon) => {
                                self.lexer.next();
                            }
                            _ => err!(self, "Expected ':' ", token),
                        }
                    } else {
                        err!(self, "Unexpected EOF")
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
                            _ => err!(self, "Expected data type ", type_token),
                        }
                    } else {
                        err!(self, "Unexpected EOF")
                    }

                    if let Some(token) = self.lexer.peek() {
                        match token.token_type {
                            TokenType::Punctuator(PunctuatorType::Comma) => {
                                self.lexer.next();
                            }
                            TokenType::Punctuator(PunctuatorType::RightParen) => {}
                            _ => err!(self, "Expected ',' or ')' ", token),
                        }
                    } else {
                        err!(self, "Unexpected EOF")
                    }
                }
                _ => err!(self, "Expected identifier ", token),
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
        // a : i32 = 0; Variable
        // a : [132; 6]; Array
        // a[1][2] = 4; Array access
        // a = 6; Variable assignment
        // a.b = 6; Struct member access

        if let Some(token) = self.lexer.peek() {
            match token.token_type {
                TokenType::Identifier(_) => {}
                _ => err!(self, "Expected identifier ", token),
            }
        } else {
            err!(self, "Unexpected EOF")
        }

        let result_node: Node;
        self.lexer.advance_cursor();
        if let Some(token) = self.lexer.peek() {
            match token.token_type {
                TokenType::Punctuator(PunctuatorType::Colon) => {
                    self.lexer.reset_cursor();
                    result_node = self.visit_declarations()?;
                }
                TokenType::Punctuator(PunctuatorType::LeftBracket) => {
                    self.lexer.reset_cursor();
                    result_node = self.visit_array_element_assignment()?;
                }
                TokenType::Operator(
                    OperatorType::Assign
                    | OperatorType::AddAssign
                    | OperatorType::SubAssign
                    | OperatorType::MulAssign
                    | OperatorType::DivAssign
                    | OperatorType::ModAssign,
                ) => {
                    self.lexer.reset_cursor();
                    result_node = self.visit_variable_assignment()?;
                }
                TokenType::Punctuator(PunctuatorType::Dot) => {
                    self.lexer.reset_cursor();
                    result_node = self.visit_struct_member_access()?;
                }
                _ => err!(self, "Expected ':' or assignment operator ", token),
            }
        } else {
            err!(self, "Unexpected EOF")
        }
        Ok(result_node)
    }

    fn visit_declarations(&mut self) -> Result<Node, Box<dyn Error>> {
        if let Some(token) = self.lexer.peek() {
            match token.token_type {
                TokenType::Identifier(_) => {}
                _ => err!(self, "Expected identifier ", token),
            }
        } else {
            err!(self, "Unexpected EOF")
        }

        self.lexer.advance_cursor();
        if let Some(token) = self.lexer.peek() {
            match token.token_type {
                TokenType::Punctuator(PunctuatorType::Colon) => {}
                _ => err!(self, "Expected ':' ", token),
            }
        } else {
            err!(self, "Unexpected EOF")
        }

        let result_node: Node;
        self.lexer.advance_cursor();
        if let Some(token) = self.lexer.peek() {
            match token.token_type {
                TokenType::DataType(_) => {
                    self.lexer.reset_cursor();
                    result_node = self.visit_variable_declaration()?;
                }
                TokenType::Punctuator(PunctuatorType::LeftBracket) => {
                    self.lexer.reset_cursor();
                    result_node = self.visit_array_declaration()?;
                }
                _ => err!(self, "Expected data type ", token),
            }
        } else {
            err!(self, "Unexpected EOF")
        }
        Ok(result_node)
    }

    fn visit_variable_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!("visit_variable_declaration")
    }

    fn visit_array_declaration(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!("visit_array_declaration")
    }

    fn visit_array_element_assignment(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!("visit_array_element_assignment")
    }

    fn visit_variable_assignment(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!("visit_variable_assignment")
    }

    fn visit_struct_member_access(&mut self) -> Result<Node, Box<dyn Error>> {
        todo!("visit_struct_member_access")
    }
}
