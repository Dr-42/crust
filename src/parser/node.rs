use crate::lexer::{token_types::DataType, OperatorType, PunctuatorType};

pub use super::node_types::NodeType;

#[derive(Debug)]
pub enum NodeData {
    Identifier(String),
    Type(DataType),
    Literal(String),
    Operator(OperatorType),
    Punctuator(PunctuatorType),
    Preprocessor(String),
}

#[derive(Debug)]
pub struct Node {
    pub node_type: NodeType,
    pub data: Option<NodeData>,
    pub line: u64,
    pub column: u64,
    pub children: Vec<Node>,
}

impl Node {
    pub fn new(node_type: NodeType, line: u64, column: u64, data: Option<NodeData>) -> Self {
        Self {
            node_type,
            line,
            column,
            data,
            children: Vec::new(),
        }
    }

    pub fn add_child(&mut self, node: Node) {
        self.children.push(node);
    }
}
