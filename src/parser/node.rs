use crate::lexer::Token;

pub use super::node_types::NodeType;

#[derive(Debug)]
pub struct Node {
    pub node_type: NodeType,
    pub data: String,
    pub line: u32,
    pub column: u32,
    pub children: Vec<Node>,
}

impl Node {
    pub fn new(node_type: NodeType, token: &Token, data: &String) -> Self {
        Self {
            node_type,
            line: token.line,
            column: token.column,
            data: data.clone(),
            children: Vec::new(),
        }
    }

    pub fn add_child(&mut self, node: Node) {
        self.children.push(node);
    }
}

#[macro_export]
macro_rules! node {
    ($node_type:expr, $token:expr, $data:expr) => {
        Node::new($node_type, $token, $data)
    };
    ($node_type:expr, $token:expr) => {
        Node::new($node_type, $token, String::new())
    };
    ($node_type:expr) => {
        Node {
            node_type: $node_type,
            data: String::new(),
            line: 0,
            column: 0,
            children: Vec::new(),
        }
    };
}
