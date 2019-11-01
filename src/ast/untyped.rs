use super::syntax::{AdditiveOp, BooleanOp, MultiplicativeOp, RelationalOp};
use crate::parser::error::SyntaxError;
use crate::range::Ranged;
use serde::Serialize;
use std::ops::Range;

#[derive(Debug, Clone, Serialize)]
pub enum NodeType<'a> {
    // Error node
    Error(SyntaxError<'a>),

    // Top
    Program,
    Compound,

    // Statements
    Decl,
    PrintStat,
    While,
    If,
    Assign,
    FunctionCall,
    FunctionDecl,
    Return,
    // Print vars
    Print,
    Println,
    Get,

    // Args
    FunctionDeclArgs,
    FunctionCallArgs,

    // Expr
    Expr,
    And,
    Or,

    // Term
    Term,
    LesserThan,
    GreaterThan,
    Equals,
    GreaterOrEquals,
    LesserOrEquals,

    // Factor
    Factor,
    Plus,
    Minus,

    // Product
    Product,
    Times,
    Divide,

    // Atom
    Unit,
    Not,

    // Unit
    Int(isize),
    Str,
    Bool(bool),
    Identifier,
    BracketedExpr,
}

impl<'a> NodeType<'a> {
    pub fn is_error(&self) -> bool {
        match self {
            NodeType::Error(_) => true,
            _ => false,
        }
    }

    pub fn unwrap_err(&self) -> SyntaxError {
        match self {
            NodeType::Error(e) => e.clone(),
            _ => panic!("Invalid conversion"),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Node<'a> {
    pub(super) ty: NodeType<'a>,
    pub(super) text_range: Range<usize>,
    pub(super) children: Vec<Node<'a>>,
}

impl<'a> Node<'a> {
    pub fn new(ty: NodeType<'a>, range: Range<usize>, children: Vec<Node<'a>>) -> Self {
        Node {
            ty,
            text_range: range,
            children
        }
    }

    pub fn ty(&self) -> &NodeType<'a> {
        &self.ty
    }

    pub fn children(&self) -> impl Iterator<Item = &Node<'a>> {
        self.children.iter()
    }
}

impl<'a> Ranged for Node<'a> {
    fn range(&self) -> &Range<usize> {
        &self.text_range
    }
}

impl<'a> From<NodeType<'a>> for AdditiveOp {
    fn from(ty: NodeType) -> Self {
        match ty {
            NodeType::Plus => AdditiveOp::Plus,
            NodeType::Minus => AdditiveOp::Minus,
            _ => panic!("Invalid node type"),
        }
    }
}

impl<'a> From<NodeType<'a>> for MultiplicativeOp {
    fn from(ty: NodeType) -> Self {
        match ty {
            NodeType::Times => MultiplicativeOp::Times,
            NodeType::Divide => MultiplicativeOp::Divide,
            _ => panic!("Invalid node type"),
        }
    }
}

impl<'a> From<NodeType<'a>> for BooleanOp {
    fn from(ty: NodeType) -> Self {
        match ty {
            NodeType::And => BooleanOp::And,
            NodeType::Or => BooleanOp::Or,
            _ => panic!("Invalid node type"),
        }
    }
}

impl<'a> From<NodeType<'a>> for RelationalOp {
    fn from(ty: NodeType) -> Self {
        match ty {
            NodeType::LesserThan => RelationalOp::LesserThan,
            NodeType::GreaterThan => RelationalOp::GreaterThan,
            NodeType::LesserOrEquals => RelationalOp::LesserOrEquals,
            NodeType::GreaterOrEquals => RelationalOp::GreaterOrEquals,
            NodeType::Equals => RelationalOp::Equals,
            _ => panic!("Invalid node type"),
        }
    }
}
