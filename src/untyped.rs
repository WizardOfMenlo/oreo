use std::ops::Range;

use crate::parser::{ExpToken, TokenList};
use crate::range::RangedObject;
use crate::syntax::{AdditiveOp, BooleanOp, MultiplicativeOp, RelationalOp};
use crate::tokens::Token;

#[derive(Debug, Clone)]
pub enum SyntaxError<'a> {
    ExpectedOneOfFoundEOF(TokenList),
    ExpectedFoundEOF(ExpToken),
    ExpectedOneOfFound(TokenList, RangedObject<Token<'a>>),
    ExpectedFound(ExpToken, RangedObject<Token<'a>>),
    LogicalError,
}

#[derive(Debug, Clone)]
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

    // Atom (TODO: might be wrong)
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
}

#[derive(Debug, Clone)]
pub struct Node<'a> {
    pub ty: NodeType<'a>,
    pub text_range: Range<usize>,
    pub children: Vec<Node<'a>>,
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
