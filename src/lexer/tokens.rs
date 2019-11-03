//! Definition of all the tokens that we use

use super::error::LexicalError;
use serde::Serialize;

/// The main token type that we work on
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Token<'a> {
    /// Any of the keywords (apart from and and or)
    Keyword(Keyword),

    /// (, ), ;, ,
    Punctuation(Punctuation),

    /// Addition, mult, ...
    Operator(Operator),

    /// A constant
    Literal(Literal<'a>),

    /// An identifier
    Identifier(&'a str),

    /// A comment
    Comment(&'a str),

    /// Any kind of error we might encounter
    Error(LexicalError<'a>),
}

impl<'a> Token<'a> {
    /// Is the current token an error?
    pub fn is_error(&self) -> bool {
        match self {
            Token::Error(_) => true,
            _ => false,
        }
    }

    /// Is the current token a comment?
    pub fn is_comment(&self) -> bool {
        match self {
            Token::Comment(_) => true,
            _ => false,
        }
    }

    /// Is this token the same as an other (where Int(1) == Int(2))
    pub fn same_kind<'b>(&self, other: &Token<'b>) -> bool
    where
        'b: 'a,
    {
        // This should match every singleton
        if self == other {
            true
        } else {
            // Match literals
            match (self, other) {
                (Self::Literal(Literal::Integer(_)), Self::Literal(Literal::Integer(_))) => true,
                (Self::Literal(Literal::Boolean(_)), Self::Literal(Literal::Boolean(_))) => true,
                (Self::Literal(Literal::String(_)), Self::Literal(Literal::String(_))) => true,
                (Self::Identifier(_), Self::Identifier(_)) => true,
                (Self::Comment(_), Self::Comment(_)) => true,
                _ => false,
            }
        }
    }
}

/// The Keywords we have
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Keyword {
    Program,
    Begin,
    End,
    Var,
    Print,
    Println,
    Get,
    While,
    If,
    Then,
    Else,
    Procedure,
    Return,
}

/// Possible Punctuation marks
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Punctuation {
    Comma,
    Semicolon,
    BracketOpen,
    BracketClose,
}

/// All the operators (including boolean ones)
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Operator {
    Assignement,
    Plus,
    Minus,
    Times,
    Divide,
    LesserThan,
    GreaterThan,
    Equals,
    GreaterOrEquals,
    LesserOrEquals,
    Or,
    And,
    Not,
}

/// Constants
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Literal<'a> {
    Integer(isize),
    Boolean(bool),
    String(&'a str),
}
