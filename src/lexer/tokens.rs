//! Definition of all the tokens that we use

use serde::Serialize;
use super::error::LexicalError;

/// The main token type that we work on
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Token<'a> {
    Keyword(Keyword),
    Punctuation(Punctuation),
    Operator(Operator),
    Literal(Literal<'a>),
    Identifier(&'a str),
    Comment(&'a str),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Punctuation {
    Comma,
    Semicolon,
    BracketOpen,
    BracketClose,
}

/// All the operators (including boolean ones)
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Literal<'a> {
    Integer(isize),
    Boolean(bool),
    String(&'a str),
}

