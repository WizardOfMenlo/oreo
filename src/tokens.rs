#[derive(Debug, Clone, PartialEq)]
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
    pub fn is_error(&self) -> bool {
        match self {
            Token::Error(_) => true,
            _ => false,
        }
    }

    pub fn is_comment(&self) -> bool {
        match self {
            Token::Comment(_) => true,
            _ => false,
        }
    }

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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Punctuation {
    Comma,
    Semicolon,
    BracketOpen,
    BracketClose,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'a> {
    Integer(isize),
    Boolean(bool),
    String(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError<'a> {
    ExpectedDoubleEqualsEOF,
    ExpectedAssignementEOF,
    UnclosedString(&'a str),
    UnclosedComment(&'a str),
    ExpectedDoubleEquals(char),
    ExpectedAssignement(char),
    UnknownChar(char),
}
