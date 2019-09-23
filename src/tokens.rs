#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Keyword(Keyword),
    Punctuation(Punctuation),
    Operator(Operator),
    Literal(Literal<'a>),
    Identifier(&'a str),
    Error(LexicalError<'a>),
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Punctuation {
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
    ExpectedDoubleEquals(char),
    ExpectedAssignement(char),
    UnknownChar(char),
}
