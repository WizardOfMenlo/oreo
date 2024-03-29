//! Error the can be encountered in parsing

use crate::lexer::tokens::Token;
use crate::parser::{ExpToken, TokenList};
use crate::range::*;

use serde::Serialize;

/// Representing possible errors during parsing
#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum SyntaxError<'a> {
    /// Was looking for one of those, found EOF
    ExpectedOneOfFoundEOF(TokenList),
    /// Was looking for this, found EOF
    ExpectedFoundEOF(ExpToken),

    /// Was looking for one of those found that
    ExpectedOneOfFound(TokenList, RangedObject<Token<'a>>),

    /// Was looking for this found that
    ExpectedFound(ExpToken, RangedObject<Token<'a>>),
}

fn tl_to_str(t: TokenList) -> String {
    format!("{:?}", t)
}

fn tok_to_str<'a>(t: &Token<'a>) -> String {
    format!("{:?}", t)
}

/// Format the syntax error for outputting
pub fn format_syntax_error<'a>(error: RangedObject<SyntaxError<'a>>, input: &'a str) -> String {
    let span = error.span(input);
    let (exp, found) = match error.inner() {
        SyntaxError::ExpectedOneOfFound(tl, f) => (tl_to_str(tl), tok_to_str(f.inner())),
        SyntaxError::ExpectedOneOfFoundEOF(tl) => (tl_to_str(tl), "EOF".to_string()),
        SyntaxError::ExpectedFound(t, f) => (tok_to_str(t), tok_to_str(f.inner())),
        SyntaxError::ExpectedFoundEOF(t) => (tok_to_str(t), "EOF".to_string()),
    };

    format!("Expected: {}, found {} in: \n {}", exp, found, span)
}
