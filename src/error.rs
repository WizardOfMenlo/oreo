use crate::parser::{ExpToken, TokenList};
use crate::range::RangedObject;
use crate::tokens::LexicalError;
use crate::tokens::Token;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub enum SyntaxError<'a> {
    ExpectedOneOfFoundEOF(TokenList),
    ExpectedFoundEOF(ExpToken),
    ExpectedOneOfFound(TokenList, RangedObject<Token<'a>>),
    ExpectedFound(ExpToken, RangedObject<Token<'a>>),
}

pub fn tl_to_str(t: TokenList) -> String {
    format!("{:?}", t)
}

pub fn tok_to_str<'a>(t: &Token<'a>) -> String {
    format!("{:?}", t)
}

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

pub fn format_lexical_error(error: RangedObject<&LexicalError>, input: &str) -> String {
    let span = error.span(input);
    let (exp, found) = match error.inner() {
        LexicalError::ExpectedDoubleEqualsEOF => ('=', None),
        LexicalError::ExpectedAssignementEOF => ('=', None),
        LexicalError::ExpectedDoubleEquals(c) => ('=', Some(c)),
        LexicalError::ExpectedAssignement(c) => ('=', Some(c)),
        LexicalError::UnknownChar(c) => {
            return format!("Unknown char '{}' in {}.", c, span);
        }
        LexicalError::UnclosedString(s) => {
            return format!("Unclosed string \"{}\" in {}.", s, span);
        }
        LexicalError::UnclosedComment(s) => {
            return format!("Unclosed comment \"{}\" in {}.", s, span);
        }
    };

    format!(
        "Expected '{}', found '{}' in {}",
        exp,
        found
            .map(ToString::to_string)
            .unwrap_or_else(|| String::from("EOF")),
        span
    )
}
