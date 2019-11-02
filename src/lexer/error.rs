//! Utilities for handling errors in the lexing part

use crate::range::*;
use serde::Serialize;

/// Possible errors that we encounter parsing
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum LexicalError<'a> {
    ExpectedDoubleEqualsEOF,
    ExpectedAssignementEOF,
    UnclosedString(&'a str),
    UnclosedComment(&'a str),
    ExpectedDoubleEquals(char),
    ExpectedAssignement(char),
    UnknownChar(char),
}


/// Format a lexical error for output
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
