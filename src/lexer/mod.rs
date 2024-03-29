//! Lexing facilities for building a token stream

pub mod error;
pub mod scanner;
pub mod token_stream;
pub mod tokens;

use crate::range::*;
use error::LexicalError;
use scanner::ScannedItem;
use tokens::*;

use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref SINGLE_CHAR_TOKEN: HashMap<char, Token<'static>> = {
        let mut m = HashMap::new();
        m.insert('(', Token::Punctuation(Punctuation::BracketOpen));
        m.insert(')', Token::Punctuation(Punctuation::BracketClose));
        m.insert(';', Token::Punctuation(Punctuation::Semicolon));
        m.insert(',', Token::Punctuation(Punctuation::Comma));
        m.insert('~', Token::Punctuation(Punctuation::Tilde));

        m.insert('+', Token::Operator(Operator::Plus));
        m.insert('-', Token::Operator(Operator::Minus));
        m.insert('*', Token::Operator(Operator::Times));
        m.insert('/', Token::Operator(Operator::Divide));

        m
    };
    static ref KEYWORDS: HashMap<&'static str, Token<'static>> = {
        let mut m = HashMap::new();
        m.insert("program", Token::Keyword(Keyword::Program));
        m.insert("begin", Token::Keyword(Keyword::Begin));
        m.insert("end", Token::Keyword(Keyword::End));
        m.insert("var", Token::Keyword(Keyword::Var));
        m.insert("print", Token::Keyword(Keyword::Print));
        m.insert("println", Token::Keyword(Keyword::Println));
        m.insert("get", Token::Keyword(Keyword::Get));
        m.insert("while", Token::Keyword(Keyword::While));
        m.insert("if", Token::Keyword(Keyword::If));
        m.insert("then", Token::Keyword(Keyword::Then));
        m.insert("else", Token::Keyword(Keyword::Else));
        m.insert("procedure", Token::Keyword(Keyword::Procedure));
        m.insert("return", Token::Keyword(Keyword::Return));

        m.insert("or", Token::Operator(Operator::Or));
        m.insert("and", Token::Operator(Operator::And));
        m.insert("not", Token::Operator(Operator::Not));

        m.insert("true", Token::Literal(Literal::Boolean(true)));
        m.insert("false", Token::Literal(Literal::Boolean(false)));

        m.insert("bool", Token::Type(Type::Boolean));
        m.insert("int", Token::Type(Type::Integer));
        m.insert("str", Token::Type(Type::Str));
        m.insert("void", Token::Type(Type::Void));

        m
    };
}

/// From some scanned objects, creates an iterator that yields tokens
pub fn lexicalize<'a>(
    it: impl Iterator<Item = RangedObject<ScannedItem<'a>>>,
) -> impl Iterator<Item = RangedObject<Token<'a>>> {
    it.map(LexicalIt::new).flatten()
}

// Struct that parses tokens from scanned items
struct LexicalIt<'a> {
    s: RangedObject<ScannedItem<'a>>,
    curr_position: usize,
    terminate: bool,
}

impl<'a> LexicalIt<'a> {
    fn new(s: RangedObject<ScannedItem<'a>>) -> Self {
        LexicalIt {
            s,
            curr_position: 0,
            terminate: false,
        }
    }
}

impl<'a> Iterator for LexicalIt<'a> {
    type Item = RangedObject<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.terminate {
            return None;
        }

        let remaining = match self.s.inner() {
            ScannedItem::Rest(s) => &s[self.curr_position..],

            // Equivalent of std::iter::once
            ScannedItem::Str(s) => {
                self.terminate = true;
                return Some(RangedObject::new(
                    Token::Literal(Literal::Str(s)),
                    self.s.range().clone(),
                ));
            }

            ScannedItem::Comment(s) => {
                self.terminate = true;
                return Some(RangedObject::new(Token::Comment(s), self.s.range().clone()));
            }

            ScannedItem::UnclosedComment(s) => {
                self.terminate = true;
                return Some(RangedObject::new(
                    Token::Error(LexicalError::UnclosedComment(s)),
                    self.s.range().clone(),
                ));
            }

            ScannedItem::UnclosedStr(s) => {
                self.terminate = true;
                return Some(RangedObject::new(
                    Token::Error(LexicalError::UnclosedString(s)),
                    self.s.range().clone(),
                ));
            }
        };

        if remaining.is_empty() {
            return None;
        }

        let offset = self.s.range().start;
        let initial_pos = self.curr_position + offset;

        let mut chars = remaining.chars();
        let start_char = chars.next().unwrap();

        // This resolves all unambiguos single chars
        if SINGLE_CHAR_TOKEN.contains_key(&start_char) {
            self.curr_position += 1;
            return SINGLE_CHAR_TOKEN
                .get(&start_char)
                .cloned()
                .map(|s| RangedObject::new(s, initial_pos..self.curr_position + offset));
        }

        // Handle the single ambiguos chars
        if start_char == '<' || start_char == '>' || start_char == ':' || start_char == '=' {
            let next = chars.next();
            let (token, move_forward) = match (start_char, next) {
                ('<', Some('=')) => (Token::Operator(Operator::LesserOrEquals), true),
                ('<', None) | ('<', Some(_)) => (Token::Operator(Operator::LesserThan), false),

                ('>', Some('=')) => (Token::Operator(Operator::GreaterOrEquals), true),
                ('>', None) | ('>', Some(_)) => (Token::Operator(Operator::GreaterThan), false),

                (':', Some('=')) => (Token::Operator(Operator::Assignement), true),
                (':', Some(x)) => (Token::Error(LexicalError::ExpectedAssignement(x)), false),
                (':', None) => (Token::Error(LexicalError::ExpectedAssignementEOF), false),

                ('=', Some('=')) => (Token::Operator(Operator::Equals), true),
                ('=', Some(x)) => (Token::Error(LexicalError::ExpectedDoubleEquals(x)), false),
                ('=', None) => (Token::Error(LexicalError::ExpectedDoubleEqualsEOF), false),

                // If we get here, a lot must be wrong
                _ => {
                    return Some(RangedObject::new(
                        Token::Error(LexicalError::UnknownChar('?')),
                        initial_pos..self.curr_position + offset,
                    ))
                }
            };

            self.curr_position += 1 + move_forward as usize;
            return Some(RangedObject::new(
                token,
                initial_pos..self.curr_position + offset,
            ));
        }

        // Get a integer literal
        if start_char.is_numeric() {
            let (token, read) = parse_literal(remaining);
            self.curr_position += read;
            return Some(RangedObject::new(
                token,
                initial_pos..self.curr_position + offset,
            ));
        }

        // Handle stuff that looks like identifiers
        if start_char.is_alphabetic() || start_char == '_' {
            let (token, read) = parse_identifier(remaining);
            self.curr_position += read;
            return Some(RangedObject::new(
                token,
                initial_pos..self.curr_position + offset,
            ));
        }

        self.curr_position += 1;
        Some(RangedObject::new(
            Token::Error(LexicalError::UnknownChar(start_char)),
            initial_pos..self.curr_position + offset,
        ))
    }
}

fn parse_literal(input: &str) -> (Token, usize) {
    // ['1', '0', 'a']
    //             ^
    // ['1', '0']
    // 10
    let first_non_number = input
        .find(|c: char| !c.is_numeric())
        .unwrap_or_else(|| input.len());
    let literal = &input[0..first_non_number];
    (
        Token::Literal(Literal::Integer(literal.parse().unwrap())),
        first_non_number,
    )
}

fn parse_identifier(input: &str) -> (Token, usize) {
    let first_non_id = input
        .find(|c: char| !(c.is_alphanumeric() || c == '_'))
        .unwrap_or_else(|| input.len());
    let id = &input[0..first_non_id];

    if KEYWORDS.contains_key(id) {
        return (KEYWORDS.get(id).cloned().unwrap(), first_non_id);
    }

    (Token::Identifier(id), first_non_id)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;
    use scanner::scan;

    #[test]
    fn lex_empty() {
        let mut res = lexicalize(scan(""));
        assert_eq!(res.next(), None);
    }

    #[test]
    fn lex_example() {
        let res: Vec<_> = lexicalize(scan("(x <= > >= < y 99.88l8 )")).collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn lex_functions() {
        let res: Vec<_> =
            lexicalize(scan("procedure id(var x, var y) begin return x; end")).collect();
        assert_debug_snapshot!(res);
    }

    #[test]
    fn lex_bool() {
        let input = "program fib begin var n := true; end";
        let parsed: Vec<_> = lexicalize(scan(input)).collect();
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn lex_types() {
        let input = "bool ~ str; int";
        let parsed: Vec<_> = lexicalize(scan(input)).collect();
        assert_debug_snapshot!(parsed);
    }

    #[test]
    fn lex_real_life() {
        let input = "program fib;\r\nbegin\r\nvar n;\r\nvar first := 0;\r\nvar second :=1;\r\nvar next;\r\nvar c :=0 ;\r\nprint \"enter the number of terms\";\r\nget n;\r\nwhile ( c < n)\r\nbegin\r\nif ( c <= 1)\r\nthen begin next := c; end\r\nelse begin\r\n next := first + second;\r\n second := next;\r\nend\r\nprint next;\r\nc := c + 1;\r\nend\r\nend\r\n";
        let res: Vec<_> = lexicalize(scan(input)).collect();
        assert_debug_snapshot!(res);
    }
}
