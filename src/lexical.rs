use crate::scanner::{Scanned, ScannedItem};
use crate::tokens::{Keyword, LexicalError, Literal, Operator, Punctuation, Token};

use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref SINGLE_CHAR_TOKEN: HashMap<char, Token<'static>> = {
        let mut m = HashMap::new();
        m.insert('(', Token::Punctuation(Punctuation::BracketOpen));
        m.insert(')', Token::Punctuation(Punctuation::BracketClose));
        m.insert(';', Token::Punctuation(Punctuation::Semicolon));

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

        m.insert("or", Token::Operator(Operator::Or));
        m.insert("and", Token::Operator(Operator::And));
        m.insert("not", Token::Operator(Operator::Not));
        m
    };
}

pub fn lexicalize<'a>(it: impl Iterator<Item = Scanned<'a>>) -> impl Iterator<Item = Token<'a>> {
    it.map(lexicalize_one).flatten()
}

fn lexicalize_one<'a>(scan: Scanned<'a>) -> impl Iterator<Item = Token<'a>> {
    LexicalIt::new(scan)
}

struct LexicalIt<'a> {
    s: Scanned<'a>,
    curr_position: usize,
    terminate: bool,
}

impl<'a> LexicalIt<'a> {
    fn new(s: Scanned<'a>) -> Self {
        LexicalIt {
            s,
            curr_position: 0,
            terminate: false,
        }
    }
}

impl<'a> Iterator for LexicalIt<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.terminate {
            return None;
        }

        let remaining = match self.s.inner {
            ScannedItem::Rest(s) => &s[self.curr_position..],
            ScannedItem::Str(s) => {
                self.terminate = true;
                return Some(Token::Literal(Literal::String(s)));
            }
            ScannedItem::UnclosedStr(s) => {
                self.terminate = true;
                return Some(Token::Error(LexicalError::UnclosedString(s)));
            }
        };

        if remaining.len() == 0 {
            return None;
        }

        let mut chars = remaining.chars();
        let start_char = chars.next().unwrap();

        // This resolves all unambiguos single chars
        if SINGLE_CHAR_TOKEN.contains_key(&start_char) {
            self.curr_position += 1;
            return SINGLE_CHAR_TOKEN.get(&start_char).cloned();
        }

        // Handle the single unambiguos chars
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
                _ => return Some(Token::Error(LexicalError::UnknownChar('?'))),
            };

            self.curr_position += 1 + move_forward as usize;
            return Some(token);
        }

        // Get a integer literal
        if start_char.is_numeric() {
            let (token, read) = parse_literal(remaining);
            self.curr_position += read;
            return Some(token);
        }

        // Handle stuff that looks like identifiers
        if start_char.is_alphabetic() || start_char == '_' {
            let (token, read) = parse_identifier(remaining);
            self.curr_position += read;
            return Some(token);
        }

        self.curr_position += 1;
        Some(Token::Error(LexicalError::UnknownChar(start_char)))
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
    use crate::scanner::scan;
    use insta::assert_debug_snapshot;

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
    fn lex_real_life() {
        let input = "program fib;\r\nbegin\r\nvar n;\r\nvar first := 0;\r\nvar second :=1;\r\nvar next;\r\nvar c :=0 ;\r\nprint \"enter the number of terms\";\r\nget n;\r\nwhile ( c < n)\r\nbegin\r\nif ( c <= 1)\r\nthen begin next := c; end\r\nelse begin\r\n next := first + second;\r\n second := next;\r\nend\r\nprint next;\r\nc := c + 1;\r\nend\r\nend\r\n";
        let res: Vec<_> = lexicalize(scan(input)).collect();
        assert_debug_snapshot!(res);
    }
}
