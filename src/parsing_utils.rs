use crate::parser::{consts, ExpToken, SyntaxError, TokenList};
use crate::range::RangedObject;
use crate::syntax::*;
use crate::tokens::*;
use std::iter::Peekable;

pub trait TokenStream<'a>: Iterator<Item = RangedObject<Token<'a>>> {
    fn peek(&mut self) -> Option<&RangedObject<Token<'a>>>;
}

pub struct ParserStream<T> {
    inner: T,
}

impl<T> ParserStream<T> {
    pub fn new(inner: T) -> Self {
        Self { inner }
    }
}

impl<'a, T> Iterator for ParserStream<T>
where
    T: Iterator<Item = RangedObject<Token<'a>>>,
{
    type Item = RangedObject<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<'a, T> TokenStream<'a> for ParserStream<Peekable<T>>
where
    T: Iterator<Item = RangedObject<Token<'a>>>,
{
    fn peek(&mut self) -> Option<&RangedObject<Token<'a>>> {
        self.inner.peek()
    }
}

#[must_use]
pub fn advance_expecting<'a>(
    it: &mut impl TokenStream<'a>,
    exp: ExpToken,
) -> Result<RangedObject<Token<'a>>, SyntaxError<'a>> {
    let next = it.next();
    match next {
        Some(tok) => {
            let found_token = tok.inner();
            if found_token.same_kind(&exp) {
                Ok(tok)
            } else {
                Err(SyntaxError::ExpectedFound(exp, tok))
            }
        }
        None => Err(SyntaxError::ExpectedButFoundEOF(exp)),
    }
}

#[must_use]
pub fn advance_expecting_one_of<'a>(
    it: &mut impl TokenStream<'a>,
    exp: TokenList,
) -> Result<RangedObject<Token<'a>>, SyntaxError<'a>> {
    let next = it.next();
    match next {
        Some(tok) => {
            let found_token = tok.inner();
            if exp.iter().any(|t| found_token.same_kind(t)) {
                Ok(tok)
            } else {
                Err(SyntaxError::ExpectedOneOfFound(exp, tok))
            }
        }
        None => Err(SyntaxError::ExpectedOneOfButFoundEOF(exp)),
    }
}

#[must_use]
pub fn peek_expecting_one_of<'a>(
    it: &mut impl TokenStream<'a>,
    exp: TokenList,
) -> Result<&RangedObject<Token<'a>>, SyntaxError<'a>> {
    let next = it.peek();
    match next {
        Some(tok) => {
            let found_token = tok.inner();
            if exp.iter().any(|t| found_token.same_kind(t)) {
                Ok(tok)
            } else {
                Err(SyntaxError::ExpectedOneOfFound(exp, tok.clone()))
            }
        }
        None => Err(SyntaxError::ExpectedOneOfButFoundEOF(exp)),
    }
}

#[must_use]
pub fn get_id(id: RangedObject<Token>) -> Result<Identifier, SyntaxError> {
    let backup = id.clone();
    match id.take_inner() {
        Token::Identifier(s) => Ok(Identifier(s)),
        _ => Err(SyntaxError::ExpectedFound(consts::ID, backup)),
    }
}

#[must_use]
pub fn advance_expecting_identifier<'a>(
    it: &mut impl Iterator<Item = RangedObject<Token<'a>>>,
) -> Result<Identifier<'a>, SyntaxError<'a>> {
    let next = it.next();
    match next {
        Some(tok) => get_id(tok),
        None => Err(SyntaxError::ExpectedButFoundEOF(consts::ID)),
    }
}

pub fn get_tail<'a, F, R, I>(
    it: &mut I,
    possible_starts: TokenList,
    func: F,
) -> Result<Option<R>, SyntaxError<'a>>
where
    I: TokenStream<'a>,
    F: Fn(&mut I) -> Result<R, SyntaxError<'a>>,
{
    match it.peek().map(|t| possible_starts.contains(t.inner())) {
        Some(true) => Ok(Some(func(it)?)),
        _ => Ok(None),
    }
}
