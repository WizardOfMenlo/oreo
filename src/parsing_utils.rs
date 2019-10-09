use crate::lexical::EnrichedToken;
use crate::parser::{consts, ExpToken, SyntaxError, TokenList};
use crate::syntax::*;
use crate::tokens::*;
use std::iter::Peekable;

pub trait TokenStream<'a>: Iterator<Item = EnrichedToken<'a>> {
    fn peek(&mut self) -> Option<&EnrichedToken<'a>>;
}

pub struct CollectorStream<T> {
    inner: T,
}

impl<T> CollectorStream<T> {
    pub fn new(inner: T) -> Self {
        CollectorStream { inner }
    }
}

impl<'a, T> Iterator for CollectorStream<T>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    type Item = EnrichedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<'a, T> TokenStream<'a> for CollectorStream<Peekable<T>>
where
    T: Iterator<Item = EnrichedToken<'a>>,
{
    fn peek(&mut self) -> Option<&EnrichedToken<'a>> {
        self.inner.peek()
    }
}

#[must_use]
pub fn advance_expecting<'a>(
    it: &mut impl TokenStream<'a>,
    exp: ExpToken,
) -> Result<EnrichedToken<'a>, SyntaxError<'a>> {
    let next = it.next();
    match next {
        Some(tok) => {
            let found_token = tok.token();
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
) -> Result<EnrichedToken<'a>, SyntaxError<'a>> {
    let next = it.next();
    match next {
        Some(tok) => {
            let found_token = tok.token();
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
) -> Result<&EnrichedToken<'a>, SyntaxError<'a>> {
    let next = it.peek();
    match next {
        Some(tok) => {
            let found_token = tok.token();
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
pub fn get_id(id: EnrichedToken) -> Result<Identifier, SyntaxError> {
    let backup = id.clone();
    match id.take_token() {
        Token::Identifier(s) => Ok(Identifier(s)),
        _ => Err(SyntaxError::ExpectedFound(consts::ID, backup)),
    }
}

#[must_use]
pub fn advance_expecting_identifier<'a>(
    it: &mut impl Iterator<Item = EnrichedToken<'a>>,
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
    match it.peek().map(|t| possible_starts.contains(t.token())) {
        Some(true) => Ok(Some(func(it)?)),
        _ => Ok(None),
    }
}
