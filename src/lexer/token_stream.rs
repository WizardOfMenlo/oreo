//! Traits for stream that yields tokens (and also is peekable)

use super::tokens::Token;
use crate::range::RangedObject;
use std::iter::Peekable;

/// A trait that yields tokens, and that can be peeked
pub trait TokenStream<'a>: Iterator<Item = RangedObject<Token<'a>>> {
    /// See what the next token in the stream (if any), is
    fn peek(&mut self) -> Option<&RangedObject<Token<'a>>>;
}

/// Impl struct
pub struct ParserStream<T> {
    inner: T,
}

impl<T> ParserStream<T> {
    /// Creates a new stream
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
