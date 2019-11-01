use super::tokens::Token;
use crate::range::RangedObject;
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
