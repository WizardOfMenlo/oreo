//! Utilites (which are really really cool in my opinion)
//! to create nodes from token streams

use super::error::SyntaxError;
use super::{ExpToken, TokenList};
use crate::ast::untyped::{Node, NodeType};
use crate::lexer::token_stream::TokenStream;
use crate::lexer::tokens::Token;
use crate::range::Ranged;
use std::ops::Range;

type IdF<T> = fn(T) -> T;

/// A utility mapping to associate behaviour with
/// Tokens that are peeked
#[derive(Debug, Default)]
pub struct PeekMapping<T> {
    // Note, not an Hashmap as we compare by kind, not by eq
    list: Vec<(ExpToken, IdF<T>)>,
}

impl<T> PeekMapping<T> {
    /// Create a new mapping
    pub fn new() -> Self {
        PeekMapping { list: Vec::new() }
    }

    /// Add a function to be called on the token found
    pub fn add(mut self, tok: ExpToken, f: IdF<T>) -> Self {
        self.list.push((tok, f));
        self
    }

    /// Add function to be used for all of the tokens above
    pub fn add_all(mut self, tok: TokenList, f: IdF<T>) -> Self {
        self.list.extend(tok.iter().map(|e| (*e, f)));
        self
    }

    /// Apply the function to the builder
    pub fn apply<'a>(self, tok: &Token<'a>, builder: T) -> T {
        let f = self
            .list
            .iter()
            .find(|(t, _)| t.same_kind(tok))
            .map(|(_, f)| f)
            .expect("Invalid mapping with token list");
        f(builder)
    }
}

/// A struct that builds a node from a token stream
pub struct NodeBuilder<'a, 'b, T> {
    start: Option<usize>,
    end: Option<usize>,
    ty: Option<NodeType<'a>>,
    children: Vec<Node<'a>>,
    it: &'b mut T,
}

impl<'a, 'b, T: TokenStream<'a>> NodeBuilder<'a, 'b, T> {
    /// Create a new (typed) builder
    pub fn new(ty: NodeType<'a>, it: &'b mut T) -> Self {
        NodeBuilder {
            start: None,
            end: None,
            children: Vec::new(),
            ty: Some(ty),
            it,
        }
    }

    /// Creates a builder where the type is later determined
    pub fn new_untyped(it: &'b mut T) -> Self {
        NodeBuilder {
            start: None,
            end: None,
            children: Vec::new(),
            ty: None,
            it,
        }
    }

    /// Sets the type of the node
    pub fn ty(mut self, ty: NodeType<'a>) -> Self {
        self.ty = Some(ty);
        self
    }

    /// Is the node set as error
    pub fn is_error_node(&self) -> bool {
        self.ty.as_ref().map(|t| t.is_error()).unwrap_or(false)
    }

    fn update_given_range(&mut self, text_range: &Range<usize>) {
        // Update the start
        if self.start.is_none() {
            self.start = Some(text_range.start);
        }

        // Update the end
        self.end = Some(text_range.end);
    }

    /// Create a children using the function provided,
    /// and add it to this node's
    pub fn children<F>(mut self, f: F) -> Self
    where
        F: Fn(&mut T) -> Node<'a>,
    {
        // We skip if we have a error
        if self.is_error_node() {
            return self;
        }

        let new_node = f(self.it);

        self.update_given_range(new_node.range());

        self.children.push(new_node);

        self
    }

    /// Advance the token stream, while also returning the found token (useful for int, bool)
    pub fn advance_expecting_and_get(mut self, tok: ExpToken) -> (Self, Token<'a>) {
        // This is just for convenience
        use crate::lexer::error::LexicalError;

        // We skip if we have a error
        if self.is_error_node() {
            return (self, Token::Error(LexicalError::UnknownChar('?')));
        }

        let next = self.it.next();

        if next.is_none() {
            self.ty = Some(NodeType::Error(SyntaxError::ExpectedFoundEOF(tok)));
            return (self, Token::Error(LexicalError::UnknownChar('?')));
        }

        let found_token = next.unwrap();
        let inner = *found_token.inner();

        self.update_given_range(found_token.range());

        if !inner.same_kind(&tok) {
            self.ty = Some(NodeType::Error(SyntaxError::ExpectedFound(
                tok,
                found_token,
            )));
            return (self, inner);
        }

        (self, inner)
    }

    /// Advance expecting a token
    pub fn advance_expecting(self, tok: ExpToken) -> Self {
        self.advance_expecting_and_get(tok).0
    }

    /// Advance expecting one of multiple tokens
    pub fn advance_expecting_one_of(mut self, toks: TokenList) -> Self {
        // We skip if we have a error
        if self.is_error_node() {
            return self;
        }

        let next = self.it.next();

        if next.is_none() {
            self.ty = Some(NodeType::Error(SyntaxError::ExpectedOneOfFoundEOF(toks)));
            return self;
        }

        let found_token = next.unwrap();

        self.update_given_range(found_token.range());

        if !toks.iter().any(|t| found_token.inner().same_kind(t)) {
            self.ty = Some(NodeType::Error(SyntaxError::ExpectedOneOfFound(
                toks,
                found_token,
            )));
            return self;
        }

        self
    }

    /// Create children using the provided function until the required token is
    /// found left in the token stream
    pub fn take_until<F>(mut self, tok: ExpToken, f: F) -> Self
    where
        F: Fn(&mut T) -> Node<'a>,
    {
        // We skip if we have a error
        if self.is_error_node() {
            return self;
        }

        loop {
            if let Some(token) = self.it.peek() {
                let token = token.inner();
                if token.same_kind(&tok) {
                    break;
                }
                self.children.push(f(self.it));
            } else {
                self.ty = Some(NodeType::Error(SyntaxError::ExpectedFoundEOF(tok)));
                break;
            }
        }
        self
    }

    /// Peek the next character, and depending on it call the provided function
    pub fn peek_and_type(mut self, tokens: TokenList, mapping: PeekMapping<Self>) -> Self {
        // We skip if we have a error
        if self.is_error_node() {
            return self;
        }

        let next = self.it.peek().cloned();
        if let Some(tok) = next {
            if tokens.iter().any(|t| t.same_kind(&tok.inner())) {
                return mapping.apply(tok.inner(), self);
            } else {
                self.ty = Some(NodeType::Error(SyntaxError::ExpectedOneOfFound(
                    tokens,
                    tok.clone(),
                )));

                // Advance
                self.it.next();
                self.update_given_range(tok.range());
            }
        } else {
            self.ty = Some(NodeType::Error(SyntaxError::ExpectedOneOfFoundEOF(tokens)));
        }
        self
    }

    /// Version which does not error out in case the thing is not there
    pub fn peek_and_maybe_type(self, tokens: TokenList, mapping: PeekMapping<Self>) -> Self {
        // We skip if we have a error
        if self.is_error_node() {
            return self;
        }

        let next = self.it.peek().cloned();
        if let Some(tok) = next {
            if tokens.iter().any(|t| t.same_kind(&tok.inner())) {
                return mapping.apply(tok.inner(), self);
            }
        }
        self
    }

    /// Your classic do this if in set, do that otherwise, but with tokens!
    pub fn peek_if_else<F, G>(self, tokens: TokenList, f: F, el: G) -> Self
    where
        F: FnOnce(Self) -> Self,
        G: FnOnce(Self) -> Self,
    {
        // We skip if we have a error
        if self.is_error_node() {
            return self;
        }

        let next = self.it.peek();
        if let Some(tok) = next {
            if tokens.iter().any(|t| t.same_kind(&tok.inner())) {
                return f(self);
            } else {
                return el(self);
            }
        }
        self
    }

    /// Useful for expr and the likes
    pub fn add_tail<F>(self, tokens: TokenList, f: F) -> Self
    where
        F: Fn(&mut T) -> Node<'a>,
    {
        self.peek_if_else(tokens, |b| b.children(f), |b| b)
    }

    /// Get one of the built children (use sparingly)
    pub fn from_children(self, index: usize) -> Self {
        let child = &self.children[index];

        let rng = child.range().clone();
        
        NodeBuilder {
            start: Some(rng.start),
            end: Some(rng.end),
            children: child.children().cloned().collect(),
            ty: Some(child.ty().clone()),
            it: self.it,
        }
    }

    /// Build the node from the builder, panics if the type is not set yet
    pub fn build(self) -> Node<'a> {
        Node::new(
            self.ty.expect("Type has not been set"),
            self.start.unwrap_or(0)..self.end.unwrap_or(0),
            self.children,
        )
    }
}
