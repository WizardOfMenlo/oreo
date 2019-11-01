use crate::error::SyntaxError;
use crate::parser::{ExpToken, TokenList};
use crate::token_stream::TokenStream;
use crate::tokens::Token;
use crate::untyped::{Node, NodeType};
use std::ops::Range;

type IdF<T> = fn(T) -> T;

#[derive(Debug, Default)]
pub struct PeekMapping<T> {
    list: Vec<(ExpToken, IdF<T>)>,
}

impl<T> PeekMapping<T> {
    pub fn new() -> Self {
        PeekMapping { list: Vec::new() }
    }

    pub fn add(mut self, tok: ExpToken, f: IdF<T>) -> Self {
        self.list.push((tok, f));
        self
    }

    pub fn add_all(mut self, tok: TokenList, f: IdF<T>) -> Self {
        self.list.extend(tok.iter().map(|e| (*e, f)));
        self
    }

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

pub struct NodeBuilder<'a, 'b, T> {
    start: Option<usize>,
    end: Option<usize>,
    ty: Option<NodeType<'a>>,
    children: Vec<Node<'a>>,
    it: &'b mut T,
}

impl<'a, 'b, T: TokenStream<'a>> NodeBuilder<'a, 'b, T> {
    pub fn new(ty: NodeType<'a>, it: &'b mut T) -> Self {
        NodeBuilder {
            start: None,
            end: None,
            children: Vec::new(),
            ty: Some(ty),
            it,
        }
    }

    pub fn new_untyped(it: &'b mut T) -> Self {
        NodeBuilder {
            start: None,
            end: None,
            children: Vec::new(),
            ty: None,
            it,
        }
    }

    pub fn ty(mut self, ty: NodeType<'a>) -> Self {
        self.ty = Some(ty);
        self
    }

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

    pub fn children<F>(mut self, f: F) -> Self
    where
        F: Fn(&mut T) -> Node<'a>,
    {
        // We skip if we have a error
        if self.is_error_node() {
            return self;
        }

        let new_node = f(self.it);

        self.update_given_range(&new_node.text_range);

        self.children.push(new_node);

        self
    }

    pub fn advance_expecting_and_get(mut self, tok: ExpToken) -> (Self, Token<'a>) {
        // This is just for convenience
        use crate::tokens::LexicalError;

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

    pub fn advance_expecting(self, tok: ExpToken) -> Self {
        self.advance_expecting_and_get(tok).0
    }

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

    // Version which does not error out in case the thing is not there
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

    // Your classic do this if in set, do that otherwise
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

    pub fn add_tail<F>(self, tokens: TokenList, f: F) -> Self
    where
        F: Fn(&mut T) -> Node<'a>,
    {
        self.peek_if_else(tokens, |b| b.children(f), |b| b)
    }

    pub fn build(self) -> Node<'a> {
        // TODO: we might want to check for non null
        Node {
            ty: self.ty.expect("Type has not been set"),
            children: self.children,
            text_range: self.start.unwrap_or(0)..self.end.unwrap_or(0),
        }
    }
}
