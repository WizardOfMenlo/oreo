//! Mod with various utilities for walking the parse tree

use crate::ast::untyped::Node;
use std::collections::VecDeque;

impl<'a> Node<'a> {
    /// Create an iterator that walks the tree depth first
    /// ```
    /// use oreo::ast::untyped::{Node, NodeType};
    /// // Invalid but explicative example
    /// let node = Node::new(NodeType::Program, 0..10, vec![
    ///     Node::new(NodeType::Compound, 0..5, vec![
    ///         Node::new(NodeType::If, 0..3, Vec::new()),
    ///     ]),
    ///     Node::new(NodeType::While, 5..10, Vec::new())
    /// ]);
    ///
    /// let mut iter = node.iter_depth_first();
    ///
    /// assert_eq!(iter.next().unwrap().ty(), &NodeType::Program);
    /// assert_eq!(iter.next().unwrap().ty(), &NodeType::Compound);
    /// assert_eq!(iter.next().unwrap().ty(), &NodeType::If);
    /// assert_eq!(iter.next().unwrap().ty(), &NodeType::While);
    ///
    /// ```
    pub fn iter_depth_first<'b>(&'b self) -> DepthFirstNonOwningNodeIt<'a, 'b> {
        DepthFirstNonOwningNodeIt { nodes: vec![self] }
    }

    /// Create an iterator that walks the tree depth first
    pub fn iter_breadth_first<'b>(&'b self) -> BreadthFirstNonOwningNodeIt<'a, 'b> {
        let mut nodes = VecDeque::new();
        nodes.push_front(self);
        BreadthFirstNonOwningNodeIt { nodes }
    }

    /// Create an iterator that walks the tree depth first
    /// ```
    /// use oreo::ast::untyped::{Node, NodeType};
    /// // Invalid but explicative example
    /// let node = Node::new(NodeType::Program, 0..10, vec![
    ///     Node::new(NodeType::Compound, 0..5, vec![
    ///         Node::new(NodeType::If, 0..3, Vec::new()),
    ///     ]),
    ///     Node::new(NodeType::While, 5..10, Vec::new())
    /// ]);
    ///
    /// let mut iter = node.iter_depth_first();
    ///
    /// assert_eq!(iter.next().unwrap().ty(), &NodeType::Program);
    /// assert_eq!(iter.next().unwrap().ty(), &NodeType::Compound);
    /// assert_eq!(iter.next().unwrap().ty(), &NodeType::If);
    /// assert_eq!(iter.next().unwrap().ty(), &NodeType::While);
    ///
    /// ```
    pub fn into_iter_depth_first(self) -> DepthFirstNodeIt<'a> {
        DepthFirstNodeIt { nodes: vec![self] }
    }

    /// Create an iterator that walks the tree depth first
    pub fn into_iter_breadth_first(self) -> BreadthFirstNodeIt<'a> {
        let mut nodes = VecDeque::new();
        nodes.push_front(self);
        BreadthFirstNodeIt { nodes }
    }
}

/// An iterator that walks the nodes depth first (no ownership)
pub struct DepthFirstNonOwningNodeIt<'a, 'b> {
    nodes: Vec<&'b Node<'a>>,
}

impl<'a, 'b> Iterator for DepthFirstNonOwningNodeIt<'a, 'b> {
    type Item = &'b Node<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.nodes.pop();
        node.map(|node| {
            self.nodes.extend(node.children().rev());
            node
        })
    }
}

/// An iterator that walks the nodes depth first
pub struct DepthFirstNodeIt<'a> {
    nodes: Vec<Node<'a>>,
}

impl<'a> Iterator for DepthFirstNodeIt<'a> {
    type Item = Node<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.nodes.pop();
        node.map(|node| {
            self.nodes.extend(node.children().cloned().rev());
            node
        })
    }
}

/// An iterator that walks the nodes breadth first (no ownership)
pub struct BreadthFirstNonOwningNodeIt<'a, 'b> {
    nodes: VecDeque<&'b Node<'a>>,
}

impl<'a, 'b> Iterator for BreadthFirstNonOwningNodeIt<'a, 'b> {
    type Item = &'b Node<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.nodes.pop_front();
        node.map(|node| {
            self.nodes.extend(node.children().rev());
            node
        })
    }
}

/// Iterator that walks the tree breadth first
pub struct BreadthFirstNodeIt<'a> {
    nodes: VecDeque<Node<'a>>,
}

impl<'a> Iterator for BreadthFirstNodeIt<'a> {
    type Item = Node<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.nodes.pop_front();
        node.map(|node| {
            self.nodes.extend(node.children().cloned());
            node
        })
    }
}

impl<'a> IntoIterator for Node<'a> {
    type Item = Node<'a>;
    type IntoIter = BreadthFirstNodeIt<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_iter_breadth_first()
    }
}
