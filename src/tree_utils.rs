use crate::ast::untyped::Node;
use std::collections::VecDeque;

impl<'a> Node<'a> {
    pub fn iter_depth_first<'b>(&'b self) -> DepthFirstNonOwningNodeIt<'a, 'b> {
        DepthFirstNonOwningNodeIt { nodes: vec![self] }
    }

    pub fn iter_breadth_first<'b>(&'b self) -> BreadthFirstNonOwningNodeIt<'a, 'b> {
        let mut nodes = VecDeque::new();
        nodes.push_front(self);
        BreadthFirstNonOwningNodeIt { nodes }
    }

    pub fn into_iter_depth_first(self) -> DepthFirstNodeIt<'a> {
        DepthFirstNodeIt { nodes: vec![self] }
    }

    pub fn into_iter_breadth_first(self) -> BreadthFirstNodeIt<'a> {
        let mut nodes = VecDeque::new();
        nodes.push_front(self);
        BreadthFirstNodeIt { nodes }
    }
}

pub struct DepthFirstNonOwningNodeIt<'a, 'b> {
    nodes: Vec<&'b Node<'a>>,
}

impl<'a, 'b> Iterator for DepthFirstNonOwningNodeIt<'a, 'b> {
    type Item = &'b Node<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.nodes.pop();
        node.map(|node| {
            self.nodes.extend(node.children.iter());
            node
        })
    }
}

pub struct DepthFirstNodeIt<'a> {
    nodes: Vec<Node<'a>>,
}

impl<'a> Iterator for DepthFirstNodeIt<'a> {
    type Item = Node<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.nodes.pop();
        node.map(|node| {
            let childrens = node.children.clone();
            self.nodes.extend(childrens);
            node
        })
    }
}

pub struct BreadthFirstNonOwningNodeIt<'a, 'b> {
    nodes: VecDeque<&'b Node<'a>>,
}

impl<'a, 'b> Iterator for BreadthFirstNonOwningNodeIt<'a, 'b> {
    type Item = &'b Node<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.nodes.pop_front();
        node.map(|node| {
            self.nodes.extend(node.children.iter());
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
            let childrens = node.children.clone();
            self.nodes.extend(childrens);
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
