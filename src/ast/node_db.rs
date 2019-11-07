//! Something

use super::untyped::Node;
use std::collections::HashMap;
use std::marker::PhantomPinned;
use std::pin::Pin;
use std::ptr::NonNull;

/// An id for a node
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct NodeId(usize);

/// A node that is flat
#[derive(Debug, Clone)]
pub struct IdNode {
    childrens: Vec<NodeId>,
}

impl IdNode {
    /// Gets the children of this node
    pub fn children(&self) -> &[NodeId] {
        &self.childrens
    }
}

/// A database that efficiently allows for querying and retrivieng nodes
#[derive(Debug)]
pub struct NodeDb<'a> {
    node: Node<'a>,
    id_nodes: HashMap<NodeId, IdNode>,
    nodes: HashMap<NodeId, NonNull<Node<'a>>>,
    start_node_id: NodeId,
    _phantom: PhantomPinned,
}

impl<'a> NodeDb<'a> {
    /// Creates a db from a node
    pub fn new(node: Node<'a>) -> Pin<Box<Self>> {
        // Ok this is complex and merit some discussion
        // In order to efficiently (aka not storing nodes and children twice)
        // store nodes, we aim to store a reference to the children of a stored not
        // Ordinarly, rust wouldn't like that, so we need to bend the rules a little
        // First of all, this is a self referential struct, i.e. one that has references upon itself
        // (in particular node is borrowed by nodes)
        // Since moving the struct would invalidate this references, we use the Pin API, which allows to
        // to define types that cannot be moved
        // Using this, we can make our thing work and be efficient!
        let db = NodeDb {
            node,
            _phantom: PhantomPinned,
            // Temp
            id_nodes: HashMap::new(),
            nodes: HashMap::new(),
            start_node_id: NodeId(0),
        };

        let mut boxed = Box::pin(db);

        let (id, id_nodes, nodes) =
            add_node(&boxed.node, NodeId(0), HashMap::new(), HashMap::new());

        // I claim this is safe, as we are never moving the box and such not
        // violating the Pin API
        unsafe {
            let reference = Pin::as_mut(&mut boxed);
            let inner = Pin::get_unchecked_mut(reference);
            inner.start_node_id = NodeId(id.0.saturating_sub(1));
            inner.id_nodes = id_nodes;
            inner.nodes = nodes;
        }

        boxed
    }

    /// All the ids that this struct owns
    pub fn get_all_node_ids<'b>(&'b self) -> impl Iterator<Item = NodeId> + 'b {
        self.id_nodes.keys().cloned()
    }

    /// Get a flat representation of the node
    pub fn get_flat_node(&self, id: NodeId) -> Option<&IdNode> {
        self.id_nodes.get(&id)
    }

    /// Get the start id (ie the top node id)
    pub fn start_id(&self) -> NodeId {
        self.start_node_id
    }

    /// Get a reference to the full node
    pub fn get_node(&self, id: NodeId) -> Option<&Node<'a>> {
        // Note this should be safe as p always refer to a node in the
        // struct
        self.nodes.get(&id).map(|p| unsafe { p.as_ref() })
    }

    /// Get all the children that this node has
    pub fn all_children(&self, id: NodeId) -> Vec<NodeId> {
        let mut res = Vec::new();
        for child in self
            .get_flat_node(id)
            .map(|n| n.children())
            .unwrap_or_default()
        {
            res.push(*child);
            res.extend(self.all_children(*child));
        }
        res
    }
}

type NodeIdMap = HashMap<NodeId, IdNode>;
type NodeMap<'a> = HashMap<NodeId, NonNull<Node<'a>>>;

// Not in class to circumvent (safely) rust borrowing rules
fn add_node<'a>(
    n: &Node<'a>,
    mut id: NodeId,
    mut id_nodes: NodeIdMap,
    mut nodes: NodeMap<'a>,
) -> (NodeId, NodeIdMap, NodeMap<'a>) {
    let mut ids = Vec::new();
    for c in n.children() {
        let (n_id, n_id_nodes, n_nodes) = add_node(c, id, id_nodes.clone(), nodes.clone());
        id = n_id;
        id_nodes.extend(n_id_nodes);
        nodes.extend(n_nodes);
        ids.push(NodeId(id.0 - 1));
    }

    id_nodes.insert(id, IdNode { childrens: ids });

    nodes.insert(id, NonNull::from(n));

    (NodeId(id.0 + 1), id_nodes, nodes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    fn node_from_str(input: &str) -> Node {
        use crate::lexer::lexicalize;
        use crate::lexer::scanner::scan;
        use crate::parser::parse;

        parse(lexicalize(scan(input)))
    }

    // We use this to test, as the Db does not serialize consistently
    fn get_all_nodes<'a>(db: &NodeDb<'a>) -> Vec<Node<'a>> {
        let mut ids: Vec<_> = db.get_all_node_ids().collect();
        ids.sort_by(|a, b| a.0.cmp(&b.0));

        ids.into_iter()
            .map(|i| db.get_node(i).expect("This should be failproof"))
            .cloned()
            .collect()
    }

    #[test]
    fn node_db_minimal() {
        let input = "program fib begin var n; end";
        let db = NodeDb::new(node_from_str(input));
        let nodes = get_all_nodes(&db);
        assert_debug_snapshot!(nodes);
    }

    #[test]
    fn node_db_minimal_assignments() {
        let input = "program fib begin var n := \"Hello\"; var m := 2; var s := (2); end";
        let db = NodeDb::new(node_from_str(input));
        let nodes = get_all_nodes(&db);
        assert_debug_snapshot!(nodes);
    }

    #[test]
    fn node_db_print_simple() {
        let input = "program fib begin print \"Hello\\n\"; println 2; get x; end";
        let db = NodeDb::new(node_from_str(input));
        let nodes = get_all_nodes(&db);
        assert_debug_snapshot!(nodes);
    }
}
