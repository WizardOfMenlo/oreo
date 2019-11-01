use crate::ast::untyped::Node;

pub fn for_each_depth_first<'a, F>(node: &Node<'a>, f: F)
where
    F: Fn(&Node<'a>) + Clone,
{
    f.clone()(node);

    node.children
        .iter()
        .for_each(|n| for_each_depth_first(n, f.clone()));
}

pub fn validate_depth_first<'a, F>(node: &Node<'a>, f: F) -> bool
where
    F: Fn(&Node<'a>) -> bool,
    F: Clone,
{
    f.clone()(node)
        && node
            .children
            .iter()
            .all(|n| validate_depth_first(n, f.clone()))
}
