use oreo::ast::node_db::NodeDb;

fn main() {
    let node = oreo::parse("program f begin var x; end");
    let db = NodeDb::new(node);
    dbg!(db);
}