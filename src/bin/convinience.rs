use oreo::ast::node_db::NodeDb;
use oreo::ast::syntax::Program;

fn main() {
    let input = r#"program fib begin procedure sum(var x ~ int, var y ~ str) begin return x + y; end end"#;
    let node = oreo::parse(input);

    dbg!(node);
}
