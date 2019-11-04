use oreo::ast::node_db::NodeDb;
use oreo::ast::symbol::*;
use oreo::ast::syntax::Program;

fn main() {
    let input = r#"program id begin
                   if ( true ) then begin var n := 5;  end
       			   else begin var k := 46; end; end"#;
    let node = oreo::parse(input);

    let db = NodeDb::new(node);
    let table = SymbolTableBuilder::new(input, &db).build(Program::new(db.start_id()));
    dbg!(table);
}
