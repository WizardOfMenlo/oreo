use oreo::ast::node_db::NodeDb;
use oreo::ast::scope_resolution::*;
use oreo::ast::symbol::*;
use oreo::ast::syntax::Program;

fn main() {
    let input = r#"
        program id begin
            var top := 10;
            if ( true ) then
            begin
                var n := top;
                var l := n;
            end
            else 
            begin 
                var k := top; 
            end;
        end"#;
    let node = oreo::parse(input);

    let db = NodeDb::new(node);

    dbg!(&db);

    let progr = Program::new(db.start_id());

    let table = SymbolTableBuilder::new(input, &db).build(progr);

    dbg!(&table);

    let resolver = VariableResolverBuilder::new(input, &table, &db).build(progr);
    dbg!(resolver);
}
