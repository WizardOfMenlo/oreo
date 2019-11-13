use oreo::ast::scope_resolution::*;
use oreo::ast::node_db::*;
use oreo::ast::symbol::*;
use oreo::ast::syntax::*;
use oreo::ast::types::{self, *};

fn db_from_str(input: &str) -> NodeDbWrap {
    use oreo::lexer::lexicalize;
    use oreo::lexer::scanner::scan;
    use oreo::parser::parse;

    let node = parse(lexicalize(scan(input)));
    NodeDb::new(node)
}

fn sym_table_from<'a>(input: &'a str, db: &NodeDb<'a>) -> SymbolTable<'a> {
    SymbolTableBuilder::new(input, db).build(Program::new(db.start_id()))
}

fn resolver<'a>(input: &'a str, db: &NodeDb<'a>, sym: &SymbolTable<'a>) -> VariableResolver {
    VariableResolverBuilder::new(input, sym, db)
        .build(Program::new(db.start_id()))
        .unwrap()
}



fn main() {
    let input = r#"program x
        begin
            procedure int f(var x ~ int)
            begin
                return x;
            end

            var x := f(1);
        end"#;


    let db = db_from_str(input);
    dbg!(&db);
    let sym = sym_table_from(input, &db);
    dbg!(&sym);
    let resolver = resolver(input, &db, &sym);

    dbg!(&resolver);
    let types = TypingsBuilder::new(&resolver, &sym, &db).build(Program::new(db.start_id()));

    dbg!(&types);
}
