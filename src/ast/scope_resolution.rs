//! Module that does variable resolution

use super::node_db::NodeDb;
use super::symbol::{IdentId, ScopeId, SymbolTable, GLOBAL_SCOPE};
use super::syntax::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VariableResolver {
    id_mapping: HashMap<Identifier, IdentId>,
}

#[derive(Debug)]
pub struct VariableResolverBuilder<'a, 'b, 'c> {
    current_scope: ScopeId,
    parents: Vec<ScopeId>,
    scope_counter: usize,
    input: &'a str,
    symbols: &'b SymbolTable<'a>,
    db: &'c NodeDb<'a>,
}

impl<'a, 'b, 'c> VariableResolverBuilder<'a, 'b, 'c> {
    pub fn new(input: &'a str, symbols: &'b SymbolTable<'a>, node_db: &'c NodeDb<'a>) -> Self {
        VariableResolverBuilder {
            current_scope: GLOBAL_SCOPE,
            input,
            symbols,
            scope_counter: 0,
            parents: Vec::new(),
            db: node_db,
        }
    }

    pub fn build(mut self, program: Program) -> VariableResolver {
        let mut res = HashMap::new();

        self.compound(program.compound(self.db), &mut res);

        VariableResolver { id_mapping: res }
    }

    fn enter_scope(&mut self) {
        self.parents.push(self.current_scope);
        self.scope_counter += 1;
        self.current_scope = self.scope_counter.into();
    }

    fn exit_scope(&mut self) {
        self.current_scope = self.parents.pop().expect("Tried to exit global scope");
    }

    // TODO: Error handle
    fn resolve_id(&mut self, id: Identifier, map: &mut HashMap<Identifier, IdentId>) {
        let parents = self.symbols.parent_scopes(self.current_scope);
        let id_str = id.id(self.db, self.input);

        dbg!(id_str);
        dbg!(&parents);

        let mut found = false;
        for scope in parents {
            if let Some(id_id) = self.symbols.get_id_scope(id_str, scope) {
                found = true;
                map.insert(id, id_id);
                break;
            }
        }

        if !found {
            panic!("Variable resolution failed")
        }
    }

    fn expr(&mut self, expr: Expr, map: &mut HashMap<Identifier, IdentId>) {
        use super::untyped::NodeType;
        for children in self.db.all_children(expr.get_id()) {
            if let NodeType::Identifier = self.db.get_node(children).expect("Invalid node id").ty()
            {
                self.resolve_id(Identifier::new(children), map);
            }
        }
    }

    fn compound(&mut self, compound: Compound, map: &mut HashMap<Identifier, IdentId>) {
        self.enter_scope();

        for statement in compound.statements(self.db) {
            match statement.downcast(self.db) {
                StatementType::Decl(d) => {
                    self.resolve_id(d.id(self.db), map);
                    if let Some(e) = d.expr(self.db) {
                        self.expr(e, map);
                    }
                }
                StatementType::Assign(a) => {
                    self.resolve_id(a.id(self.db), map);
                    self.expr(a.expr(self.db), map);
                }
                StatementType::FunctionCall(f) => {
                    self.resolve_id(f.id(self.db), map);
                    for arg in f.args(self.db) {
                        self.expr(arg.expr(self.db), map)
                    }
                }
                StatementType::FunctionDecl(f) => {
                    self.resolve_id(f.id(self.db), map);
                    self.enter_scope();
                    for arg in f.args(self.db) {
                        self.resolve_id(arg.id(self.db), map);
                    }
                    self.compound(f.compound(self.db), map);
                    self.exit_scope();
                }
                StatementType::If(i) => {
                    self.expr(i.condition(self.db), map);
                    self.compound(i.if_branch(self.db), map);
                    if let Some(c) = i.else_branch(self.db) {
                        self.compound(c, map);
                    }
                }
                StatementType::While(w) => {
                    self.expr(w.condition(self.db), map);
                    self.compound(w.compound(self.db), map);
                }
                StatementType::Return(r) => self.expr(r.expr(self.db), map),
                StatementType::PrintStat(p) => match p.downcast(self.db) {
                    PrintTypes::Get(g) => self.resolve_id(g.id(self.db), map),
                    PrintTypes::Print(p) => self.expr(p.expr(self.db), map),
                    PrintTypes::Println(p) => self.expr(p.expr(self.db), map),
                },
            }
        }

        self.exit_scope();
    }
}

#[cfg(test)]
mod tests {
    use super::super::symbol::{SymbolTable, SymbolTableBuilder, VariableRecord};
    use super::*;
    use insta::assert_debug_snapshot;
    use std::pin::Pin;

    fn db_from_str(input: &str) -> Pin<Box<NodeDb>> {
        use crate::lexer::lexicalize;
        use crate::lexer::scanner::scan;
        use crate::parser::parse;

        let node = parse(lexicalize(scan(input)));
        NodeDb::new(node)
    }

    fn sym_table_from<'a>(input: &'a str, db: &NodeDb<'a>) -> SymbolTable<'a> {
        SymbolTableBuilder::new(input, db).build(Program::new(db.start_id()))
    }

    use super::super::untyped::Node;

    // We use this to have a determined order
    fn determinize<'a>(
        resolver: VariableResolver,
        db: &NodeDb<'a>,
        sym: &'a SymbolTable<'a>,
    ) -> Vec<(Node<'a>, Identifier, VariableRecord<'a>)> {
        let mut res = Vec::new();
        for (id_n, ident_id) in resolver.id_mapping {
            let node = id_n.get_node(&db).clone();
            let record = sym.get_variable_record(ident_id).unwrap();
            res.push((node, id_n, record))
        }

        // We want to fix the order, else all will break randomly
        res.sort_by(|a, b| {
            a.2.id()
                .cmp(&b.2.id())
                .then(a.1.get_id().cmp(&b.1.get_id()))
        });

        res
    }

    #[test]
    fn scope_res_simple() {
        let input = "program x begin var x := 10; var y := x + 1; end";
        let db = db_from_str(input);
        let sym = sym_table_from(input, &db);
        let resolver =
            VariableResolverBuilder::new(input, &sym, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(resolver, &db, &sym));
    }

    #[test]
    fn scope_res_nested() {
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
        let db = db_from_str(input);
        let sym = sym_table_from(input, &db);
        let resolver =
            VariableResolverBuilder::new(input, &sym, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(resolver, &db, &sym));
    }

    #[test]
    #[should_panic]
    fn scope_res_nested_invalid() {
        let input = r#"
        program id begin
            if ( true ) then
            begin
                var n := 1;
            end
            else 
            begin 
                var k := n; 
            end;
        end"#;
        let db = db_from_str(input);
        let sym = sym_table_from(input, &db);
        let _resolver =
            VariableResolverBuilder::new(input, &sym, &db).build(Program::new(db.start_id()));
    }

    #[test]
    fn scope_res_test_fun_decl() {
        let input = r#"
        program id begin
            procedure f(var x, var y) begin
                var z;
                var k := z;
            end
       	end"#;
        let db = db_from_str(input);
        let sym = sym_table_from(input, &db);
        let resolver =
            VariableResolverBuilder::new(input, &sym, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(resolver, &db, &sym));
    }
}