//! Utilities for figuring out symbols and scopes

use super::node_db::{NodeDb, NodeId};
use super::syntax::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
struct IdentId(usize);

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
struct ScopeId(usize);

#[derive(Debug, Default)]
struct SymbolTable<'a> {
    scopes: HashMap<ScopeId, Scope<'a>>,
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    parent: ScopeId,
    variables: PartialSymbolTable<'a>,
}

#[derive(Debug, Default, Clone)]
struct PartialSymbolTable<'a> {
    vars: Vec<VariableRecord<'a>>,
}

#[derive(Debug, Clone)]
struct VariableRecord<'a> {
    id: IdentId,
    text: &'a str,
    decl: NodeId,
}

#[derive(Debug)]
struct SymbolTableBuilder<'a, 'b> {
    current_id: IdentId,
    current_scope: ScopeId,
    symb: SymbolTable<'a>,
    input: &'a str,
    db: &'b NodeDb<'a>,
}

impl<'a, 'b> SymbolTableBuilder<'a, 'b> {
    fn new(input: &'a str, db: &'b NodeDb<'a>) -> Self {
        SymbolTableBuilder {
            current_id: IdentId(0),
            current_scope: ScopeId(0),
            symb: SymbolTable::default(),
            input,
            db,
        }
    }

    fn build(mut self, program: Program) -> SymbolTable<'a> {
        self.compound(program.compound(self.db));

        self.symb
    }

    fn enter_new_scope(&mut self) {
        let temp = self.current_scope;
        self.current_scope = ScopeId(temp.0 + 1);
        let new_scope = Scope {
            parent: temp,
            variables: PartialSymbolTable::default(),
        };

        self.symb.scopes.insert(self.current_scope, new_scope);
    }

    fn exit_scope(&mut self) {
        let current_sco = self
            .symb
            .scopes
            .get(&self.current_scope)
            .expect("Invalid scope");
        self.current_scope = current_sco.parent;
    }

    fn add_var(&mut self, id: Identifier, node_id: NodeId) {
        let id_str = id.id(self.db, self.input);
        let mut current_scope = self
            .symb
            .scopes
            .get(&self.current_scope)
            .cloned()
            .expect("Invalid scope");
        current_scope.variables.vars.push(VariableRecord {
            id: self.current_id,
            decl: node_id,
            text: id_str,
        });
        self.current_id = IdentId(self.current_id.0 + 1);
        self.symb.scopes.insert(self.current_scope, current_scope);
    }

    fn compound(&mut self, compound: Compound) {
        self.enter_new_scope();
        for statement in compound.statements(self.db) {
            match statement.downcast(self.db) {
                StatementType::Decl(d) => {
                    self.add_var(d.id(self.db), d.get_id());
                }
                StatementType::FunctionDecl(f) => {
                    self.enter_new_scope();
                    for arg in f.args(self.db) {
                        self.add_var(arg.id(self.db), arg.get_id())
                    }
                    self.compound(f.compound(self.db));
                    self.exit_scope();
                }
                StatementType::If(i) => {
                    self.compound(i.if_branch(self.db));
                    if let Some(c) = i.else_branch(self.db) {
                        self.compound(c);
                    }
                }
                StatementType::While(w) => {
                    self.compound(w.compound(self.db));
                }
                _ => continue,
            }
        }
        self.exit_scope();
    }
}

#[cfg(test)]
mod tests {
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

    #[test]
    fn test_simple() {
        let input = "program x begin var x := 10; end";
        let db = db_from_str(input);
        let table = SymbolTableBuilder::new(input, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(table);
    }
}
