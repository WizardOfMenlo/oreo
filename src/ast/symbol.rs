//! Utilities for figuring out symbols and scopes

use super::syntax::*;
use super::untyped::Node;
use super::node_db::NodeId;
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
struct SymbolTableBuilder<'a> {
    current_id: IdentId,
    current_scope: ScopeId,
    symb: SymbolTable<'a>,
    input: &'a str
}

impl<'a, 'b> SymbolTableBuilder<'a, 'b> {
    fn new(input: &'a str) -> Self {
        SymbolTableBuilder {
            current_id: IdentId(0),
            current_scope: ScopeId(0),
            symb: SymbolTable::default(),
            input
        }
    }

    fn build(mut self, program: &'a Program<'a, 'b>) -> SymbolTable<'a, 'b> {
        self.compound(program.compound());

        self.symb
    }

    fn enter_new_scope(&mut self) {
        let temp = self.current_scope;
        self.current_scope = ScopeId(temp.0 + 1);
        let new_scope = Scope {
            parent: temp,
            variables: PartialSymbolTable::default()
        };

        self.symb.scopes.insert(self.current_scope, new_scope);
    }

    fn exit_scope(&mut self) {
        let current_sco = self.symb.scopes.get(&self.current_scope).expect("Invalid scope");
        self.current_scope = current_sco.parent;
    }

    fn add_var(&mut self, id: Identifier<'a, 'b>, node: &'b Node<'a>) {
        let id_str = id.id(self.input);
        let mut current_scope = self.symb.scopes.get(&self.current_scope).cloned().expect("Invalid scope");
        current_scope.variables.vars.push(VariableRecord {
            id: self.current_id,
            decl: node,
            text: id_str,
        });
        self.current_id = IdentId(self.current_id.0 + 1);
        self.symb.scopes.insert(self.current_scope, current_scope);
    }

    fn compound(&mut self, compound: Compound<'a, 'b>) 
    {
        self.enter_new_scope();
        for statement in compound.statements() {
            match statement.downcast().clone() {
                StatementType::Decl(d) => {
                    self.add_var(d.id(), d.get_node());
                }
                StatementType::FunctionDecl(f) => {
                    for arg in f.args() {
                        self.add_var(arg.id(), arg.get_node())
                    }
                }
                _ => continue
            }
        }
        self.exit_scope();
    }
}
