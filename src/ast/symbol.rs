//! Utilities for figuring out symbols and scopes

use super::node_db::NodeDb;
use super::syntax::*;
use std::collections::HashMap;

/// An id for an identifier
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct IdentId(usize);

/// A id for a scope
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct ScopeId(usize);

/// A full symbol table
#[derive(Debug, Default)]
pub struct SymbolTable<'a> {
    scopes: HashMap<ScopeId, Scope<'a>>,
}

/// A scope
#[derive(Debug, Clone)]
pub struct Scope<'a> {
    parent: ScopeId,
    variables: PartialSymbolTable<'a>,
}

/// A symbol table for a scope
#[derive(Debug, Default, Clone)]
pub struct PartialSymbolTable<'a> {
    vars: Vec<VariableRecord<'a>>,
}

/// A possible declaration
#[derive(Debug, Clone)]
pub enum DeclarationContext {
    /// var x := 0
    Decl(Decl),
    /// procedure f(var f) ...
    FunctionArg(FunctionDeclArgs),
}

/// A record for a variable (i.e. declaration and name)
#[derive(Debug, Clone)]
pub struct VariableRecord<'a> {
    id: IdentId,
    text: &'a str,
    decl: DeclarationContext,
}

/// Build the symbol table from a node
#[derive(Debug)]
pub struct SymbolTableBuilder<'a, 'b> {
    current_id: IdentId,
    current_scope: ScopeId,
    // Used to generate new ids
    scope_count: usize,
    symb: SymbolTable<'a>,
    input: &'a str,
    db: &'b NodeDb<'a>,
}

impl<'a, 'b> SymbolTableBuilder<'a, 'b> {
    /// Starts building the symbol table
    pub fn new(input: &'a str, db: &'b NodeDb<'a>) -> Self {
        SymbolTableBuilder {
            current_id: IdentId(0),
            current_scope: ScopeId(0),
            scope_count: 0,
            symb: SymbolTable::default(),
            input,
            db,
        }
    }

    /// Build the full table
    pub fn build(mut self, program: Program) -> SymbolTable<'a> {
        self.compound(program.compound(self.db));

        self.symb
    }

    fn get_current_scope(&mut self) -> &mut Scope<'a> {
        self.symb
            .scopes
            .get_mut(&self.current_scope)
            .expect("Invalid scope")
    }

    fn enter_new_scope(&mut self) {
        dbg!(&self.symb);
        let new_scope = Scope {
            parent: self.current_scope,
            variables: PartialSymbolTable::default(),
        };

        self.scope_count += 1;
        self.current_scope = ScopeId(self.scope_count);
        self.symb.scopes.insert(self.current_scope, new_scope);

        dbg!(&self.symb);
    }

    fn exit_scope(&mut self) {
        self.current_scope = self.get_current_scope().parent;
    }

    fn add_var(&mut self, id: Identifier, decl: DeclarationContext) {
        let curr_id = self.current_id;
        let id_str = id.id(self.db, self.input);
        self.get_current_scope()
            .variables
            .vars
            .push(VariableRecord {
                id: curr_id,
                text: id_str,
                decl,
            });

        self.current_id = IdentId(self.current_id.0 + 1);
    }

    fn compound(&mut self, compound: Compound) {
        self.enter_new_scope();
        for statement in compound.statements(self.db) {
            match statement.downcast(self.db) {
                StatementType::Decl(d) => {
                    self.add_var(d.id(self.db), DeclarationContext::Decl(d));
                }
                StatementType::FunctionDecl(f) => {
                    self.enter_new_scope();
                    for arg in f.args(self.db) {
                        self.add_var(arg.id(self.db), DeclarationContext::FunctionArg(arg));
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

    // We use this to have a determined order
    fn determinize<'a>(table: SymbolTable<'a>) -> Vec<(ScopeId, Scope<'a>)> {
        let mut v: Vec<_> = table.scopes.into_iter().collect();
        v.sort_by(|a, b| (a.0).0.cmp(&(b.0).0));
        v
    }

    #[test]
    fn test_simple() {
        let input = "program x begin var x := 10; end";
        let db = db_from_str(input);
        let table = SymbolTableBuilder::new(input, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(table));
    }

    #[test]
    fn test_nested() {
        let input = r#"program id begin
                   if ( true ) then begin var n := 5;  end
       			   else begin var k := 46; end; end"#;
        let db = db_from_str(input);
        let table = SymbolTableBuilder::new(input, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(table));
    }

    #[test]
    fn test_fun_decl() {
        let input = r#"
        program id begin
            procedure f(var x, var y) begin
                var z;
                var k;
            end
       	end"#;
        let db = db_from_str(input);
        let table = SymbolTableBuilder::new(input, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(table));
    }
}
