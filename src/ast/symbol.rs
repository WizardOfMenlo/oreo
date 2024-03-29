//! Utilities for figuring out symbols and scopes

use super::node_db::NodeDb;
use super::syntax::*;
use std::collections::HashMap;

/// Id for the global scope
pub const GLOBAL_SCOPE: ScopeId = ScopeId(0);

/// An id for an identifier
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct IdentId(pub usize);

/// A id for a scope
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct ScopeId(usize);

impl From<usize> for ScopeId {
    fn from(i: usize) -> Self {
        ScopeId(i)
    }
}

/// A full symbol table
#[derive(Debug, Default)]
pub struct SymbolTable<'a> {
    scopes: HashMap<ScopeId, Scope<'a>>,
}

impl<'a> SymbolTable<'a> {
    /// Get the parent of the scope (closest enclosing to furthest)
    pub fn parent_scopes(&self, mut s: ScopeId) -> Vec<ScopeId> {
        let mut v = vec![s];
        while let Some(scope_id) = self.scopes.get(&s).expect("Invalid scope id").parent {
            v.push(scope_id);
            s = scope_id;
        }

        v
    }

    /// Get the kind of scope we have
    pub fn scope_ty(&self, s: ScopeId) -> ScopeType {
        self.scopes.get(&s).expect("Invalid scope id").ty
    }

    /// Get the identifier for an id in scope if any (with a filter on funcs)
    pub fn get_id_scope(
        &self,
        identifier: &'a str,
        scope: ScopeId,
        limit_to_func: bool,
    ) -> Option<IdentId> {
        self.scopes
            .get(&scope)
            .and_then(|s| {
                s.variables
                    .vars
                    .iter()
                    .filter(|s| {
                        if limit_to_func {
                            if let DeclarationContext::FunctionDecl(_) = s.decl {
                                true
                            } else {
                                false
                            }
                        } else {
                            true
                        }
                    })
                    .find(|s| s.text == identifier)
            })
            .map(|r| r.id)
    }

    /// Get information about the declaration of a id
    pub fn get_variable_record(&self, id: IdentId) -> Option<VariableRecord> {
        self.scopes
            .iter()
            .map(|(_, s)| s.variables.vars.iter())
            .flatten()
            .find(|r| r.id == id)
            .cloned()
    }
}

/// The type this scope can be
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ScopeType {
    /// A function scope (i.e. does not refer to prev stuff)
    FunctionScope,

    /// A normal scope (usual nesting)
    NormalScope,
}

/// A scope
#[derive(Debug, Clone)]
pub struct Scope<'a> {
    parent: Option<ScopeId>,
    ty: ScopeType,
    variables: PartialSymbolTable<'a>,
}

/// A symbol table for a scope
#[derive(Debug, Default, Clone)]
pub struct PartialSymbolTable<'a> {
    // TODO: As always, should hashmap this
    vars: Vec<VariableRecord<'a>>,
}

/// A possible declaration
#[derive(Debug, Clone)]
pub enum DeclarationContext {
    /// var x := 0
    Decl(Decl),
    /// procedure f(var x) ...
    FunctionArg(FunctionDeclArgs),

    /// Defining a function
    FunctionDecl(FunctionDecl),
}

/// A record for a variable (i.e. declaration and name)
#[derive(Debug, Clone)]
pub struct VariableRecord<'a> {
    id: IdentId,
    text: &'a str,
    decl: DeclarationContext,
}

impl<'a> VariableRecord<'a> {
    /// Get the id for this record
    pub fn id(&self) -> IdentId {
        self.id
    }

    /// Get the context for this variable decl
    pub fn decl(&self) -> &DeclarationContext {
        &self.decl
    }
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
        let mut symb = SymbolTable::default();
        symb.scopes.insert(
            GLOBAL_SCOPE,
            Scope {
                parent: None,
                ty: ScopeType::NormalScope,
                variables: PartialSymbolTable::default(),
            },
        );
        SymbolTableBuilder {
            current_id: IdentId(0),
            current_scope: GLOBAL_SCOPE,
            scope_count: 0,
            symb,
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

    fn enter_new_scope(&mut self, ty: ScopeType) {
        let new_scope = Scope {
            parent: Some(self.current_scope),
            ty,
            variables: PartialSymbolTable::default(),
        };

        self.scope_count += 1;
        self.current_scope = ScopeId(self.scope_count);
        self.symb.scopes.insert(self.current_scope, new_scope);
    }

    fn exit_scope(&mut self) {
        self.current_scope = self
            .get_current_scope()
            .parent
            .expect("Tried to exit global scope");
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
        self.enter_new_scope(ScopeType::NormalScope);
        for statement in compound.statements(self.db) {
            match statement.downcast(self.db) {
                StatementType::Decl(d) => {
                    self.add_var(d.id(self.db), DeclarationContext::Decl(d));
                }
                StatementType::FunctionDecl(f) => {
                    // Note the id is declared in the parent scope
                    self.add_var(f.id(self.db), DeclarationContext::FunctionDecl(f));
                    // We introduce a new scope so the vars don't leak outside
                    self.enter_new_scope(ScopeType::FunctionScope);
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
    fn symbol_table_simple() {
        let input = "program x begin var x := 10; end";
        let db = db_from_str(input);
        let table = SymbolTableBuilder::new(input, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(table));
    }

    #[test]
    fn symbol_table_nested() {
        let input = r#"program id begin
                   if ( true ) then begin var n := 5;  end
       			   else begin var k := 46; end; end"#;
        let db = db_from_str(input);
        let table = SymbolTableBuilder::new(input, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(table));
    }

    #[test]
    fn symbol_table_decl() {
        let input = r#"
        program id begin
            procedure void f(var x ~ int, var y ~ bool) begin
                var z;
                var k;
            end
       	end"#;
        let db = db_from_str(input);
        let table = SymbolTableBuilder::new(input, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(table));
    }

    #[test]
    fn symbol_table_recursive() {
        let input = r#"
        program id begin
            procedure int f() begin
                return f();
            end
       	end"#;
        let db = db_from_str(input);
        let table = SymbolTableBuilder::new(input, &db).build(Program::new(db.start_id()));
        assert_debug_snapshot!(determinize(table));
    }
}
