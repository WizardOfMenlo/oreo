//! Facilities for converting the untyped node to an
//! AST

pub mod node_db;
pub mod scope_resolution;
pub mod symbol;
pub mod syntax;
pub mod untyped;

/// The AST

pub struct AST<'a> {
    program: syntax::Program,
    db: node_db::NodeDbWrap<'a>,
    symbols: symbol::SymbolTable<'a>,
    variables: scope_resolution::VariableResolver,
}

impl<'a> AST<'a> {
    /// Creates an AST from a node. Unfallible, panic if node is already err or if resolution fails
    pub fn new(node: untyped::Node<'a>, input: &'a str) -> Self {
        let db = node_db::NodeDb::new(node);
        let program = syntax::Program::new(db.start_id());
        let symbols = symbol::SymbolTableBuilder::new(input, &db).build(program);
        let variables = scope_resolution::VariableResolverBuilder::new(input, &symbols, &db)
            .build(program)
            .expect("Resolution error");

        AST {
            db,
            program,
            symbols,
            variables,
        }
    }

    /// Get the node db
    pub fn db(&self) -> &node_db::NodeDb {
        &self.db
    }

    /// Get the top level node
    pub fn program(&self) -> syntax::Program {
        self.program
    }

    /// Get all the symbols
    pub fn symbols(&self) -> &symbol::SymbolTable {
        &self.symbols
    }

    /// Get the variables resolved
    pub fn variables(&self) -> &scope_resolution::VariableResolver {
        &self.variables
    }
}
