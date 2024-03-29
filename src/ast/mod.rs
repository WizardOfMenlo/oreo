//! Facilities for converting the untyped node to an
//! AST

pub mod node_db;
pub mod scope_resolution;
pub mod symbol;
pub mod syntax;
pub mod types;
pub mod untyped;

pub use node_db::NodeId;
pub use symbol::IdentId;

use std::collections::HashMap;

/// The AST
#[derive(Debug)]
pub struct AST<'a> {
    program_name: &'a str,
    program: syntax::Program,
    db: node_db::NodeDbWrap<'a>,
    symbols: symbol::SymbolTable<'a>,
    variables: scope_resolution::VariableResolver,
    types: types::Typings,
    strings: HashMap<NodeId, &'a str>,
}

impl<'a> AST<'a> {
    /// Creates an AST from a node. Unfallible, panic if node is already err or if resolution fails
    pub fn new(node: untyped::Node<'a>, input: &'a str) -> Self {
        let db = node_db::NodeDb::new(node);
        let program = syntax::Program::new(db.start_id());
        let symbols = symbol::SymbolTableBuilder::new(input, &db).build(program);
        // Note that errors are returned here (not particularly well formatted)
        let variables = scope_resolution::VariableResolverBuilder::new(input, &symbols, &db)
            .build(program)
            .expect("Resolution error");

        // Note that errors are returned here (not particularly well formatted)
        let types = types::TypingsBuilder::new(&variables, &symbols, &db)
            .build(program)
            .expect("Typings errors");

        // Get all the strings in the source code
        let mut strings = HashMap::new();
        use untyped::NodeType;
        for id in db.all_children(db.start_id()) {
            let node = db.get_node(id).unwrap();
            match node.ty() {
                NodeType::Str => {
                    strings.insert(id, &input[node.text_range.clone()]);
                }
                _ => continue,
            }
        }

        AST {
            program_name: program.id(&db).id(&db, input),
            db,
            program,
            symbols,
            variables,
            types,
            strings,
        }
    }

    /// The name of the program
    pub fn program_name(&self) -> &'a str {
        self.program_name
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

    /// Get the types resolved
    pub fn types(&self) -> &types::Typings {
        &self.types
    }
}
