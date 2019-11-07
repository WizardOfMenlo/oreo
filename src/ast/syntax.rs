//! Typed syntax that is backed by Node

use super::node_db::{NodeDb, NodeId};
use super::untyped::{Node, NodeType};

/// Macro for simple definition of a syntax node
macro_rules! syntax_node {
    ($id:ident) => {
        /// A syntax node for an $id
        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
        pub struct $id(NodeId);

        impl $id {
            /// Instantiate the syntax node
            pub fn new(node: NodeId) -> Self {
                $id(node)
            }

            /// Get node id
            pub fn get_id(self) -> NodeId {
                self.0
            }

            /// Get the raw node
            pub fn get_node<'a, 'b>(self, db: &'b NodeDb<'a>) -> &'b Node<'a> {
                db.get_node(self.0).expect("Invalid syntax node")
            }

            /// Get children
            pub fn children<'a, 'b>(self, db: &'b NodeDb<'a>) -> &'b [NodeId] {
                db.get_flat_node(self.0)
                    .expect("Invalid syntax node")
                    .children()
            }
        }
    };
}

syntax_node!(Program);

impl Program {
    /// Get the program name
    pub fn id<'a>(self, db: &NodeDb<'a>) -> Identifier {
        Identifier(self.children(db)[0])
    }

    /// Program main body
    pub fn compound<'a>(self, db: &NodeDb<'a>) -> Compound {
        Compound(self.children(db)[1])
    }
}

syntax_node!(Compound);

impl Compound {
    /// Get the inner statements
    pub fn statements<'a>(self, db: &NodeDb<'a>) -> Vec<Statement> {
        self.children(db).iter().map(|n| Statement(*n)).collect()
    }
}

syntax_node!(Statement);

impl Statement {
    /// Convert the statement to a more specific one
    pub fn downcast<'a>(self, db: &NodeDb<'a>) -> StatementType {
        let n = self.0;
        match self.get_node(db).ty() {
            NodeType::Decl => StatementType::Decl(Decl(n)),
            NodeType::PrintStat => StatementType::PrintStat(PrintStat(n)),
            NodeType::While => StatementType::While(While(n)),
            NodeType::If => StatementType::If(If(n)),
            NodeType::Assign => StatementType::Assign(Assign(n)),
            NodeType::FunctionCall => StatementType::FunctionCall(FunctionCall(n)),
            NodeType::FunctionDecl => StatementType::FunctionDecl(FunctionDecl(n)),
            NodeType::Return => StatementType::Return(Return(n)),
            _ => panic!("Invalid node constructed"),
        }
    }
}

#[allow(missing_docs)]
#[derive(Debug, Clone)]
/// The possible types that a statement can be
pub enum StatementType {
    Decl(Decl),
    PrintStat(PrintStat),
    While(While),
    If(If),
    Assign(Assign),
    FunctionCall(FunctionCall),
    FunctionDecl(FunctionDecl),
    Return(Return),
}

syntax_node!(Decl);

impl Decl {
    /// The identifier being declared
    pub fn id<'a>(self, db: &NodeDb<'a>) -> Identifier {
        Identifier(self.children(db)[0])
    }

    /// The expression to assign (if any)
    pub fn expr<'a>(self, db: &NodeDb<'a>) -> Option<Expr> {
        self.children(db).get(1).map(|n| Expr(*n))
    }
}

syntax_node!(PrintStat);

impl PrintStat {
    /// Convert to the more specific print typ
    pub fn downcast<'a>(self, db: &NodeDb<'a>) -> PrintTypes {
        let node = self.children(db)[0];
        match self.get_node(db).ty() {
            NodeType::Print => PrintTypes::Print(Print(node)),
            NodeType::Println => PrintTypes::Println(Println(node)),
            NodeType::Get => PrintTypes::Get(Get(node)),
            _ => panic!("Invalid node constructed"),
        }
    }
}

/// Possible print types
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum PrintTypes {
    Print(Print),
    Println(Println),
    Get(Get),
}

syntax_node!(Print);

impl Print {
    /// The expression to print
    pub fn expr<'a>(self, db: &NodeDb<'a>) -> Expr {
        Expr(self.children(db)[0])
    }
}

syntax_node!(Println);

impl Println {
    /// The expression to print
    pub fn expr<'a>(self, db: &NodeDb<'a>) -> Expr {
        Expr(self.children(db)[0])
    }
}

syntax_node!(Get);

impl Get {
    /// The identifier to set
    pub fn id<'a>(self, db: &NodeDb<'a>) -> Identifier {
        Identifier(self.children(db)[0])
    }
}

syntax_node!(While);

impl While {
    /// The condition to evaluate
    pub fn condition<'a>(self, db: &NodeDb<'a>) -> Expr {
        Expr(self.children(db)[0])
    }

    /// The body to be executed
    pub fn compound<'a>(self, db: &NodeDb<'a>) -> Compound {
        Compound(self.children(db)[1])
    }
}

syntax_node!(If);

impl If {
    /// The condition to evaluate
    pub fn condition<'a>(self, db: &NodeDb<'a>) -> Expr {
        Expr(self.children(db)[0])
    }

    /// If true
    pub fn if_branch<'a>(self, db: &NodeDb<'a>) -> Compound {
        Compound(self.children(db)[1])
    }

    /// If false (optional)
    pub fn else_branch<'a>(self, db: &NodeDb<'a>) -> Option<Compound> {
        self.children(db).get(2).map(|n| Compound(*n))
    }
}

syntax_node!(Assign);

impl Assign {
    /// The identifier to set
    pub fn id<'a>(self, db: &NodeDb<'a>) -> Identifier {
        Identifier(self.children(db)[0])
    }

    /// The expression to set it to
    pub fn expr<'a>(self, db: &NodeDb<'a>) -> Expr {
        Expr(self.children(db)[1])
    }
}

syntax_node!(FunctionDecl);

impl FunctionDecl {
    /// Function to declare
    pub fn id<'a>(self, db: &NodeDb<'a>) -> Identifier {
        Identifier(self.children(db)[0])
    }

    /// The arguments
    pub fn args<'a>(self, db: &NodeDb<'a>) -> Vec<FunctionDeclArgs> {
        let children = self.children(db);
        children[1..children.len() - 1]
            .iter()
            .map(|n| FunctionDeclArgs(*n))
            .collect()
    }

    /// The body to be executed
    pub fn compound<'a>(self, db: &NodeDb<'a>) -> Compound {
        let children = self.children(db);
        Compound(children[children.len() - 1])
    }
}

syntax_node!(FunctionDeclArgs);

impl FunctionDeclArgs {
    /// The arg id
    pub fn id<'a>(self, db: &NodeDb<'a>) -> Identifier {
        Identifier(self.children(db)[0])
    }
}

syntax_node!(FunctionCallArgs);

impl FunctionCallArgs {
    /// The expression in the argument
    pub fn expr<'a>(self, db: &NodeDb<'a>) -> Expr {
        Expr(self.children(db)[0])
    }
}

syntax_node!(Return);

impl Return {
    /// The expression to return
    pub fn expr<'a>(self, db: &NodeDb<'a>) -> Expr {
        Expr(self.children(db)[0])
    }
}

syntax_node!(FunctionCall);

impl FunctionCall {
    /// The function id to call
    pub fn id<'a>(self, db: &NodeDb<'a>) -> Identifier {
        Identifier(self.children(db)[0])
    }

    /// The arguments of the call
    pub fn args<'a>(self, db: &NodeDb<'a>) -> Vec<FunctionCallArgs> {
        self.children(db)[1..]
            .iter()
            .map(|n| FunctionCallArgs(*n))
            .collect()
    }
}

syntax_node!(Expr);

impl Expr {
    /// The head of the expression
    pub fn head<'a>(self, db: &NodeDb<'a>) -> Term {
        Term(self.children(db)[0])
    }

    /// Tail of the expression
    pub fn tail<'a>(self, db: &NodeDb<'a>) -> Option<ExprPrime> {
        self.children(db).get(1).map(|e| ExprPrime(*e))
    }
}

syntax_node!(ExprPrime);

impl ExprPrime {
    /// The operation
    pub fn op<'a>(self, db: &NodeDb<'a>) -> BooleanOp {
        self.get_node(db).ty().clone().into()
    }

    /// The right operand
    pub fn rhs<'a>(self, db: &NodeDb<'a>) -> Term {
        Term(self.children(db)[0])
    }

    /// Possible tails
    pub fn tail<'a>(self, db: &NodeDb<'a>) -> Option<ExprPrime> {
        self.children(db).get(1).map(|e| ExprPrime(*e))
    }
}

syntax_node!(Term);

impl Term {
    /// Head of the term
    pub fn head<'a>(self, db: &NodeDb<'a>) -> Factor {
        Factor(self.children(db)[0])
    }

    /// Tail of the term
    pub fn tail<'a>(self, db: &NodeDb<'a>) -> Option<TermPrime> {
        self.children(db).get(1).map(|n| TermPrime(*n))
    }
}

syntax_node!(TermPrime);

impl TermPrime {
    /// The operation
    pub fn op<'a>(self, db: &NodeDb<'a>) -> RelationalOp {
        self.get_node(db).ty().clone().into()
    }

    /// Rhs
    pub fn rhs<'a>(self, db: &NodeDb<'a>) -> Factor {
        Factor(self.children(db)[0])
    }

    /// Optional tail
    pub fn tail<'a>(self, db: &NodeDb<'a>) -> Option<TermPrime> {
        self.children(db).get(1).map(|n| TermPrime(*n))
    }
}

syntax_node!(Factor);

impl Factor {
    /// The head of a factor
    pub fn head<'a>(self, db: &NodeDb<'a>) -> Product {
        Product(self.children(db)[0])
    }

    /// Optional tail
    pub fn tail<'a>(self, db: &NodeDb<'a>) -> Option<FactorPrime> {
        self.children(db).get(1).map(|n| FactorPrime(*n))
    }
}

syntax_node!(FactorPrime);

impl FactorPrime {
    /// The operation in the tail
    pub fn op<'a>(self, db: &NodeDb<'a>) -> AdditiveOp {
        self.get_node(db).ty().clone().into()
    }

    /// The rhs of the tail
    pub fn rhs<'a>(self, db: &NodeDb<'a>) -> Product {
        Product(self.children(db)[0])
    }

    /// Optional tail
    pub fn tail<'a>(self, db: &NodeDb<'a>) -> Option<FactorPrime> {
        self.children(db).get(1).map(|n| FactorPrime(*n))
    }
}

syntax_node!(Product);

impl Product {
    /// The head
    pub fn head<'a>(self, db: &NodeDb<'a>) -> Atom {
        Atom(self.children(db)[0])
    }

    /// Opt tail
    pub fn tail<'a>(self, db: &NodeDb<'a>) -> Option<ProductPrime> {
        self.children(db).get(1).map(|n| ProductPrime(*n))
    }
}

syntax_node!(ProductPrime);

impl ProductPrime {
    /// The operation
    pub fn op<'a>(self, db: &NodeDb<'a>) -> MultiplicativeOp {
        self.get_node(db).ty().clone().into()
    }

    /// The rhs of the product
    pub fn rhs<'a>(self, db: &NodeDb<'a>) -> Atom {
        Atom(self.children(db)[0])
    }

    /// Optional tail
    pub fn tail<'a>(self, db: &NodeDb<'a>) -> Option<ProductPrime> {
        self.children(db).get(1).map(|n| ProductPrime(*n))
    }
}

syntax_node!(Atom);

/// Possible types of an atom
pub enum AtomType {
    /// Literal, function call or bracketed expr
    Unit(Unit),

    /// The not of an atom
    Not(Atom),
}

impl Atom {
    /// Convert an atom to a more specific type
    pub fn downcast<'a>(self, db: &NodeDb<'a>) -> AtomType {
        let n = self.children(db)[0];
        match self.get_node(db).ty() {
            NodeType::Not => AtomType::Not(Atom(n)),
            NodeType::Unit => AtomType::Unit(Unit(n)),
            _ => panic!("Invalid atom type"),
        }
    }
}

syntax_node!(Unit);

impl Unit {
    /// Convert a unit into a more specific type
    pub fn downcast<'a>(self, db: &NodeDb<'a>) -> UnitType {
        match self.get_node(db).ty() {
            NodeType::Int(i) => UnitType::Int(*i),
            NodeType::Bool(b) => UnitType::Bool(*b),
            NodeType::Str => UnitType::Str(self.0),
            NodeType::Identifier => UnitType::Identifier(Identifier(self.children(db)[0])),
            NodeType::FunctionCall => UnitType::FunctionCall(FunctionCall(self.children(db)[0])),
            NodeType::BracketedExpr => UnitType::BracketedExpr(Expr(self.children(db)[0])),
            _ => panic!("Invalid atom type"),
        }
    }
}

/// Possible unit types
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum UnitType {
    Int(isize),
    Str(NodeId),
    Bool(bool),
    Identifier(Identifier),
    FunctionCall(FunctionCall),
    BracketedExpr(Expr),
}

syntax_node!(Identifier);

impl Identifier {
    /// Get the string id
    pub fn id<'a>(self, db: &NodeDb<'a>, text: &'a str) -> &'a str {
        use crate::range::Ranged;
        &text[self.get_node(db).range().clone()]
    }
}

#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum AdditiveOp {
    Plus,
    Minus,
}

#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum MultiplicativeOp {
    Times,
    Divide,
}

#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum RelationalOp {
    LesserThan,
    GreaterThan,
    Equals,
    GreaterOrEquals,
    LesserOrEquals,
}

#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum BooleanOp {
    And,
    Or,
}

/// Enum to convert any node into a typed one
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum SyntaxNode {
    Program(Program),
    Compound(Compound),
    Decl(Decl),
    PrintStat(PrintStat),
    Print(Print),
    Println(Println),
    Get(Get),
    While(While),
    If(If),
    Assign(Assign),
    FunctionDecl(FunctionDecl),
    FunctionDeclArgs(FunctionDeclArgs),
    FunctionCall(FunctionCall),
    FunctionCallArgs(FunctionCallArgs),
    Return(Return),
    Expr(Expr),
    ExprPrime(ExprPrime),
    Term(Term),
    TermPrime(TermPrime),
    Factor(Factor),
    FactorPrime(FactorPrime),
    Product(Product),
    ProductPrime(ProductPrime),
    Atom(Atom),
    Unit(Unit),
    Identifier(Identifier),
}

impl SyntaxNode {
    /// Convert a id to a syntax node
    pub fn from<'a>(input: NodeId, db: &NodeDb<'a>) -> Self {
        match db.get_node(input).expect("Invalid id").ty() {
            NodeType::Program => SyntaxNode::Program(Program(input)),
            NodeType::Compound => SyntaxNode::Compound(Compound(input)),
            NodeType::Decl => SyntaxNode::Decl(Decl(input)),
            NodeType::PrintStat => SyntaxNode::PrintStat(PrintStat(input)),

            NodeType::While => SyntaxNode::While(While(input)),
            NodeType::If => SyntaxNode::If(If(input)),
            NodeType::Assign => SyntaxNode::Assign(Assign(input)),
            NodeType::FunctionCall => SyntaxNode::FunctionCall(FunctionCall(input)),
            NodeType::FunctionDecl => SyntaxNode::FunctionDecl(FunctionDecl(input)),
            NodeType::Return => SyntaxNode::Return(Return(input)),
            NodeType::Print => SyntaxNode::Print(Print(input)),
            NodeType::Println => SyntaxNode::Println(Println(input)),
            NodeType::Get => SyntaxNode::Get(Get(input)),
            NodeType::FunctionDeclArgs => SyntaxNode::FunctionDeclArgs(FunctionDeclArgs(input)),
            NodeType::FunctionCallArgs => SyntaxNode::FunctionCallArgs(FunctionCallArgs(input)),

            NodeType::Expr => SyntaxNode::Expr(Expr(input)),
            NodeType::And | NodeType::Or => SyntaxNode::ExprPrime(ExprPrime(input)),

            NodeType::Term => SyntaxNode::Term(Term(input)),
            NodeType::LesserThan
            | NodeType::GreaterThan
            | NodeType::Equals
            | NodeType::GreaterOrEquals
            | NodeType::LesserOrEquals => SyntaxNode::TermPrime(TermPrime(input)),

            NodeType::Factor => SyntaxNode::Factor(Factor(input)),
            NodeType::Plus | NodeType::Minus => SyntaxNode::FactorPrime(FactorPrime(input)),

            NodeType::Product => SyntaxNode::Product(Product(input)),
            NodeType::Times | NodeType::Divide => SyntaxNode::ProductPrime(ProductPrime(input)),

            NodeType::Unit | NodeType::Not => SyntaxNode::Atom(Atom(input)),

            NodeType::Int(_) | NodeType::Str | NodeType::Bool(_) | NodeType::BracketedExpr => {
                SyntaxNode::Unit(Unit(input))
            }

            NodeType::Identifier => SyntaxNode::Identifier(Identifier(input)),

            NodeType::Error(_) => panic!("Called on error node"),
        }
    }
}
