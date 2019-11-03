//! Typed syntax that is backed by Node

use super::untyped::{Node, NodeType};

/// The main program node
#[derive(Debug, Clone)]
pub struct Program<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Program<'a, 'b> {
    /// Get the program name
    pub fn id(&self) -> Identifier {
        Identifier(&self.0.children[0])
    }

    /// Program main body
    pub fn compound(&self) -> Compound {
        Compound(&self.0.children[1])
    }
}

/// Represent a collection of statements
#[derive(Debug, Clone)]
pub struct Compound<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Compound<'a, 'b> {
    /// Get the inner statements
    pub fn statements(&self) -> Vec<Statement> {
        self.0.children.iter().map(|n| Statement(n)).collect()
    }
}

/// A general statement
#[derive(Debug, Clone)]
pub struct Statement<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Statement<'a, 'b> {
    /// Convert the statement to a more specific one
    pub fn downcast(&self) -> StatementType<'a, 'b> {
        let n = self.0;
        match n.ty {
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
pub enum StatementType<'a, 'b> {
    Decl(Decl<'a, 'b>),
    PrintStat(PrintStat<'a, 'b>),
    While(While<'a, 'b>),
    If(If<'a, 'b>),
    Assign(Assign<'a, 'b>),
    FunctionCall(FunctionCall<'a, 'b>),
    FunctionDecl(FunctionDecl<'a, 'b>),
    Return(Return<'a, 'b>),
}

/// A declartion
#[derive(Debug, Clone)]
pub struct Decl<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Decl<'a, 'b> {
    /// The identifier being declared
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }

    /// The expression to assign (if any)
    pub fn expr(&self) -> Option<Expr<'a, 'b>> {
        self.0.children.get(1).map(|n| Expr(n))
    }
}

/// Either Print, Println, Get
#[derive(Debug, Clone)]
pub struct PrintStat<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> PrintStat<'a, 'b> {
    /// Convert to the more specific print typ
    pub fn downcast(&self) -> PrintTypes<'a, 'b> {
        let node = &self.0.children[0];
        match node.ty {
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
pub enum PrintTypes<'a, 'b> {
    Print(Print<'a, 'b>),
    Println(Println<'a, 'b>),
    Get(Get<'a, 'b>),
}

/// A print statement
#[derive(Debug, Clone)]
pub struct Print<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Print<'a, 'b> {
    /// The expression to print
    pub fn expr(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }
}

/// A println statement
#[derive(Debug, Clone)]
pub struct Println<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Println<'a, 'b> {
    /// The expression to print
    pub fn expr(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }
}

/// Get statement
#[derive(Debug, Clone)]
pub struct Get<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Get<'a, 'b> {
    /// The identifier to set
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }
}

/// A while statement
#[derive(Debug, Clone)]
pub struct While<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> While<'a, 'b> {
    /// The condition to evaluate
    pub fn condition(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }

    /// The body to be executed
    pub fn compound(&self) -> Compound<'a, 'b> {
        Compound(&self.0.children[1])
    }
}

/// A if statement
#[derive(Debug, Clone)]
pub struct If<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> If<'a, 'b> {
    /// The condition to evaluate
    pub fn condition(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }

    /// If true
    pub fn if_branch(&self) -> Compound<'a, 'b> {
        Compound(&self.0.children[1])
    }

    /// If false (optional)
    pub fn else_branch(&self) -> Option<Compound<'a, 'b>> {
        self.0.children.get(2).map(|n| Compound(n))
    }
}

/// An assign statement
#[derive(Debug, Clone)]
pub struct Assign<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Assign<'a, 'b> {
    /// The identifier to set
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }

    /// The expression to set it as
    pub fn expr(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[1])
    }
}

/// A function declaration statement
#[derive(Debug, Clone)]
pub struct FunctionDecl<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FunctionDecl<'a, 'b> {
    /// Function to declare
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }

    /// The arguments
    pub fn args(&self) -> Vec<FunctionDeclArgs<'a, 'b>> {
        self.0.children[1..self.0.children.len() - 1]
            .iter()
            .map(|n| FunctionDeclArgs(n))
            .collect()
    }

    /// The body to execute
    pub fn inner(&self) -> Compound<'a, 'b> {
        Compound(&self.0.children[self.0.children.len() - 1])
    }
}

/// The arguments of a declaration
#[derive(Debug, Clone)]
pub struct FunctionDeclArgs<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FunctionDeclArgs<'a, 'b> {
    /// The arg id
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }
}

/// The arguments of a call
#[derive(Debug, Clone)]
pub struct FunctionCallArgs<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FunctionCallArgs<'a, 'b> {
    /// The expression in the argument
    pub fn expr(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }
}

/// A return statement
#[derive(Debug, Clone)]
pub struct Return<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Return<'a, 'b> {
    /// The expression to return
    pub fn expr(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }
}

/// A function call (either in expr or statement)
#[derive(Debug, Clone)]
pub struct FunctionCall<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FunctionCall<'a, 'b> {
    /// The function id to call
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }

    /// The arguments of the call
    pub fn args(&self) -> Vec<FunctionCallArgs<'a, 'b>> {
        self.0.children[1..].iter().map(FunctionCallArgs).collect()
    }
}

/// An expression
#[derive(Debug, Clone)]
pub struct Expr<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Expr<'a, 'b> {
    /// The head of the expression
    pub fn head(&self) -> Term<'a, 'b> {
        Term(&self.0.children[0])
    }

    /// Tail of the expression
    pub fn tail(&self) -> Option<ExprPrime<'a, 'b>> {
        self.0.children.get(1).map(ExprPrime)
    }
}

/// The tail of an expression (consists of and/or)
#[derive(Debug, Clone)]
pub struct ExprPrime<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> ExprPrime<'a, 'b> {
    /// The operation
    pub fn op(&self) -> BooleanOp {
        self.0.ty.clone().into()
    }

    /// The right operand
    pub fn rhs(&self) -> Term<'a, 'b> {
        Term(&self.0.children[0])
    }

    /// Possible tails
    pub fn tail(&self) -> Option<ExprPrime<'a, 'b>> {
        self.0.children.get(1).map(ExprPrime)
    }
}

/// A term
#[derive(Debug, Clone)]
pub struct Term<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Term<'a, 'b> {
    /// Head of the term
    pub fn head(&self) -> Factor<'a, 'b> {
        Factor(&self.0.children[0])
    }

    /// Tail of the term
    pub fn tail(&self) -> Option<TermPrime<'a, 'b>> {
        self.0.children.get(1).map(TermPrime)
    }
}

/// The tail of a term (<, <=, >, >=, ==)
#[derive(Debug, Clone)]
pub struct TermPrime<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> TermPrime<'a, 'b> {
    /// The operation
    pub fn op(&self) -> RelationalOp {
        self.0.ty.clone().into()
    }

    /// Rhs
    pub fn rhs(&self) -> Factor<'a, 'b> {
        Factor(&self.0.children[0])
    }

    /// Optional tail
    pub fn tail(&self) -> Option<TermPrime<'a, 'b>> {
        self.0.children.get(1).map(TermPrime)
    }
}

/// A factor
#[derive(Debug, Clone)]
pub struct Factor<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Factor<'a, 'b> {
    /// The head of a factor
    pub fn head(&self) -> Product<'a, 'b> {
        Product(&self.0.children[0])
    }

    /// Optional tail
    pub fn tail(&self) -> Option<FactorPrime<'a, 'b>> {
        self.0.children.get(1).map(FactorPrime)
    }
}

/// Tail of a factor (+, -)
#[derive(Debug, Clone)]
pub struct FactorPrime<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FactorPrime<'a, 'b> {
    /// The operation in the tail
    pub fn op(&self) -> AdditiveOp {
        self.0.ty.clone().into()
    }

    /// The rhs of the tail
    pub fn rhs(&self) -> Product<'a, 'b> {
        Product(&self.0.children[0])
    }

    /// Optional tail
    pub fn tail(&self) -> Option<FactorPrime<'a, 'b>> {
        self.0.children.get(1).map(FactorPrime)
    }
}

/// A product
#[derive(Debug, Clone)]
pub struct Product<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Product<'a, 'b> {
    /// The head
    pub fn head(&self) -> Atom<'a, 'b> {
        Atom(&self.0.children[0])
    }

    /// Opt tail
    pub fn tail(&self) -> Option<ProductPrime<'a, 'b>> {
        self.0.children.get(1).map(ProductPrime)
    }
}

/// A product (*, /)
#[derive(Debug, Clone)]
pub struct ProductPrime<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> ProductPrime<'a, 'b> {
    /// The operation
    pub fn op(&self) -> MultiplicativeOp {
        self.0.ty.clone().into()
    }

    /// The rhs of the product
    pub fn rhs(&self) -> Atom<'a, 'b> {
        Atom(&self.0.children[0])
    }

    /// Optional tail
    pub fn tail(&self) -> Option<ProductPrime<'a, 'b>> {
        self.0.children.get(1).map(ProductPrime)
    }
}

/// An atom
#[derive(Debug, Clone)]
pub struct Atom<'a, 'b>(&'a Node<'b>);

/// Possible types of an atom
pub enum AtomType<'a, 'b> {
    /// Literal, function call or bracketed expr
    Unit(Unit<'a, 'b>),

    /// The not of an atom
    Not(Atom<'a, 'b>),
}

impl<'a, 'b> Atom<'a, 'b> {
    /// Convert an atom to a more specific type
    pub fn downcast(&self) -> AtomType<'a, 'b> {
        let n = &self.0.children[0];
        match self.0.ty {
            NodeType::Not => AtomType::Not(Atom(n)),
            NodeType::Unit => AtomType::Unit(Unit(n)),
            _ => panic!("Invalid atom type"),
        }
    }
}

/// A literal, Identifier, func call or bracketed expr
#[derive(Debug, Clone)]
pub struct Unit<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Unit<'a, 'b> {
    /// Convert a unit into a more specific type
    pub fn downcast(&self) -> UnitType<'a, 'b> {
        match self.0.ty {
            NodeType::Int(i) => UnitType::Int(i),
            NodeType::Bool(b) => UnitType::Bool(b),
            NodeType::Str => UnitType::Str(&self.0),
            NodeType::Identifier => UnitType::Identifier(Identifier(&self.0.children[0])),
            NodeType::FunctionCall => UnitType::FunctionCall(FunctionCall(&self.0.children[0])),
            NodeType::BracketedExpr => UnitType::BracketedExpr(Expr(&self.0.children[0])),
            _ => panic!("Invalid atom type"),
        }
    }
}

/// Possible unit types
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum UnitType<'a, 'b> {
    Int(isize),
    Str(&'a Node<'b>),
    Bool(bool),
    Identifier(Identifier<'a, 'b>),
    FunctionCall(FunctionCall<'a, 'b>),
    BracketedExpr(Expr<'a, 'b>),
}

/// Node representing an identifier
#[derive(Debug, Clone)]
pub struct Identifier<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Identifier<'a, 'b> {
    /// Get the string id
    pub fn identifier<'c>(&self, text: &'c str) -> &'c str {
        &text[self.0.text_range.clone()]
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
pub enum SyntaxNode<'a, 'b> {
    Program(Program<'a, 'b>),
    Compound(Compound<'a, 'b>),
    Statement(Statement<'a, 'b>),
    Decl(Decl<'a, 'b>),
    PrintStat(PrintStat<'a, 'b>),
    Print(Print<'a, 'b>),
    Println(Println<'a, 'b>),
    Get(Get<'a, 'b>),
    While(While<'a, 'b>),
    If(If<'a, 'b>),
    Assign(Assign<'a, 'b>),
    FunctionDecl(FunctionDecl<'a, 'b>),
    FunctionDeclArgs(FunctionDeclArgs<'a, 'b>),
    FunctionCall(FunctionCall<'a, 'b>),
    FunctionCallArgs(FunctionCallArgs<'a, 'b>),
    Return(Return<'a, 'b>),
    Expr(Expr<'a, 'b>),
    ExprPrime(ExprPrime<'a, 'b>),
    Term(Term<'a, 'b>),
    TermPrime(TermPrime<'a, 'b>),
    Factor(Factor<'a, 'b>),
    FactorPrime(FactorPrime<'a, 'b>),
    Product(Product<'a, 'b>),
    ProductPrime(ProductPrime<'a, 'b>),
    Atom(Atom<'a, 'b>),
    Unit(Unit<'a, 'b>),
    Identifier(Identifier<'a, 'b>),
}

impl<'a, 'b> From<&'b Node<'a>> for SyntaxNode<'a, 'b> {
    fn from(input: &'b Node<'a>) -> Self {
        match input.ty {
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
