use crate::untyped::Node;
use crate::untyped::NodeType;

pub struct Program<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Program<'a, 'b> {
    pub fn id(&self) -> Identifier {
        Identifier(&self.0.children[0])
    }

    pub fn compound(&self) -> Compound {
        Compound(&self.0.children[1])
    }
}

pub struct Compound<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Compound<'a, 'b> {
    pub fn statements(&self) -> Vec<Statement> {
        self.0.children.iter().map(|n| Statement(n)).collect()
    }
}

pub struct Statement<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Statement<'a, 'b> {
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

pub struct Decl<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Decl<'a, 'b> {
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }

    pub fn expr(&self) -> Option<Expr<'a, 'b>> {
        self.0.children.get(1).map(|n| Expr(n))
    }
}

pub struct PrintStat<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> PrintStat<'a, 'b> {
    pub fn downcast(&self) -> Print<'a, 'b> {
        let node = &self.0.children[0];
        match self.0.ty {
            NodeType::Print => Print::Print(Expr(node)),
            NodeType::Println => Print::Println(Expr(node)),
            NodeType::Get => Print::Get(Identifier(node)),
            _ => panic!("Invalid node constructed"),
        }
    }
}

pub enum Print<'a, 'b> {
    Print(Expr<'a, 'b>),
    Println(Expr<'a, 'b>),
    Get(Identifier<'a, 'b>),
}

pub struct While<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> While<'a, 'b> {
    pub fn condition(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }

    pub fn compound(&self) -> Compound<'a, 'b> {
        Compound(&self.0.children[1])
    }
}

pub struct If<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> If<'a, 'b> {
    pub fn condition(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }

    pub fn if_branch(&self) -> Compound<'a, 'b> {
        Compound(&self.0.children[1])
    }

    pub fn else_branch(&self) -> Option<Compound<'a, 'b>> {
        self.0.children.get(2).map(|n| Compound(n))
    }
}

pub struct Assign<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Assign<'a, 'b> {
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }

    pub fn expr(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[1])
    }
}

pub struct FunctionDecl<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FunctionDecl<'a, 'b> {
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }

    pub fn args(&self) -> Vec<FunctionDeclArgs<'a, 'b>> {
        self.0.children[1..self.0.children.len() - 1]
            .iter()
            .map(|n| FunctionDeclArgs(n))
            .collect()
    }

    pub fn inner(&self) -> Compound<'a, 'b> {
        Compound(&self.0.children[self.0.children.len() - 1])
    }
}

pub struct FunctionDeclArgs<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FunctionDeclArgs<'a, 'b> {
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }
}

pub struct FunctionCallArgs<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FunctionCallArgs<'a, 'b> {
    pub fn expr(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }
}

pub struct Return<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Return<'a, 'b> {
    pub fn expr(&self) -> Expr<'a, 'b> {
        Expr(&self.0.children[0])
    }
}

pub struct FunctionCall<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FunctionCall<'a, 'b> {
    pub fn id(&self) -> Identifier<'a, 'b> {
        Identifier(&self.0.children[0])
    }

    pub fn args(&self) -> Vec<FunctionCallArgs<'a, 'b>> {
        self.0.children[1..].iter().map(FunctionCallArgs).collect()
    }
}

pub struct Expr<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Expr<'a, 'b> {
    pub fn head(&self) -> Term<'a, 'b> {
        Term(&self.0.children[0])
    }

    pub fn tail(&self) -> Option<ExprPrime<'a, 'b>> {
        self.0.children.get(1).map(ExprPrime)
    }
}

pub struct ExprPrime<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> ExprPrime<'a, 'b> {
    pub fn op(&self) -> BooleanOp {
        self.0.ty.clone().into()
    }

    pub fn rhs(&self) -> Term<'a, 'b> {
        Term(&self.0.children[0])
    }

    pub fn tail(&self) -> Option<ExprPrime<'a, 'b>> {
        self.0.children.get(1).map(ExprPrime)
    }
}

pub struct Term<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Term<'a, 'b> {
    pub fn head(&self) -> Factor<'a, 'b> {
        Factor(&self.0.children[0])
    }

    pub fn tail(&self) -> Option<TermPrime<'a, 'b>> {
        self.0.children.get(1).map(TermPrime)
    }
}

pub struct TermPrime<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> TermPrime<'a, 'b> {
    pub fn op(&self) -> RelationalOp {
        self.0.ty.clone().into()
    }

    pub fn rhs(&self) -> Factor<'a, 'b> {
        Factor(&self.0.children[0])
    }

    pub fn tail(&self) -> Option<TermPrime<'a, 'b>> {
        self.0.children.get(1).map(TermPrime)
    }
}

pub struct Factor<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Factor<'a, 'b> {
    pub fn head(&self) -> Product<'a, 'b> {
        Product(&self.0.children[0])
    }

    pub fn tail(&self) -> Option<FactorPrime<'a, 'b>> {
        self.0.children.get(1).map(FactorPrime)
    }
}

pub struct FactorPrime<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> FactorPrime<'a, 'b> {
    pub fn op(&self) -> AdditiveOp {
        self.0.ty.clone().into()
    }

    pub fn rhs(&self) -> Product<'a, 'b> {
        Product(&self.0.children[0])
    }

    pub fn tail(&self) -> Option<FactorPrime<'a, 'b>> {
        self.0.children.get(1).map(FactorPrime)
    }
}

pub struct Product<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Product<'a, 'b> {
    pub fn head(&self) -> Atom<'a, 'b> {
        Atom(&self.0.children[0])
    }

    pub fn tail(&self) -> Option<ProductPrime<'a, 'b>> {
        self.0.children.get(1).map(ProductPrime)
    }
}

pub struct ProductPrime<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> ProductPrime<'a, 'b> {
    pub fn op(&self) -> MultiplicativeOp {
        self.0.ty.clone().into()
    }

    pub fn rhs(&self) -> Atom<'a, 'b> {
        Atom(&self.0.children[0])
    }

    pub fn tail(&self) -> Option<ProductPrime<'a, 'b>> {
        self.0.children.get(1).map(ProductPrime)
    }
}

pub struct Atom<'a, 'b>(&'a Node<'b>);

pub enum AtomType<'a, 'b> {
    Unit(Unit<'a, 'b>),
    Not(Atom<'a, 'b>),
}

impl<'a, 'b> Atom<'a, 'b> {
    pub fn downcast(&self) -> AtomType<'a, 'b> {
        let n = &self.0.children[0];
        match self.0.ty {
            NodeType::Not => AtomType::Not(Atom(n)),
            NodeType::Unit => AtomType::Unit(Unit(n)),
            _ => panic!("Invalid atom type"),
        }
    }
}

pub struct Unit<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Unit<'a, 'b> {
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

pub enum UnitType<'a, 'b> {
    Int(isize),
    Str(&'a Node<'b>),
    Bool(bool),
    Identifier(Identifier<'a, 'b>),
    FunctionCall(FunctionCall<'a, 'b>),
    BracketedExpr(Expr<'a, 'b>),
}

pub struct Identifier<'a, 'b>(&'a Node<'b>);

impl<'a, 'b> Identifier<'a, 'b> {
    pub fn identifier<'c>(&self, text: &'c str) -> &'c str {
        &text[self.0.text_range.clone()]
    }
}

pub enum AdditiveOp {
    Plus,
    Minus,
}

pub enum MultiplicativeOp {
    Times,
    Divide,
}

pub enum RelationalOp {
    LesserThan,
    GreaterThan,
    Equals,
    GreaterOrEquals,
    LesserOrEquals,
}

pub enum BooleanOp {
    And,
    Or,
}
