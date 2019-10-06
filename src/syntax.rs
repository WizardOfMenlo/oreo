#[derive(Debug, Clone)]
pub struct Identifier<'a>(pub(crate) &'a str);

#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub id: Identifier<'a>,
    pub compound: Compound<'a>,
}

#[derive(Debug, Clone)]
pub struct Compound<'a> {
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Decl(Decl<'a>),
    Print(Print<'a>),
    While(While<'a>),
    If(If<'a>),
    Assign(Assign<'a>),
    FunctionDecl(FunctionDecl<'a>),
    Return(Return<'a>),
}

#[derive(Debug, Clone)]
pub struct Decl<'a> {
    pub id: Identifier<'a>,
    pub expr: Option<Expr<'a>>,
}

#[derive(Debug, Clone)]
pub enum Print<'a> {
    Print(Expr<'a>),
    Println(Expr<'a>),
    Get(Identifier<'a>),
}

#[derive(Debug, Clone)]
pub struct While<'a> {
    pub condition: Expr<'a>,
    pub compound: Compound<'a>,
}

#[derive(Debug, Clone)]
pub struct If<'a> {
    pub condition: Expr<'a>,
    pub if_branch: Compound<'a>,
    pub else_branch: Option<Compound<'a>>,
}

#[derive(Debug, Clone)]
pub struct Assign<'a> {
    pub id: Identifier<'a>,
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl<'a> {
    pub id: Identifier<'a>,
    pub args: Vec<Identifier<'a>>,
    pub inner: Compound<'a>,
}

#[derive(Debug, Clone)]
pub struct Return<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone)]
pub enum AdditiveOp {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
pub enum MultiplicativeOp {
    Times,
    Divide,
}

#[derive(Debug, Clone)]
pub enum RelationalOp {
    LesserThan,
    GreaterThan,
    Equals,
    GreaterOrEquals,
    LesserOrEquals,
}

#[derive(Debug, Clone)]
pub enum BooleanOp {
    And,
    Or,
}

#[derive(Debug, Clone)]
pub struct Expr<'a> {
    pub head: Term<'a>,
    pub tail: Option<ExprPrime<'a>>,
}

#[derive(Debug, Clone)]
pub struct ExprPrime<'a> {
    pub op: BooleanOp,
    pub rhs: Term<'a>,
    pub tail: Option<Box<ExprPrime<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Term<'a> {
    pub head: Factor<'a>,
    pub tail: Option<TermPrime<'a>>,
}

#[derive(Debug, Clone)]
pub struct TermPrime<'a> {
    pub op: RelationalOp,
    pub rhs: Factor<'a>,
    pub tail: Option<Box<TermPrime<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Factor<'a> {
    pub head: Product<'a>,
    pub tail: Option<FactorPrime<'a>>,
}

#[derive(Debug, Clone)]
pub struct FactorPrime<'a> {
    pub op: AdditiveOp,
    pub rhs: Product<'a>,
    pub tail: Option<Box<FactorPrime<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Product<'a> {
    pub head: Atom<'a>,
    pub tail: Option<ProductPrime<'a>>,
}

#[derive(Debug, Clone)]
pub struct ProductPrime<'a> {
    pub op: MultiplicativeOp,
    pub rhs: Atom<'a>,
    pub tail: Option<Box<ProductPrime<'a>>>,
}

#[derive(Debug, Clone)]
pub enum Atom<'a> {
    Unit(Unit<'a>),
    Not(Box<Atom<'a>>),
}

#[derive(Debug, Clone)]
pub enum Unit<'a> {
    Int(isize),
    Str(&'a str),
    Boolean(bool),
    Identifier(Identifier<'a>),
    FunctionCall(FunctionCall<'a>),
    BracketedExpr(Box<Expr<'a>>),
}

#[derive(Debug, Clone)]
pub struct FunctionCall<'a> {
    pub id: Identifier<'a>,
    pub args: Vec<Expr<'a>>,
}
