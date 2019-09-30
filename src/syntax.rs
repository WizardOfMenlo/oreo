#[derive(Debug)]
pub struct Identifier<'a>(pub(crate) &'a str);

#[derive(Debug)]
pub struct Program<'a> {
    pub id: Identifier<'a>,
    pub compound: Compound<'a>,
}

#[derive(Debug)]
pub struct Compound<'a> {
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Decl(Decl<'a>),
    Print(Print<'a>),
    While(While<'a>),
    If(If<'a>),
    Assign(Assign<'a>),
    FunctionDecl(FunctionDecl<'a>),
    Return(Return<'a>),
}

#[derive(Debug)]
pub struct Decl<'a> {
    pub id: Identifier<'a>,
    pub expr: Option<Expr<'a>>,
}

#[derive(Debug)]
pub enum Print<'a> {
    Print(Expr<'a>),
    Println(Expr<'a>),
    Get(Identifier<'a>),
}

#[derive(Debug)]
pub struct While<'a> {
    pub condition: Bool<'a>,
    pub compound: Compound<'a>,
}

#[derive(Debug)]
pub struct If<'a> {
    pub condition: Bool<'a>,
    pub if_branch: Compound<'a>,
    pub else_branch: Option<Compound<'a>>,
}

#[derive(Debug)]
pub struct Assign<'a> {
    pub id: Identifier<'a>,
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub struct FunctionDecl<'a> {
    pub id: Identifier<'a>,
    pub args: Vec<Identifier<'a>>,
    pub inner: Compound<'a>,
}

#[derive(Debug)]
pub struct Return<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub struct Expr<'a> {
    pub head: ExprHead<'a>,
    pub tail: Option<Box<ExprPrime<'a>>>,
}

#[derive(Debug)]
pub enum ExprHead<'a> {
    Boolean(Box<Bool<'a>>),
    BracketedExpr(Box<Expr<'a>>),
    Unit(Unit<'a>),
}

#[derive(Debug)]
pub struct ExprPrime<'a> {
    pub operator: BinaryExprOp,
    pub operand: Expr<'a>,
    pub tail: Option<Box<ExprPrime<'a>>>,
}

#[derive(Debug)]
pub enum BinaryExprOp {
    Plus,
    Minus,
    Times,
    Divide,
}

#[derive(Debug)]
pub enum Unit<'a> {
    Int(isize),
    String(&'a str),
    Identifier(Identifier<'a>),
}

#[derive(Debug)]
pub struct Bool<'a> {
    s: std::marker::PhantomData<&'a usize>,
}
