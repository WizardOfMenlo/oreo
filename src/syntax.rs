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
    FunctionCall(FunctionCall<'a>),
}

#[derive(Debug)]
pub struct FunctionCall<'a> {
    pub id: Identifier<'a>,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug)]
pub struct Bool<'a> {
    pub head: Box<BooleanHead<'a>>,
    pub tail: Option<Box<BoolTail<'a>>>,
}

#[derive(Debug)]
pub enum BooleanHead<'a> {
    RelationalOperation(RelationalOperation<'a>),
    Not(Bool<'a>),
    BooleanLiteral(bool),
}

#[derive(Debug)]
pub struct RelationalOperation<'a> {
    pub head: BooleanUnit<'a>,
    pub tail: RelationalOperationTail<'a>,
}

#[derive(Debug)]
pub struct RelationalOperationTail<'a> {
    pub opt_lhs_tail: Option<ExprPrime<'a>>,
    pub operator: RelationalOperator,
    pub rhs: Expr<'a>,
}

#[derive(Debug)]
pub enum BooleanUnit<'a> {
    BracketedExpr(Expr<'a>),
    Unit(Unit<'a>),
}

#[derive(Debug)]
pub enum RelationalOperator {
    LesserThan,
    GreaterThan,
    Equals,
    GreaterOrEquals,
    LesserOrEquals,
}

#[derive(Debug)]
pub struct BoolTail<'a> {
    pub head: BoolTailHeadExpr<'a>,
    pub tail: Option<Box<BoolTail<'a>>>,
}

#[derive(Debug)]
pub enum BoolTailHeadExpr<'a> {
    RelationalOperationTail(RelationalOperationTail<'a>),
    BooleanOperation(BooleanOperation<'a>),
}

#[derive(Debug)]
pub struct BooleanOperation<'a> {
    pub op: BooleanOperator,
    pub rhs: Bool<'a>,
}

#[derive(Debug)]
pub enum BooleanOperator {
    And,
    Or,
}
