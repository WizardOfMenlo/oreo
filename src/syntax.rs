pub struct Identifier<'a>(&'a str);

pub struct Program<'a> {
    id: Identifier<'a>,
    compund: Compound<'a>,
}

pub struct Compound<'a> {
    statements: Vec<Statement<'a>>,
}

pub enum Statement<'a> {
    Decl(Decl<'a>),
    Print(Print<'a>),
    While(While<'a>),
    If(If<'a>),
    Assign(Assign<'a>),
}

pub struct Decl<'a> {
    id: Identifier<'a>,
    expr: Option<Expr<'a>>,
}

pub enum Print<'a> {
    Print(Expr<'a>),
    Println(Expr<'a>),
    Get(Identifier<'a>),
}

pub struct While<'a> {
    condition: Bool<'a>,
    compound: Compound<'a>,
}

pub struct If<'a> {
    condition: Bool<'a>,
    if_branch: Compound<'a>,
    else_branch: Option<Compound<'a>>,
}

pub struct Assign<'a> {
    id: Identifier<'a>,
    expr: Expr<'a>,
}

pub struct Expr<'a> {
    head: ExprHead<'a>,
    tail: Option<Box<ExprPrime<'a>>>
}

pub enum ExprHead<'a> {
    Boolean(Box<Bool<'a>>),
    BracketedExpr(Box<Expr<'a>>),
    Unit(Unit<'a>)
}

pub struct ExprPrime<'a> {
    operation: BinaryExprOp,
    operand: Expr<'a>,
    tail: Option<Box<ExprPrime<'a>>>
}

pub enum BinaryExprOp {
    Plus,
    Minus,
    Times,
    Divide
}

pub enum Unit<'a> {
    Int(isize),
    String(&'a str),
    Identifier(Identifier<'a>)
}

pub struct Bool<'a> {
    head: BoolHead<'a>,
    tail: Option<Box<BoolPrime<'a>>>
}

pub enum BoolHead<'a> {
    RelationalOperation(RelationalOperation<'a>),
    Not(Box<Bool<'a>>),
    BooleanLiteral(bool),
}

pub struct RelationalOperation<'a> {
    head: RelationalExprHead<'a>,
    exprtail: ExprPrime<'a>,
    operation: RelationalExprOp,
    right: Expr<'a>
}

pub enum RelationalExprHead<'a> {
    BracketedExpr(Box<Expr<'a>>),
    Unit(Unit<'a>)
}

pub enum RelationalExprOp {
    GreaterThan,
    LesserThan,
    GreaterEquals,
    LesserEquals,
    Equals,
}

pub enum RelationalOp {
    ExprRelation(RelationalExprOp),
    Or,
    And
}

pub struct BoolPrime<'a>
{
    head: ExprPrime<'a>,
    operation: RelationalOp,
    tail: Option<Box<BoolPrime<'a>>>
}