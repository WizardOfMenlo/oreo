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

pub enum Expr<'a> {
    Addition(Box<BinaryOperator<'a>>),
    Subtraction(Box<BinaryOperator<'a>>),
    Multiplication(Box<BinaryOperator<'a>>),
    Division(Box<BinaryOperator<'a>>),
    Bool(Box<Bool<'a>>),
    Expr(Box<Expr<'a>>),
    Literal(Literal<'a>),
    Identifier(Identifier<'a>)
}

pub enum Literal<'a> {
    Str(&'a str),
    Int(isize)
}

pub struct BinaryOperator<'a> {
    left: Expr<'a>,
    right: Expr<'a>,
}

pub enum Bool<'a> {
    LessThan(BooleanRelOperator<'a>),
    GreaterThan(BooleanRelOperator<'a>),
    LesserOrEquals(BooleanRelOperator<'a>),
    GreaterOrEquals(BooleanRelOperator<'a>),
    Equals(BooleanRelOperator<'a>),
    And(Box<BooleanBinaryOperator<'a>>),
    Or(Box<BooleanBinaryOperator<'a>>),
    Not(Box<Bool<'a>>),
    BooleanLiteral(bool),
}

pub struct BooleanRelOperator<'a> {
    first: Expr<'a>,
    second: Expr<'a>,
}

pub struct BooleanBinaryOperator<'a> {
    first: Bool<'a>,
    second: Bool<'a>,
}