//! Mod with some things we use more or less everywhere

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
