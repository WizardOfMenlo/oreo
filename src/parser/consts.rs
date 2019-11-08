//! A lot of constants that we use in the parsing phase

use super::{ExpToken, TokenList};
use crate::lexer::tokens::*;

pub(crate) const ID: ExpToken = Token::Identifier("");
pub(crate) const INT: ExpToken = Token::Literal(Literal::Integer(0));
pub(crate) const STRING: ExpToken = Token::Literal(Literal::Str(""));
pub(crate) const BOOL: ExpToken = Token::Literal(Literal::Boolean(false));

pub(crate) const TILDE: ExpToken = Token::Punctuation(Punctuation::Tilde);
pub(crate) const SEMICOLON: ExpToken = Token::Punctuation(Punctuation::Semicolon);
pub(crate) const B_OPEN: ExpToken = Token::Punctuation(Punctuation::BracketOpen);
pub(crate) const B_CLOSE: ExpToken = Token::Punctuation(Punctuation::BracketClose);

pub(crate) const IF: ExpToken = Token::Keyword(Keyword::If);
pub(crate) const PRINT: ExpToken = Token::Keyword(Keyword::Print);
pub(crate) const PRINTLN: ExpToken = Token::Keyword(Keyword::Println);
pub(crate) const GET: ExpToken = Token::Keyword(Keyword::Get);
pub(crate) const WHILE: ExpToken = Token::Keyword(Keyword::While);
pub(crate) const PROCEDURE: ExpToken = Token::Keyword(Keyword::Procedure);
pub(crate) const RETURN: ExpToken = Token::Keyword(Keyword::Return);
pub(crate) const VAR: ExpToken = Token::Keyword(Keyword::Var);
pub(crate) const END: ExpToken = Token::Keyword(Keyword::End);

pub(crate) const ASSIGN: ExpToken = Token::Operator(Operator::Assignement);

pub(crate) const POSSIBLE_DECL_SYMBOLS: TokenList = &[ASSIGN, SEMICOLON];

pub(crate) const PLUS: ExpToken = Token::Operator(Operator::Plus);
pub(crate) const MINUS: ExpToken = Token::Operator(Operator::Minus);

pub(crate) const TIMES: ExpToken = Token::Operator(Operator::Times);
pub(crate) const DIVIDE: ExpToken = Token::Operator(Operator::Divide);

pub(crate) const AND: ExpToken = Token::Operator(Operator::And);
pub(crate) const OR: ExpToken = Token::Operator(Operator::Or);

pub(crate) const LESS: ExpToken = Token::Operator(Operator::LesserThan);
pub(crate) const GREAT: ExpToken = Token::Operator(Operator::GreaterThan);
pub(crate) const LEQ: ExpToken = Token::Operator(Operator::LesserOrEquals);
pub(crate) const GEQ: ExpToken = Token::Operator(Operator::GreaterOrEquals);
pub(crate) const EQ: ExpToken = Token::Operator(Operator::Equals);

pub(crate) const NOT: ExpToken = Token::Operator(Operator::Not);

pub(crate) const ADDITIVE_OPS: TokenList = &[PLUS, MINUS];

pub(crate) const MULTIPLICATIVE_OPS: TokenList = &[TIMES, DIVIDE];

pub(crate) const RELATIONAL_OPS: TokenList = &[LESS, GREAT, GEQ, LEQ, EQ];

pub(crate) const BOOL_OPS: TokenList = &[AND, OR];

pub(crate) const POSSIBLE_STATEMENT_STARTS: TokenList =
    &[VAR, IF, PRINT, PRINTLN, GET, WHILE, PROCEDURE, RETURN, ID];

pub(crate) const PRINT_STARTS: TokenList = &[PRINT, PRINTLN, GET];

pub(crate) const POSSIBLE_TYPES: TokenList = &[
    Token::Type(Type::Integer),
    Token::Type(Type::Boolean),
    Token::Type(Type::Str),
];

pub(crate) const POSSIBLE_UNIT_STARTS: TokenList = &[ID, B_OPEN, STRING, INT, BOOL];

pub(crate) const POSSIBLE_EXPR_PRIME_STARTS: TokenList = BOOL_OPS;

pub(crate) const POSSIBLE_TERM_PRIME_STARTS: TokenList = RELATIONAL_OPS;

pub(crate) const POSSIBLE_FACTOR_PRIME_STARTS: TokenList = ADDITIVE_OPS;

pub(crate) const POSSIBLE_PRODUCT_PRIME_STARTS: TokenList = MULTIPLICATIVE_OPS;

pub(crate) const POSSIBLE_ATOM_START: TokenList = &[NOT, ID, B_OPEN, STRING, BOOL, INT];
