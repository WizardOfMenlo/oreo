pub mod parser;
pub mod ast;
pub mod range;
pub mod lexer;
pub mod tree_utils;

use ast::untyped::Node;

pub fn parse(input: &str) -> Node {
    parser::parse(lexer::lexicalize(lexer::scanner::scan(input)))
}
