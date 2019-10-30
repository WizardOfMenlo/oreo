pub mod lexical;
pub mod parser;
pub mod parsing_utils;
pub mod range;
pub mod scanner;
pub mod syntax;
pub mod tokens;
pub mod untyped;
//pub mod validator;

use untyped::Node;

pub fn parse(input: &str) -> Node {
    parser::parse(lexical::lexicalize(scanner::scan(input)))
}
