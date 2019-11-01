pub mod error;
pub mod lexical;
pub mod node_builder;
pub mod parser;
pub mod range;
pub mod scanner;
pub mod syntax;
pub mod token_stream;
pub mod tokens;
pub mod tree_utils;
pub mod untyped;

use untyped::Node;

pub fn parse(input: &str) -> Node {
    parser::parse(lexical::lexicalize(scanner::scan(input)))
}
