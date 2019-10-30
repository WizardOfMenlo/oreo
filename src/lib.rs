pub mod lexical;
pub mod nodebuilder;
pub mod parser;
pub mod range;
pub mod scanner;
pub mod syntax;
pub mod tokens;
pub mod tokenstream;
pub mod untyped;
//pub mod validator;

use untyped::Node;

pub fn parse(input: &str) -> Node {
    parser::parse(lexical::lexicalize(scanner::scan(input)))
}
