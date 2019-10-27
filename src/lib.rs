pub mod lexical;
pub mod parser;
pub mod parsing_utils;
pub mod range;
pub mod scanner;
pub mod syntax;
pub mod tokens;
pub mod validator;

use parser::ParsingResult;
use syntax::Program;

pub fn parse(input: &str) -> ParsingResult<Program> {
    parser::parse(lexical::lexicalize(scanner::scan(input)))
}
