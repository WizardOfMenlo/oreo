pub mod lexical;
pub mod parser;
pub mod parsing_utils;
pub mod scanner;
pub mod syntax;
pub mod tokens;
pub mod validator;

use parser::ParsingResult;
use syntax::Program;

pub fn parse<'a>(input: &'a str) -> ParsingResult<'a, Program<'a>> {
    parser::parse(lexical::lexicalize(scanner::scan(input)))
}
