//! Crate containing a parser and (hopefully) compiler for the oreo programming languages

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod range;
pub mod tree_utils;

use ast::untyped::Node;

/// Utility function parsing a string into a text node
/// ```
///     use oreo::parse;
///     let prog = parse("program fib begin procedure id(var x) begin return x; end end");
/// ```
pub fn parse(input: &str) -> Node {
    parser::parse(lexer::lexicalize(lexer::scanner::scan(input)))
}
