use oreo::ast::syntax::Program;
use oreo::ast::AST;
use oreo::codegen::tac::TACBuilder;
use oreo::codegen::*;
use oreo::lexer::lexicalize;
use oreo::lexer::scanner::scan;
use oreo::parser::parse;
use structopt::StructOpt;

use oreo::range::{Ranged, RangedObject};
use oreo::lexer::tokens::Token;
use oreo::lexer::error::format_lexical_error;
use oreo::parser::error::format_syntax_error;

#[derive(Debug, StructOpt)]
struct Args {
    /// The input to parse
    #[structopt(short, long)]
    input: String,
}

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    let opt = Args::from_args();
    let input = &opt.input;

    let tokens : Vec<_> = lexicalize(scan(input)).collect();

    tokens
        .iter()
        .filter(|t| t.inner().is_error())
        .for_each(|error| {
            if let Token::Error(e) = error.inner() {
                println!(
                    "{}",
                    format_lexical_error(error.clone().map(|_| e), &opt.input)
                );
            }
        });

    let node = parse(tokens.into_iter());

    node.iter_breadth_first().for_each(|n| {
        if n.ty().is_error() {
            println!(
                "{}",
                format_syntax_error(
                    RangedObject::new(n.ty().unwrap_err(), n.range().clone()),
                    &opt.input
                )
            )
        }
    });

    if node.iter_breadth_first().any(|n| n.ty().is_error()) {
        println!("Fix the above errors");
        return;
    }

    let ast = AST::new(node, input);

    let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));

    let hla = HLABuilder::new(HLABuilder::default_regs()).build(&tac);

    for instr in hla {
        println!("{}", instr);
    }
}
