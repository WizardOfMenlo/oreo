use oreo::ast::syntax::Program;
use oreo::ast::AST;
use oreo::codegen::tac::TACBuilder;
use oreo::lexer::lexicalize;
use oreo::lexer::scanner::scan;
use oreo::parser::parse;

use structopt::StructOpt;

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
    // Note, no error handling here
    let node = parse(lexicalize(scan(input)));

    dbg!(&node);

    let ast = AST::new(node, input);

    dbg!(&ast);

    let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));

    dbg!(&tac);

    println!("{}", tac);
}
