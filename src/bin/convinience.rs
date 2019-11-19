use oreo::ast::syntax::Program;
use oreo::ast::AST;
use oreo::codegen::tac::TACBuilder;
use oreo::codegen::*;
use oreo::lexer::lexicalize;
use oreo::lexer::scanner::scan;
use oreo::parser::parse;

fn main() {
    let input = r#"program x
        begin
            var x := false;
            var y := not x;
        end"#;

    // Note, no error handling here
    let node = parse(lexicalize(scan(input)));

    dbg!(&node);

    let ast = AST::new(node, input);

    dbg!(&ast);

    let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));

    println!("{}", tac);

    let hla = HLABuilder::new().build(&tac);

    dbg!(hla);
}
