use oreo::ast::syntax::Program;
use oreo::ast::AST;
use oreo::codegen::tac::TACBuilder;
use oreo::lexer::lexicalize;
use oreo::lexer::scanner::scan;
use oreo::parser::parse;


fn main() {
    let input = r#"program x
        begin
            procedure int f()
            begin
                return f();
            end 
        end"#;


    // Note, no error handling here
    let node = parse(lexicalize(scan(input)));

    dbg!(&node);

    let ast = AST::new(node, input);

    dbg!(&ast);

    let tac = TACBuilder::new(&ast).build(Program::new(ast.db().start_id()));

    dbg!(&tac);

    println!("{}", tac);
}
