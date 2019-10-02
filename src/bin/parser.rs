use oreo::lexical::lexicalize;
use oreo::parser::parse;
use oreo::scanner::scan;

fn main() {
    let input = r#"
        program x
        begin
            var x := a < b;
        end
        "#;

    let result = parse(lexicalize(scan(input)));
    println!("{:?}", result);
}
