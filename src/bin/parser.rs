use oreo::lexical::lexicalize;
use oreo::parser::parse;
use oreo::scanner::scan;

fn main() {
    let input = r#"
        program x
        begin
            if (id1 < (id2 < id3)) then begin x := 1 end;
        end
        "#;

    let result = parse(lexicalize(scan(input)));
    println!("{:?}", result);
}
