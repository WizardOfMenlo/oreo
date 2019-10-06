use oreo::lexical::lexicalize;
use oreo::parser::parse;
use oreo::scanner::scan;
use oreo::validator::validate_program;

fn main() {
    let input = r#"
program test
                                   
begin                              
    if ( 42 ) then begin n := 5;  end
       			   else begin n := 46; end;
end    
        "#;

    let result = parse(lexicalize(scan(input))).unwrap();
    println!("{:?}", result);
    println!("{:?}", validate_program(&result));
}
