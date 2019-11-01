use oreo::lexer::error::format_lexical_error;
use oreo::lexer::lexicalize;
use oreo::lexer::scanner::scan;
use oreo::lexer::tokens::Token;
use oreo::range::*;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Args {
    /// The input to parse
    #[structopt(short, long)]
    input: String,
}

fn main() {
    let opt = Args::from_args();
    let input = opt.input;
    let tokens = lexicalize(scan(&input)).collect::<Vec<_>>();
    tokens
        .iter()
        .filter(|t| t.inner().is_error())
        .for_each(|error| {
            if let Token::Error(e) = error.inner() {
                println!(
                    "{}",
                    format_lexical_error(RangedObject::new(e, error.range().clone()), &input)
                );
            }
        });

    tokens
        .iter()
        .filter(|t| !t.inner().is_error())
        .for_each(|el| {
            println!("{:?}", el.inner());
        })
}
