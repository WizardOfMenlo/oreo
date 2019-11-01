use oreo::lexer::lexicalize;
use oreo::lexer::scanner::scan;
use oreo::lexer::tokens::{LexicalError, Token};
use oreo::range::RangedObject;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Args {
    /// The input to parse
    #[structopt(short, long)]
    input: String,
}

fn print_error(error: RangedObject<&LexicalError>, input: &str) {
    let (exp, found) = match error.inner() {
        LexicalError::ExpectedDoubleEqualsEOF => ('=', None),
        LexicalError::ExpectedAssignementEOF => ('=', None),
        LexicalError::ExpectedDoubleEquals(c) => ('=', Some(c)),
        LexicalError::ExpectedAssignement(c) => ('=', Some(c)),
        LexicalError::UnknownChar(c) => {
            println!("Unknown char '{}' in {}.", c, error.span(input));
            return;
        }
        LexicalError::UnclosedString(s) => {
            println!("Unclosed string \"{}\" in {}.", s, error.span(input));
            return;
        }
        LexicalError::UnclosedComment(s) => {
            println!("Unclosed comment \"{}\" in {}.", s, error.span(input));
            return;
        }
    };

    println!(
        "Expected '{}', found '{}' in {}",
        exp,
        found
            .map(ToString::to_string)
            .unwrap_or_else(|| String::from("EOF")),
        error.span(input)
    );
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
                print_error(RangedObject::new(e, error.range().clone()), &input)
            }
        });

    tokens
        .iter()
        .filter(|t| !t.inner().is_error())
        .for_each(|el| {
            println!("{:?}", el.inner());
        })
}
