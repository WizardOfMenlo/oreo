use oreo::lexical::lexicalize;
use oreo::scanner::scan;
use oreo::tokens::{LexicalError, Token};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Args {
    /// The input to parse
    #[structopt(short, long)]
    input: String,
}

fn print_error(error: &LexicalError, line: usize) {
    let (exp, found) = match error {
        LexicalError::ExpectedDoubleEqualsEOF => ('=', None),
        LexicalError::ExpectedAssignementEOF => ('=', None),
        LexicalError::ExpectedDoubleEquals(c) => ('=', Some(c)),
        LexicalError::ExpectedAssignement(c) => ('=', Some(c)),
        LexicalError::UnknownChar(c) => {
            println!("Unknown char '{}' at line {}.", c, line);
            return;
        }
        LexicalError::UnclosedString(s) => {
            println!("Unclosed string \"{}\" at line {}.", s, line);
            return;
        }
    };

    println!(
        "Expected '{}', found '{}' at line {}",
        exp,
        found
            .map(ToString::to_string)
            .unwrap_or_else(|| String::from("EOF")),
        line
    );
}

fn main() {
    let opt = Args::from_args();
    let tokens = lexicalize(scan(&opt.input)).collect::<Vec<_>>();
    tokens
        .iter()
        .filter(|t| t.token().is_error())
        .for_each(|error| match error.token() {
            Token::Error(e) => print_error(e, error.line_no()),
            _ => {}
        });

    tokens
        .iter()
        .filter(|t| !t.token().is_error())
        .for_each(|el| {
            println!("{:?}", el.token());
        })
}
