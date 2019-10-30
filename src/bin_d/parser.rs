use oreo::lexical::lexicalize;
use oreo::parser::{parse, SyntaxError};
use oreo::range::RangedObject;
use oreo::scanner::scan;
use oreo::tokens::{LexicalError, Token};
use std::str::FromStr;
use structopt::StructOpt;

#[derive(Debug)]
enum LexMode {
    Json,
    Yaml,
    Rust,
}

impl FromStr for LexMode {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<LexMode, Self::Err> {
        Ok(match s {
            "json" => LexMode::Json,
            "yml" => LexMode::Yaml,
            "rust" => LexMode::Rust,
            _ => return Err("Invalid mode"),
        })
    }
}

#[derive(Debug, StructOpt)]
struct Args {
    /// The input to parse
    #[structopt(short, long)]
    input: String,

    mode: LexMode,
}

fn print_lexical_error(error: RangedObject<&LexicalError>, input: &str) {
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

fn token_to_string<'a>(t: &Token<'a>) -> String {
    format!("{:?}", t)
}

fn token_list_to_string(t: impl IntoIterator<Item = &'static Token<'static>>) -> String {
    t.into_iter()
        .map(token_to_string)
        .collect::<Vec<_>>()
        .join(",")
}

fn print_syntax_error(error: SyntaxError, input: &str) {
    let (expected, found) = match error {
        SyntaxError::ExpectedOneOfButFoundEOF(l) => (token_list_to_string(l), None),
        SyntaxError::ExpectedButFoundEOF(t) => (token_to_string(&t), None),
        SyntaxError::ExpectedFound(e, f) => (token_to_string(&e), Some(f)),
        SyntaxError::ExpectedOneOfFound(e, f) => (token_list_to_string(e), Some(f)),
        SyntaxError::LogicalError => {
            println!("Something went really wrong");
            return;
        }
    };

    let range = found
        .as_ref()
        .map(|t| t.range().clone())
        .unwrap_or(0..input.len());

    println!(
        "Expected {}, found {} in {}",
        expected,
        found
            .map(|t| token_to_string(t.inner()))
            .unwrap_or_else(|| String::from("EOF")),
        &input[range]
    )
}

fn main() {
    let opt = Args::from_args();
    let tokens = lexicalize(scan(&opt.input)).collect::<Vec<_>>();
    // Print lexing errors before parsing error
    tokens
        .iter()
        .filter(|t| t.inner().is_error())
        .for_each(|error| {
            if let Token::Error(e) = error.inner() {
                print_lexical_error(RangedObject::new(e, error.range().clone()), &opt.input)
            }
        });

    let parse_res = parse(tokens.into_iter());

    if let Err(err) = parse_res {
        print_syntax_error(err, &opt.input);
        return;
    }

    let program = parse_res.unwrap();

    let validation = oreo::validator::validate_program(&program);
    if let Err(err) = validation {
        println!("Error validating the typing, {:?}", err);
    }

    let out = match opt.mode {
        LexMode::Json => serde_json::to_string_pretty(&program).unwrap(),
        LexMode::Yaml => serde_yaml::to_string(&program).unwrap(),
        LexMode::Rust => format!("{:?}", program),
    };

    println!("{}", out);
}
