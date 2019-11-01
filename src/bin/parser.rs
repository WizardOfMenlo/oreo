use oreo::parser::error::format_syntax_error;
use oreo::lexer::error::format_lexical_error;
use oreo::lexer::lexicalize;
use oreo::parser::parse;
use oreo::range::RangedObject;
use oreo::lexer::scanner::scan;
use oreo::lexer::tokens::Token;
use oreo::tree_utils::for_each_depth_first;
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

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    let opt = Args::from_args();
    let tokens = lexicalize(scan(&opt.input)).collect::<Vec<_>>();
    // Print lexing errors before parsing error
    tokens
        .iter()
        .filter(|t| t.inner().is_error())
        .for_each(|error| {
            if let Token::Error(e) = error.inner() {
                println!(
                    "{}",
                    format_lexical_error(error.clone().map(|_| e), &opt.input)
                );
            }
        });

    let parse_tree = parse(tokens.into_iter());

    for_each_depth_first(&parse_tree, |n| {
        if n.ty.is_error() {
            println!(
                "{}",
                format_syntax_error(
                    RangedObject::new(n.ty.unwrap_err(), n.text_range.clone()),
                    &opt.input
                )
            )
        }
    });

    let out = match opt.mode {
        LexMode::Json => serde_json::to_string_pretty(&parse_tree).unwrap(),
        LexMode::Yaml => serde_yaml::to_string(&parse_tree).unwrap(),
        LexMode::Rust => format!("{:?}", parse_tree),
    };

    println!("{}", out);
}
