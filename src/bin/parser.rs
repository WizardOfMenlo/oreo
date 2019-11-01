use oreo::lexer::error::format_lexical_error;
use oreo::lexer::lexicalize;
use oreo::lexer::scanner::scan;
use oreo::lexer::tokens::Token;
use oreo::parser::error::format_syntax_error;
use oreo::parser::parse;
use oreo::range::*;
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

    parse_tree.iter_breadth_first().for_each(|n| {
        if n.ty().is_error() {
            println!(
                "{}",
                format_syntax_error(
                    RangedObject::new(n.ty().unwrap_err(), n.range().clone()),
                    &opt.input
                )
            )
        }
    });

    if parse_tree.iter_breadth_first().any(|n| n.ty().is_error()) {
        println!("Fix the above errors");
        return;
    }

    use oreo::ast::untyped::*;

    let node = Node::new(
        NodeType::Program,
        0..10,
        vec![
            Node::new(
                NodeType::Compound,
                0..5,
                vec![Node::new(NodeType::If, 0..3, Vec::new())],
            ),
            Node::new(NodeType::While, 5..10, Vec::new()),
        ],
    );

    let out = match opt.mode {
        LexMode::Json => serde_json::to_string_pretty(&parse_tree).unwrap(),
        LexMode::Yaml => serde_yaml::to_string(&parse_tree).unwrap(),
        LexMode::Rust => format!("{:?}", parse_tree),
    };

    println!("{}", out);
}
