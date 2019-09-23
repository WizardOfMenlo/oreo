use oreo::lexical::lexicalize;
use oreo::scanner::scan;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Args {
    /// The input to parse
    #[structopt(short, long)]
    input: String,
}

fn main() {
    let opt = Args::from_args();
    println!("{:?}", lexicalize(scan(&opt.input)).collect::<Vec<_>>());
}
