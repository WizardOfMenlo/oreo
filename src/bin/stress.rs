use criterion::black_box;
use oreo::parse;

fn generate_program_from_size(s: usize) -> String {
    format!(
        "program fib begin {} end",
        std::iter::repeat("var x := x;")
            .take(s)
            .collect::<Vec<_>>()
            .join("")
    )
}

fn main() {
    let input = generate_program_from_size(16384);
    let result = parse(&input);
    black_box(result);
}
