use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use oreo::lexer::lexicalize;
use oreo::lexer::scanner::scan;
use oreo::parser::parse;

fn generate_program_from_size(s: usize) -> String {
    format!(
        "program fib begin {} end",
        std::iter::repeat("var x := x;")
            .take(s)
            .collect::<Vec<_>>()
            .join("")
    )
}

fn generate_branching_program_from_size(s: usize) -> String {
    let mut nested_if = String::from("%");
    let cond = "if (true) then begin % end;";

    for _ in 0..s {
        nested_if = nested_if.replace('%', cond)
    }

    nested_if = nested_if.replace('%', "var x := 0");

    format!("program fib begin {} end", nested_if)
}

fn generate_complex_epr_from_size(s: usize) -> String {
    let mut nested_if = String::from("%");
    let cond = "not 1 * 2 + 3 <= 4 and %";

    for _ in 0..s {
        nested_if = nested_if.replace('%', cond)
    }

    nested_if = nested_if.replace('%', "var x := 0");

    format!("program fib begin var x := {}; end", nested_if)
}

fn parsing_linear(c: &mut Criterion) {
    let sizes = [128, 256, 512, 1024, 2048];
    let mut group = c.benchmark_group("parsing_lin");

    for (s, p) in sizes
        .iter()
        .copied()
        .map(|s| (s, generate_program_from_size(s)))
    {
        group.throughput(Throughput::Bytes(p.len() as u64));
        let tokens = lexicalize(scan(&p)).collect::<Vec<_>>();
        group.bench_with_input(BenchmarkId::from_parameter(s), &tokens, |b, p| {
            b.iter(|| parse(p.iter().cloned()));
        });
    }
    group.finish();
}

fn scanning(c: &mut Criterion) {
    let sizes = [128, 256, 512, 1024, 2048];
    let mut group = c.benchmark_group("scannin");

    for (s, p) in sizes
        .iter()
        .copied()
        .map(|s| (s, generate_program_from_size(s)))
    {
        group.throughput(Throughput::Bytes(p.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(s), &p, |b, p| {
            b.iter(|| scan(p).collect::<Vec<_>>());
        });
    }
    group.finish();
}

fn lexical(c: &mut Criterion) {
    let sizes = [128, 256, 512, 1024, 2048];
    let mut group = c.benchmark_group("lexical");

    for (s, p) in sizes
        .iter()
        .copied()
        .map(|s| (s, generate_program_from_size(s)))
    {
        group.throughput(Throughput::Bytes(p.len() as u64));
        // Removed scan overhead here
        let scanned = scan(&p).collect::<Vec<_>>();
        group.bench_with_input(BenchmarkId::from_parameter(s), &scanned, |b, p| {
            b.iter(|| lexicalize(p.iter().cloned()).collect::<Vec<_>>());
        });
    }
    group.finish();
}

fn parsing_nest(c: &mut Criterion) {
    let sizes = [2, 4, 8, 16, 32];
    let mut group = c.benchmark_group("parsing_nested");

    for (s, p) in sizes
        .iter()
        .copied()
        .map(|s| (s, generate_branching_program_from_size(s)))
    {
        group.throughput(Throughput::Bytes(p.len() as u64));
        let tokens = lexicalize(scan(&p)).collect::<Vec<_>>();
        group.bench_with_input(BenchmarkId::from_parameter(s), &tokens, |b, p| {
            b.iter(|| parse(p.iter().cloned()));
        });
    }
    group.finish();
}

fn parsing_expr(c: &mut Criterion) {
    let sizes = [2, 4, 8, 16, 32];
    let mut group = c.benchmark_group("parsing_expr");

    for (s, p) in sizes
        .iter()
        .copied()
        .map(|s| (s, generate_complex_epr_from_size(s)))
    {
        group.throughput(Throughput::Bytes(p.len() as u64));
        let tokens = lexicalize(scan(&p)).collect::<Vec<_>>();
        group.bench_with_input(BenchmarkId::from_parameter(s), &tokens, |b, p| {
            b.iter(|| parse(p.iter().cloned()));
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    parsing_expr,
    scanning,
    lexical,
    parsing_linear,
    parsing_nest
);
criterion_main!(benches);
