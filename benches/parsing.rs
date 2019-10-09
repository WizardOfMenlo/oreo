use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
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

fn parsing(c: &mut Criterion) {
    let sizes = [128, 256, 512, 1024, 2048];
    let mut group = c.benchmark_group("parsing");

    for p in sizes.iter().copied().map(generate_program_from_size) {
        group.throughput(Throughput::Bytes(p.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(&p), &p, |b, p| {
            b.iter(|| parse(p));
        });
    }
    group.finish();
}

criterion_group!(benches, parsing);
criterion_main!(benches);
