// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Criterion benchmarks for Proven Rust bindings

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use proven::*;

fn bench_safe_math(c: &mut Criterion) {
    let mut group = c.benchmark_group("SafeMath");

    group.bench_function("safe_add", |b| {
        b.iter(|| safe_add(black_box(1000), black_box(2000)))
    });

    group.bench_function("safe_mul", |b| {
        b.iter(|| safe_mul(black_box(100), black_box(200)))
    });

    group.bench_function("safe_div", |b| {
        b.iter(|| safe_div(black_box(1000), black_box(7)))
    });

    group.bench_function("overflow_check", |b| {
        b.iter(|| safe_add(black_box(i64::MAX), black_box(1)))
    });

    group.finish();
}

fn bench_safe_string(c: &mut Criterion) {
    let mut group = c.benchmark_group("SafeString");
    let test_string = "The quick brown fox jumps over the lazy dog";

    group.bench_function("truncate", |b| {
        b.iter(|| truncate(black_box(test_string), black_box(20)))
    });

    group.bench_function("sanitize", |b| {
        b.iter(|| sanitize(black_box("<script>alert('xss')</script>")))
    });

    group.bench_function("validate_length", |b| {
        b.iter(|| validate_length(black_box(test_string), black_box(10), black_box(100)))
    });

    group.bench_function("validate_utf8", |b| {
        b.iter(|| validate_utf8(black_box(test_string)))
    });

    group.finish();
}

fn bench_safe_crypto(c: &mut Criterion) {
    let mut group = c.benchmark_group("SafeCrypto");
    let test_data = "benchmark test data for cryptographic operations";

    group.bench_function("sha256", |b| {
        b.iter(|| sha256(black_box(test_data)))
    });

    group.bench_function("sha512", |b| {
        b.iter(|| sha512(black_box(test_data)))
    });

    group.bench_function("hmac_sha256", |b| {
        b.iter(|| hmac_sha256(black_box("secret-key"), black_box(test_data)))
    });

    group.bench_function("random_bytes_32", |b| {
        b.iter(|| random_bytes(black_box(32)))
    });

    let hash = sha256(test_data);
    group.bench_function("constant_time_eq", |b| {
        b.iter(|| constant_time_eq(black_box(&hash), black_box(&hash)))
    });

    group.finish();
}

fn bench_safe_json(c: &mut Criterion) {
    let mut group = c.benchmark_group("SafeJson");
    let simple_json = r#"{"name":"test","value":42}"#;
    let nested_json = r#"{"user":{"name":"John","age":30},"items":[1,2,3]}"#;

    group.bench_function("parse_simple", |b| {
        b.iter(|| parse_json(black_box(simple_json)))
    });

    group.bench_function("parse_nested", |b| {
        b.iter(|| parse_json(black_box(nested_json)))
    });

    group.bench_function("stringify", |b| {
        let obj = JsonValue::Object(vec![
            ("name".to_string(), JsonValue::String("test".to_string())),
            ("value".to_string(), JsonValue::Number(42.0)),
        ]);
        b.iter(|| stringify(black_box(&obj)))
    });

    group.finish();
}

fn bench_safe_network(c: &mut Criterion) {
    let mut group = c.benchmark_group("SafeNetwork");

    group.bench_function("parse_ipv4", |b| {
        b.iter(|| parse_ipv4(black_box("192.168.1.1")))
    });

    group.bench_function("parse_ipv6", |b| {
        b.iter(|| parse_ipv6(black_box("2001:0db8:85a3:0000:0000:8a2e:0370:7334")))
    });

    group.bench_function("parse_cidr", |b| {
        b.iter(|| parse_cidr(black_box("10.0.0.0/8")))
    });

    group.bench_function("validate_port", |b| {
        b.iter(|| validate_port(black_box(8080)))
    });

    group.bench_function("is_private_ip", |b| {
        let ip = IPv4::new(192, 168, 1, 1);
        b.iter(|| is_private_ip(black_box(&ip)))
    });

    group.finish();
}

fn bench_safe_url(c: &mut Criterion) {
    let mut group = c.benchmark_group("SafeUrl");

    group.bench_function("parse_url", |b| {
        b.iter(|| parse_url(black_box("https://example.com/path?query=value")))
    });

    group.bench_function("encode_query_value", |b| {
        b.iter(|| encode_query_value(black_box("hello world & more")))
    });

    group.finish();
}

fn bench_safe_email(c: &mut Criterion) {
    let mut group = c.benchmark_group("SafeEmail");

    group.bench_function("validate_email", |b| {
        b.iter(|| validate_email(black_box("user+tag@mail.example.com")))
    });

    group.finish();
}

fn bench_safe_uuid(c: &mut Criterion) {
    let mut group = c.benchmark_group("SafeUUID");

    group.bench_function("parse_uuid", |b| {
        b.iter(|| parse_uuid(black_box("550e8400-e29b-41d4-a716-446655440000")))
    });

    group.bench_function("generate_uuid_v4", |b| {
        b.iter(|| generate_uuid_v4())
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_safe_math,
    bench_safe_string,
    bench_safe_crypto,
    bench_safe_json,
    bench_safe_network,
    bench_safe_url,
    bench_safe_email,
    bench_safe_uuid,
);

criterion_main!(benches);
