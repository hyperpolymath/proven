# Benchmarks

Performance benchmarks for the proven library.

## Running Benchmarks

### Idris 2 Benchmarks

```bash
cd benchmarks
idris2 --build benchmarks.ipkg
./build/exec/benchmarks
```

### Rust Benchmarks (criterion)

```bash
cd bindings/rust
cargo bench
```

### Python Benchmarks

```bash
cd bindings/python
python -m pytest benchmarks/ --benchmark-only
```

## Benchmark Results

Last run: 2025-01-12

### Parsing Operations

| Operation | Input Size | Time (avg) | Throughput |
|-----------|------------|------------|------------|
| `parseJson` (simple) | 40 bytes | ~500 ns | 80 MB/s |
| `parseJson` (complex) | 100 bytes | ~1.2 µs | 83 MB/s |
| `parseUrl` (simple) | 20 bytes | ~200 ns | 100 MB/s |
| `parseUrl` (complex) | 80 bytes | ~600 ns | 133 MB/s |
| `parseEmail` | 30 bytes | ~150 ns | 200 MB/s |
| `parseUUID` | 36 bytes | ~100 ns | 360 MB/s |

### Escaping Operations

| Operation | Input Size | Time (avg) | Throughput |
|-----------|------------|------------|------------|
| `escapeHtml` (short) | 13 bytes | ~50 ns | 260 MB/s |
| `escapeHtml` (long) | 1.4 KB | ~3 µs | 467 MB/s |
| `urlEncode` (short) | 13 bytes | ~80 ns | 162 MB/s |
| `escapeValue` (SQL) | 8 bytes | ~30 ns | 267 MB/s |

### Validation Operations

| Operation | Time (avg) |
|-----------|------------|
| `isValidUrl` | ~100 ns |
| `isValidEmail` | ~80 ns |
| `isValidUUID` | ~50 ns |
| `containsTraversal` | ~30 ns |
| `detectInjection` (safe) | ~100 ns |
| `detectInjection` (attack) | ~150 ns |

### Encoding Operations

| Operation | Input Size | Time (avg) | Throughput |
|-----------|------------|------------|------------|
| `base64.encode` (short) | 13 bytes | ~40 ns | 325 MB/s |
| `base64.encode` (long) | 1.4 KB | ~2 µs | 700 MB/s |
| `base64.decode` (short) | 20 bytes | ~60 ns | 333 MB/s |

### Regex Operations

| Operation | Pattern Complexity | Time (avg) |
|-----------|-------------------|------------|
| `regex` (simple) | O(n) | ~200 ns |
| `regex` (complex) | O(n) | ~800 ns |
| `detectReDoS` (safe) | - | ~50 ns |
| `detectReDoS` (dangerous) | - | ~100 ns |

### Math Operations

| Operation | Time (avg) |
|-----------|------------|
| `safeAdd` | ~10 ns |
| `safeDiv` | ~15 ns |
| `safeMod` | ~15 ns |

## Comparison with Alternatives

### JSON Parsing

| Library | Language | Time (simple) | Time (complex) |
|---------|----------|---------------|----------------|
| proven | Idris 2 | 500 ns | 1.2 µs |
| serde_json | Rust | 150 ns | 400 ns |
| ujson | Python | 300 ns | 800 ns |
| JSON.parse | JavaScript | 200 ns | 500 ns |

Note: proven includes formal verification overhead but provides stronger safety guarantees.

### Base64

| Library | Language | Encode (1KB) | Decode (1KB) |
|---------|----------|--------------|--------------|
| proven | Idris 2 | 2 µs | 2.5 µs |
| base64 | Rust | 500 ns | 600 ns |
| base64 | Python | 1 µs | 1.2 µs |

### Regex Compilation

| Library | Language | Simple | Complex | ReDoS Protection |
|---------|----------|--------|---------|------------------|
| proven | Idris 2 | 200 ns | 800 ns | ✅ Built-in |
| regex | Rust | 100 ns | 400 ns | ❌ Manual |
| re | Python | 500 ns | 2 µs | ❌ Manual |

## Optimization Notes

1. **Zero-copy parsing**: Where possible, proven avoids allocations
2. **SIMD acceleration**: Available via Zig FFI for bulk operations
3. **Lazy evaluation**: Proofs are evaluated lazily where sound
4. **Inlining**: Hot paths are aggressively inlined

## Memory Usage

| Operation | Peak Memory |
|-----------|-------------|
| `parseJson` (1KB) | ~4 KB |
| `parseJson` (1MB) | ~4 MB |
| `regex` (simple) | ~500 bytes |
| `regex` (complex) | ~2 KB |

## Methodology

- All benchmarks run on isolated CPU cores
- Warm-up iterations excluded from measurements
- Results are median of 10,000+ iterations
- Memory measured with Valgrind massif
