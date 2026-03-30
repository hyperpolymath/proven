# TEST-NEEDS.md — proven

> Generated 2026-03-29 by punishing audit.

## Current State

| Category     | Count | Notes |
|-------------|-------|-------|
| Unit tests   | ~15   | Zig FFI integration_test, Go proven_test, Gleam proven_test, Elixir proven_test, Lua proven_spec, Ruby proven_spec, Nim test_proven, OCaml test_proven, Ada test_proven, Ephapax tests (test_all.zig, test_simple.c), Python test_benchmarks |
| Integration  | 1     | Zig FFI integration test |
| E2E          | 0     | None |
| Benchmarks   | 5     | Rust benches (benchmarks.rs, safe_math.rs), JavaScript benchmark.mjs, Python test_benchmarks.py, benchmarks.ipkg (Idris2) |

**Source modules:** ~1005 across Idris2 core (312 .idr files), Zig FFI (91 files), Rust (76 files), and 100+ language bindings. Bindings span ~120 languages from Ada to Zsh.

## What's Missing

### P2P (Property-Based) Tests
- [ ] Safe math: property tests for overflow prevention across all integer sizes
- [ ] Proven core: property tests for each verified property (the proofs type-check, but do the implementations match?)
- [ ] Cross-binding consistency: same operation in 120 bindings produces identical results

### E2E Tests
- [ ] Full proof chain: Idris2 specification -> Zig implementation -> language binding -> verification
- [ ] Binding roundtrip: for each of 120 bindings, call proven -> verify result -> compare
- [ ] Multi-language integration: Rust + JavaScript + Python all calling same proven functions and agreeing

### Aspect Tests
- **Security:** No tests for integer overflow in bindings (the whole point is preventing this), FFI boundary safety across 120 languages
- **Performance:** Benchmarks exist for Rust, JS, Python (good). Missing: Zig FFI overhead, Idris2 compile time, binding call overhead comparison across all languages
- **Concurrency:** No tests for thread-safe proven operations across language bindings
- **Error handling:** No tests for out-of-range inputs, unsupported operations, binding version mismatch

### Build & Execution
- [ ] Idris2 compilation of 312 .idr files
- [ ] Zig FFI tests
- [ ] Language binding tests (currently: 11 of 120+ tested = 9%)

### Benchmarks Needed (Existing + Missing)
- [x] Rust safe_math benchmarks (EXISTS, real)
- [x] JavaScript benchmarks (EXISTS)
- [x] Python benchmarks (EXISTS)
- [x] Idris2 benchmarks.ipkg (EXISTS)
- [ ] Zig FFI call overhead
- [ ] Cross-binding comparison (all 120 languages, same operations)
- [ ] Compilation time benchmarks

### Self-Tests
- [ ] All 312 Idris2 proofs type-check
- [ ] FFI exports match Idris2 specifications
- [ ] Binding API surface matches core API

### CRITICAL GAPS

| Area | Count | Tested | Coverage |
|------|-------|--------|----------|
| Idris2 core proofs | 312 | 0 unit tests (type-checked only) | Type-check only |
| Zig FFI | 91 | 1 integration | **1.1%** |
| Language bindings | ~120 | 11 | **9.2%** |
| Rust bindings | 76 | 2 bench files | Bench only |

## Priority

**CRITICAL.** The crown jewel of formally verified code has 312 Idris2 proofs that type-check (which IS a form of verification) but only 11 of 120+ language bindings are tested. The Idris2 type-checking provides strong guarantees for the core, but the bindings are where bugs hide — the translation from proven Idris2 to each target language. 91% of bindings are unverified. Benchmarks are a bright spot with 5 real benchmark files across 4 languages.
