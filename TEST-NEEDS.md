# TEST-NEEDS.md — proven

> Generated 2026-03-29 by punishing audit.
> Updated 2026-04-04: CRG Grade C blitz — P2P + E2E + aspect tests added.

## CRG Grade: C — ACHIEVED 2026-04-04

**Test count after blitz:** 54 tests pass in Rust binding (up from 8)

| Category    | Module                                | Count | Status   |
|-------------|---------------------------------------|-------|----------|
| Unit        | `core::tests`                         | 8     | PASS     |
| P2P         | `tests::p2p` (proptest)               | 16    | PASS     |
| E2E         | `tests::e2e` (structural/reflexive)   | 10    | PASS     |
| Aspect      | `tests::aspect` (security/boundary)   | 20    | PASS     |
| **Total**   |                                       | **54**| **PASS** |

### P2P Tests Added (`bindings/rust/src/tests/p2p.rs`)

Property-based tests using `proptest`:
- `prop_bounded_percentage_in_range` — any i64 in [0,100] accepted by Bounded<0,100>
- `prop_bounded_percentage_out_of_range` — any i64 outside [0,100] produces OutOfBounds
- `prop_bounded_port_valid` — Bounded<0,65535> accepts full port range
- `prop_bounded_byte_valid` — Bounded<0,255> accepts full byte range
- `prop_non_empty_from_nonempty_vec` — NonEmpty preserves length invariant
- `prop_non_empty_from_empty_vec_is_none` — NonEmpty rejects empty input
- `prop_non_empty_roundtrip` — from_vec → to_vec preserves all elements
- `prop_status_ok_is_ok` — STATUS_OK always maps to Ok
- `prop_nonzero_status_is_err` — any non-zero status produces Err
- `prop_int_result_ok_preserves_value` — OK IntResult forwards value unchanged
- `prop_int_result_overflow_is_err` — STATUS_ERR_OVERFLOW always maps to Error::Overflow
- `prop_int_result_underflow_is_err` — STATUS_ERR_UNDERFLOW always maps to Error::Underflow
- `prop_int_result_div_zero_is_err` — STATUS_ERR_DIVISION_BY_ZERO always maps correctly
- `prop_error_display_never_empty` — Display never produces empty string for any variant
- `prop_error_clone_equals_original` — Clone/PartialEq consistency
- `prop_status_to_result_is_deterministic` — same code always yields same variant

### E2E Tests Added (`bindings/rust/src/tests/e2e.rs`)

Structural/reflexive end-to-end tests (no libproven.so required):
- `e2e_ffi_status_constants_are_defined` — all 12 status constants have correct values
- `e2e_full_status_to_error_chain` — every error code maps to the correct Error variant
- `e2e_int_result_chain_ok` — IntResult OK chain: status=0, value=42 → Ok(42)
- `e2e_int_result_chain_errors` — IntResult error chain: overflow/underflow/div-zero
- `e2e_ffi_struct_sizes_match_c_abi` — IntResult=16B, BoolResult=5–8B, FloatResult=16B
- `e2e_type_aliases_exported` — Bounded/NonEmpty accessible from crate root
- `e2e_error_implements_std_error` — Error is boxable as dyn std::error::Error
- `e2e_error_display_messages` — all variants have meaningful display strings
- `e2e_bounded_const_bounds` — MIN/MAX const accessors are correct
- `e2e_int_result_alignment` — IntResult has 8-byte alignment (C ABI match)

### Aspect Tests Added (`bindings/rust/src/tests/aspect.rs`)

Security, boundary, and concurrency aspects:
- `aspect_bounded_i64_max_is_rejected` — i64::MAX rejected by Percentage
- `aspect_bounded_i64_min_is_rejected` — i64::MIN rejected by any non-negative type
- `aspect_bounded_minus_one_is_rejected` — -1 rejected by Port [0,65535]
- `aspect_bounded_65536_exceeds_port_range` — 65536 rejected by Port
- `aspect_bounded_zero_is_valid` — 0 is valid lower boundary
- `aspect_bounded_100_is_valid` — 100 is valid upper boundary for Percentage
- `aspect_bounded_101_exceeds_percentage` — 101 is rejected
- `aspect_ffi_null_pointer_is_error` — STATUS_ERR_NULL_POINTER never succeeds
- `aspect_ffi_int_result_null_pointer_discards_value` — error value field is ignored
- `aspect_overflow_and_underflow_are_distinct` — two distinct error variants
- `aspect_div_zero_is_distinct_from_overflow` — div-zero not confused with overflow
- `aspect_ffi_allocation_failed_is_error` — allocation failures always produce Err
- `aspect_not_implemented_status_maps_correctly` — -99 maps to NotImplemented
- `aspect_unknown_status_codes_produce_error` — gaps in code space → Error::Unknown
- `aspect_int_result_error_value_is_discarded` — value field ignored for all error codes
- `aspect_error_variants_are_distinct` — no two variants accidentally equal
- `aspect_error_is_send_sync` — Error: Send + Sync
- `aspect_result_is_send_sync` — Result<i64>: Send + Sync
- `aspect_bounded_get_always_in_range` — get() always within [MIN,MAX]
- `aspect_only_zero_is_success` — STATUS_OK=0 is the unique success code

---

## Original State (Pre-Blitz)

| Category     | Count | Notes |
|-------------|-------|-------|
| Unit tests   | ~15   | Zig FFI integration_test, Go proven_test, Gleam proven_test, Elixir proven_test, Lua proven_spec, Ruby proven_spec, Nim test_proven, OCaml test_proven, Ada test_proven, Ephapax tests (test_all.zig, test_simple.c), Python test_benchmarks |
| Integration  | 1     | Zig FFI integration test |
| E2E          | 0     | None |
| Benchmarks   | 5     | Rust benches (benchmarks.rs, safe_math.rs), JavaScript benchmark.mjs, Python test_benchmarks.py, benchmarks.ipkg (Idris2) |

**Source modules:** ~1005 across Idris2 core (312 .idr files), Zig FFI (91 files), Rust (76 files), and 100+ language bindings. Bindings span ~120 languages from Ada to Zsh.

## Remaining Gaps (Post-Blitz)

### Runtime P2P Tests (require libproven.so)
- [ ] Safe math overflow: `SafeMath::add(i64::MAX, 1)` → `Err(Overflow)`
- [ ] Safe math underflow: `SafeMath::sub(i64::MIN, 1)` → `Err(Underflow)`
- [ ] Safe div-zero: `SafeMath::div(1, 0)` → `Err(DivisionByZero)`
- [ ] Safe abs of MIN: `SafeMath::abs(i64::MIN)` → `Err(Overflow)`

### Full Proof-Chain E2E (require Idris2 build + libproven.so)
- [ ] Idris2 spec → Zig FFI → Rust binding → verification roundtrip
- [ ] Multi-language consistency: Rust + Gleam + Elixir agree on all operations

### Cross-Binding Coverage
- [ ] 89% of bindings (109/120) still untested
- [ ] Cross-binding consistency: same operation in 120 bindings produces identical results

### Benchmarks Needed
- [ ] Zig FFI call overhead
- [ ] Cross-binding comparison (all 120 languages, same operations)
- [ ] Compilation time benchmarks

### Self-Tests
- [ ] All 312 Idris2 proofs type-check (CI gate)
- [ ] FFI exports match Idris2 specifications
- [ ] Binding API surface matches core API

## Priority

**CRG Grade C ACHIEVED** for the Rust binding via structural and property-based tests. The next step toward Grade B+ is runtime integration tests (requiring `libproven.so` from a full Zig+Idris2 build) and expanding coverage to other language bindings (currently 9.2% coverage across 120 languages).
