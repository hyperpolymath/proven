<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
-->

# Audit: FFI binding `unsafe` blocks across `bindings/` and `ffi/`

**Auditor**: Jonathan D.A. Jewell
**Date**: 2026-05-26
**Scope**: all `panic-attack assail` Critical/High `UnsafeCode` + `UnsafeFFI` findings located under `bindings/*/src/`, `ffi/*/src/`, and `domain-specific/*/ffi/*/src/`.
**Cross-reference**: campaign tracker [hyperpolymath/panic-attack#32](https://github.com/hyperpolymath/panic-attack/issues/32).
**Registry**: `audits/assail-classifications.a2ml`.

## Context

`proven` is the Idris2-implemented library whose logic is **formally verified** (dependent types + totality checking). Each binding-language directory (`bindings/<lang>/src/`) is a *thin wrapper* over the C ABI exposed by `libproven` via the Zig FFI bridge (`ffi/zig/`). Wrappers do exactly three things:

1. Declare `extern "C"` FFI signatures matching `libproven.h`.
2. Provide safe-language wrappers around the unsafe FFI calls (allocating buffers, mapping `ProvenStatus` to language-native errors).
3. Free allocated strings via `proven_free_string` to satisfy the ABI's ownership contract.

**No logic is reimplemented in any binding.** Every function calls through to the formally-verified Idris2 implementation. The `unsafe` blocks (or equivalent in each language) exist solely at the C-ABI boundary, where they are required by the language to call across.

## Per-binding rationale

The classifications cover the following binding/ffi roots:

- `bindings/ada/src/` — Ada `pragma Interface (C)` callouts. The `.adb` carries the FFI shim only.
- `bindings/c/` — direct C consumer of `libproven.h`; not unsafe per Rust semantics but flagged by the same detector.
- `bindings/elixir/` — Erlang NIF + Port wrappers.
- `bindings/ephapax-affine/src/` — AffineScript binding compiled to Rust; unsafe blocks at the `extern "C"` boundary.
- `bindings/ephapax-linear/src/` — Rust linear-types binding to `libproven`; same FFI pattern as `bindings/rust/`.
- `bindings/go/` — cgo wrappers.
- `bindings/haskell/` — `foreign import ccall` FFI.
- `bindings/rust/src/` — canonical Rust binding; every `unsafe` block has an inline `// SAFETY:` comment and calls a `proven_*` C function.
- `bindings/sml/src/` — SML/NJ FFI to the `libproven` shared object.
- `bindings/vcl/` — Varnish-VCL binding (HTTP edge use case).
- `bindings/zig/src/` — Zig direct binding (also acts as the canonical reference for the FFI shape).
- `ffi/beam/src/proven_nif.zig` — BEAM NIF in Zig; bridges Erlang to `libproven`.
- `domain-specific/http/ffi/zig/src/http.zig` — http-specific Zig FFI bridge.

## Anti-gameability

The registry is `audits/assail-classifications.a2ml` — a separate file from any binding source under scan. Adding a new `unsafe` block inside `bindings/<lang>/src/` cannot self-suppress; the registry edit + an update to a section in this audit doc is required, both of which are reviewable.

The classification is **scoped to FFI-boundary files**. Any `unsafe` block outside `bindings/*/src/`, `ffi/*/src/`, or `domain-specific/*/ffi/*/src/` remains visible to assail and will be flagged unsuppressed.

## Verification

Locally on this branch: `panic-attack assail . --headless` reports `UnsafeCode: 0 active, 88 suppressed` and `UnsafeFFI: 0 active, 41 suppressed` after the registry is loaded. The non-FFI findings (CommandInjection, HardcodedSecret, DynamicCodeExecution, SupplyChain, etc.) remain visible and will be handled in separate follow-up PRs.

## Out of scope this audit

- `CommandInjection` (15 findings) — separate triage; mostly tooling/build scripts.
- `HardcodedSecret` (6 findings) — separate triage; mostly test fixtures.
- `DynamicCodeExecution` (5 findings) — separate triage.
- `SupplyChain`, `UnsafeTypeCoercion`, `UncheckedAllocation`, `PanicPath` — separate triage.

Refs hyperpolymath/panic-attack#32.
