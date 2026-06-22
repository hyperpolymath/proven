<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
-->

# Audit: Track C — non-FFI Critical/High `panic-attack assail` findings

**Auditor**: Jonathan D.A. Jewell
**Date**: 2026-06-23
**Scope**: the 33 Critical/High `panic-attack assail` findings aggregated in
[hyperpolymath/proven#68](https://github.com/hyperpolymath/proven/issues/68)
("Track C"), i.e. every Critical/High finding **except** the `UnsafeCode` /
`UnsafeFFI` findings handled by Track A
([audit-ffi-bindings-2026-05-26.md](audit-ffi-bindings-2026-05-26.md), PR #67).
**Cross-reference**: campaign tracker
[hyperpolymath/panic-attack#32](https://github.com/hyperpolymath/panic-attack/issues/32).
**Registry**: `audits/assail-classifications.a2ml`.

## Context

`proven` is the Idris2-implemented library whose logic is **formally verified**
(dependent types + totality checking). Each `bindings/<lang>/` directory is a
*thin wrapper* over the C ABI exposed by `libproven` via the Zig FFI bridge; no
logic is reimplemented in any binding (repo `.claude/CLAUDE.md`, ADR-008).

The `panic-attack assail` detectors used here are substring/keyword pattern
matchers. They fire on the *names* of methods, modules, FFI symbols, and on
words appearing in comments — without semantic understanding of whether the
matched token is an executable call. The triage below reads each flagged site
and records, per file, whether it is a genuine defect (→ fixed at source) or a
false positive (→ classified in the registry, which is a separate file that a
new unsafe block cannot self-suppress).

## Disposition summary

| Category | Count | Disposition |
|---|---|---|
| CommandInjection | 15 | false positive — `binding-wrapper-naming` (registered earlier; verified again here) |
| HardcodedSecret | 6 | false positive — `protocol-type-identifier` (registered earlier; verified again here) |
| DynamicCodeExecution | 5 | false positive — `binding-wrapper-naming` / `generated-code` |
| UnsafeTypeCoercion | 2 | false positive — `legitimate-ffi` (Nim) / `documentation-reference` (Haskell) |
| PanicPath | 1 | false positive — `documentation-reference` (Haskell) |
| UncheckedAllocation | 2 | **1 fixed at source** (`stubs.c`) + 1 false positive (`proven_udf.c`, already NULL-checked) |
| SupplyChain | 2 | resolved — both `flake.nix` files removed in the Nix→Guix migration |
| **Total** | **33** | |

## The one genuine defect — fixed at source

### `bindings/ephapax/src/stubs.c` — UncheckedAllocation (Critical)

Four allocations dereferenced their result without a NULL check:

- `idris_proven_lru_new` — `malloc(sizeof(LRUCache))` then `cache->capacity = …`
- `idris_proven_buffer_new` — `malloc(sizeof(Buffer))` then `buf->capacity = …`,
  and a second `malloc(capacity)` for `buf->data`
- `idris_proven_resource_new_handle` — `malloc(sizeof(ResourceHandle))` then
  `handle->resource_id = …`

Under allocation failure (OOM, or an attacker-influenced large `capacity`) each
would dereference NULL and crash. The file is an explicitly temporary test stub
(`// TODO: Replace with actual Idris2-compiled proven library`), but the
unchecked dereference is a real defect, so it is fixed rather than suppressed.

**Fix**: each pointer-returning constructor now returns `NULL` on allocation
failure, matching the file's existing convention (pointer-returning functions
return a pointer; the partial `Buffer` case frees `buf` before returning `NULL`
to avoid a leak). Callers across the FFI boundary already treat a `NULL` handle
as a failed constructor.

## False positives — classified

### §DynamicCodeExecution — `eval` is a libproven method name (5)

Every flagged site routes the expression to libproven's verified calculator via
the `proven_calculator_eval` C ABI export; no language-level `eval()` of
attacker input occurs.

- `bindings/deno/src/safe_calculator.ts`, `bindings/javascript/src/safe_calculator.js`
  — a method *named* `eval()` whose body calls `symbols.proven_calculator_eval(…)`.
- `bindings/assemblyscript/src/ffi.ts` — an `@external` FFI *declaration* of
  `proven_calculator_eval`; AssemblyScript→Wasm has no `eval()` primitive.
- `bindings/elm/interop/proven-elm-ports.js` — calls `lib.proven_calculator_eval(…)`
  over an Elm port; JS `eval()` is never referenced.
- `build-simple/exec/test_ffi_exports_app/test_ffi_exports.ss` — Idris2 Chez
  backend generated test harness; the `eval` is `blodwen-eval-scheme` in the
  Idris runtime-support prelude, never invoked with runtime input.
  Classification: `generated-code`.

> Naming note (non-blocking): the public `eval()` method on the JS/TS calculator
> bindings is a readability hazard precisely because it tripped this detector. A
> future major-version rename to `evaluate()` would remove the ambiguity. This is
> an API-cosmetic change, not a security issue, and is out of scope for this PR.

### §UnsafeTypeCoercion (2)

- `bindings/nim/src/proven/safe_hex.nim` — `cast[pointer](unsafeAddr a[0])`
  marshals a Nim string buffer to C `void*` at the libproven FFI boundary; each
  cast is length-guarded (`a.len > 0`) with a dummy sentinel for the empty case.
  Idiomatic safe-Nim FFI. Classification: `legitimate-ffi`.
- `bindings/haskell/src/Proven.hs` — the token `unsafeCoerce` appears only in
  the module Haddock docstring asserting its *absence*. No such call exists.
  Classification: `documentation-reference`.

### §PanicPath (1)

- `bindings/haskell/src/Proven.hs` — `error`/`undefined` appear only in the
  same docstring and in error-constructor type names (e.g. `ErrEncodingError`);
  no `error`/`undefined` call exists. Classification: `documentation-reference`.

### §UncheckedAllocation — false positive (1)

- `bindings/sql/mysql/proven_udf.c` — every `malloc` (the `initid->ptr`
  allocations in the `*_init` UDF entry points) is immediately followed by an
  `if (ptr == NULL)` guard that frees any owned string, sets `*is_null = 1` and
  returns, per the MySQL UDF convention. No unchecked dereference.
  Classification: `already-null-checked`.

### §SupplyChain — resolved by file removal (2)

Both flagged files (`flake.nix`, `bindings/nix/flake.nix`) were removed during
the estate Nix→Guix migration (commit `ee62f7e`). Build-time dependency pinning
is now provided by `guix.scm` + `guix/channels.scm`. The surviving
`bindings/nix/default.nix` is a consumer-facing expression and was not flagged
for unpinned flake inputs. Classification: `resolved-file-removed`. (Per estate
policy Nix is not used for `proven`'s own build; the `bindings/nix/` directory
remains only as a binding offered *to* Nix consumers of `libproven`.)

### §CommandInjection (15) and §HardcodedSecret (6)

Re-verified during this pass; rationale unchanged from
[audit-ffi-bindings-2026-05-26.md](audit-ffi-bindings-2026-05-26.md)
(§CommandInjection, §HardcodedSecret) and already present in the registry.
`eval`/`system`/`escape_shell` are method names, the Guile module path
`(system foreign)`, and a sanitiser predicate name respectively; the
`HardcodedSecret` sites are protocol/type identifiers and a common-password
*dictionary* (used by the strength checker), not credential literals.

## Anti-gameability

Identical to the Track A audit: the registry
(`audits/assail-classifications.a2ml`) is a separate file from any scanned
source. Adding a new `eval`/`malloc`/`cast` site inside a binding cannot
self-suppress; a reviewable registry edit plus an entry in this audit doc is
required. The one genuine defect was fixed in source, not suppressed, so it will
re-fire if regressed.

## Upstream note (panic-attack)

The recurring root cause of this noise is that the `DynamicCodeExecution`,
`CommandInjection`, `PanicPath` and `UnsafeTypeCoercion` detectors match the
*substring* (`eval`, `system`, `error`, `unsafeCoerce`) rather than an AST call
node, so they fire on method names, FFI symbol names, module paths and comments.
A foundational reduction in false positives belongs in the scanner — tracked at
hyperpolymath/panic-attack#32 — and is out of scope for this repository's PR.

Refs hyperpolymath/proven#68, hyperpolymath/panic-attack#32.
