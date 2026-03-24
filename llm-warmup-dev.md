<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- LLM warmup context — DEVELOPER level (<400 lines) -->
<!-- Feed this to an LLM before doing development work on proven -->

# proven — Developer Context

## Architecture: The Iron Rule

**All computation happens in Idris2. No exceptions.**

```
src/Proven/           Idris2 with dependent types — THE TRUTH
    |
    | idris2 --codegen refc
    v
build/refc/           Generated C code (RefC backend)
    |
    | zig build (links RefC output + Idris2 runtime)
    v
ffi/zig/zig-out/lib/  libproven.so / libproven.dylib / libproven.dll
    |
    | Language-specific FFI calls
    v
bindings/*/           Thin wrappers — data marshaling ONLY
```

## Idris2 Module Structure

The `proven.ipkg` declares 258 modules. Key categories:

### Core (src/Proven/)
- `Core.idr` — Fundamental types, `ProvenResult`, `SafeNat`, `BoundedInt`
- `Axioms.idr` — Axioms (should be empty — everything proved)

### Safe* modules (104 modules)
Pattern: `src/Proven/Safe<Domain>.idr`
- `SafeArith.idr` — Overflow-proof arithmetic
- `SafeString.idr` — Length-bounded string operations
- `SafeCrypto.idr` — Cryptographic primitives
- `SafeBuffer.idr` — Bounds-checked buffers
- `SafeParse.idr` — Total parsing (always terminates)
- `SafeNet.idr` — Network operations
- `SafeFS.idr` — Filesystem operations
- etc.

Each Safe* module exports functions with dependent type signatures that make
invalid states unrepresentable. For example:
```idris
safeAdd : (a : Int) -> (b : Int) -> {auto prf : NoOverflow a b} -> Int
```

### FFI Wrappers (65 modules in src/Proven/FFI/)
These declare the C-ABI-compatible function signatures that Zig exports.
Pattern: `src/Proven/FFI/<Domain>FFI.idr`

### ECHIDNA Integration (src/Proven/ECHIDNA/)
Neurosymbolic proof validation hooks for the echidna theorem prover.

## Zig FFI Bridge (ffi/zig/)

`ffi/zig/build.zig` accepts these build options:
- `-Didris-refc=<path>` — Path to RefC output directory
- `-Didris-refc-runtime=<path>` — Path to Idris2 RefC runtime headers
- `-Didris-c-support=<path>` — Path to Idris2 C support headers
- `-Didris-support-lib=<path>` — Path to libidris2_support

`ffi/zig/src/main.zig` exports C-ABI functions that:
1. Accept C-compatible arguments
2. Call the corresponding Idris2 RefC compiled function
3. Return C-compatible results

The Zig layer adds NO safety logic. It is purely structural bridging.

## Bindings Rules (CRITICAL)

### What bindings MUST do:
- Call `libproven` functions via FFI
- Marshal data between language types and C types
- Propagate errors from C return codes
- Include SPDX headers and author attribution

### What bindings MUST NOT do:
- Reimplement any algorithm (even "simple" ones)
- Use unsafe patterns: `unwrap()`, `getExn`, `Obj.magic`, `panic!()`
- Claim "formally verified" for code that does not call Idris2

### Pre-submission validation (MANDATORY):
```bash
./hypatia-v2 . --severity=critical --severity=high --exclude=bindings/*/tests
# Must show 0 findings
```

### Lesson from v0.9.0 rejection:
130 unsafe patterns found in bindings (5 CRITICAL, 124 HIGH). ReScript used
`getExn`, Rust used `unwrap()`. This blocked opam publication. Never again.

## Build Commands

```bash
just build          # Full pipeline (2-5 min)
just build-refc     # Step 1: Idris2 → RefC C code
just build-ffi      # Step 2: Zig links RefC → libproven.so
just typecheck      # Typecheck all 258 modules
just typecheck-ffi  # Typecheck FFI-only package
just verify-totality # Run totality checker
just test           # All tests (Idris2 + Zig FFI)
just test-idris     # Idris2 tests only
just test-ffi       # Zig FFI integration tests
just scan           # hypatia security scan
just env-info       # Show Idris2/Zig/pack versions
just doctor         # Check prerequisites
just heal           # Install instructions
```

## .ipkg files

| File | Purpose |
|------|---------|
| `proven.ipkg` | Full library (258 modules) |
| `proven-ffi.ipkg` | FFI-only subset |
| `core-only.ipkg` | Core types only |
| `test-simple.ipkg` | Test suite |
| `test-ffi.ipkg` | FFI integration tests |

## Machine-Readable Metadata

All in `.machine_readable/6a2/`:
- `STATE.a2ml` — Current progress, blockers, next actions
- `META.a2ml` — Architecture decisions (ADR-008: FFI-only bindings)
- `ECOSYSTEM.a2ml` — Ecosystem position and relationships
- `AGENTIC.a2ml` — AI agent interaction patterns
- `NEUROSYM.a2ml` — Neurosymbolic config
- `PLAYBOOK.a2ml` — Operational runbook

**NEVER** create these files in the root directory.

## Adding a New Safe* Module

1. Create `src/Proven/SafeNewDomain.idr` with total functions
2. Add FFI signatures in `src/Proven/FFI/NewDomainFFI.idr`
3. Add module to `proven.ipkg` and `proven-ffi.ipkg`
4. Run `just typecheck` — must pass with zero warnings
5. Add Zig export in `ffi/zig/src/main.zig`
6. Run `just build && just test`
7. Add bindings in `bindings/*/` (thin wrappers only)
8. Run hypatia scan before committing

## Adding a New Binding

1. Create directory `bindings/<lang>/`
2. Add package metadata (Cargo.toml, package.json, etc.)
3. Write FFI calls to `libproven` — NO reimplementation
4. Add tests that verify FFI calls work
5. Run `./hypatia-v2 bindings/<lang>/ --severity=critical --severity=high`
6. Must show 0 findings before PR

## Banned Patterns

In Idris2:
- `believe_me` — BANNED (breaks proofs)
- `assert_total` — BANNED (hides partiality)
- `unsafePerformIO` — BANNED

In bindings:
- Rust: `unwrap()`, `expect()`, `panic!()`, `unsafe` (except FFI boundary)
- ReScript: `getExn`, `Obj.magic`
- Any pattern hypatia flags as HIGH or CRITICAL

## CI/CD

17 workflows in `.github/workflows/`. Key ones:
- `hypatia-scan.yml` — Security scanning
- `codeql.yml` — Code analysis
- `quality.yml` — TruffleHog, EditorConfig
- `mirror.yml` — GitLab/Bitbucket mirroring

## License

PMPL-1.0-or-later throughout.
Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
Git: 6759885+hyperpolymath@users.noreply.github.com
