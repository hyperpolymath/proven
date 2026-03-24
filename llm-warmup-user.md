<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- LLM warmup context — USER level (<200 lines) -->
<!-- Feed this to an LLM before asking questions about proven -->

# proven — User Context

## What it is

A formally verified safety library. Provides safe arithmetic, string handling,
cryptographic primitives, and data validation — all backed by mathematical proofs
in Idris2 with dependent types.

## Architecture (critical to understand)

```
Idris2 source (src/Proven/) → Zig FFI bridge (ffi/zig/) → Language bindings (bindings/)
```

- **Idris2** is the sole computation layer. 258 modules, all total, zero `believe_me`.
- **Zig** bridges Idris2's RefC output to a C ABI shared library (`libproven.so`).
- **Bindings** (120+ languages) are thin FFI wrappers. They never reimplement logic.

## Key files

| Path | Purpose |
|------|---------|
| `src/Proven.idr` | Main re-export module |
| `src/Proven/Core.idr` | Core types and proofs |
| `src/Proven/Safe*.idr` | 104 verified safety modules |
| `src/Proven/FFI/` | 65 FFI wrapper modules |
| `ffi/zig/build.zig` | Zig build config |
| `ffi/zig/src/main.zig` | C ABI exports |
| `proven.ipkg` | Main Idris2 package (258 modules) |
| `proven-ffi.ipkg` | FFI-only package |
| `bindings/` | 120+ language binding targets |

## Build pipeline

```bash
just build          # Full: Idris2 → RefC → Zig → libproven.so
just typecheck      # Verify all 258 modules
just test           # Run Idris2 + Zig FFI tests
just doctor         # Check tool prerequisites
```

Prerequisites: Idris2 (via pack), Zig (via asdf), just.

## What the bindings do

Each binding directory (e.g., `bindings/rust/`, `bindings/rescript/`) contains
a thin wrapper that calls `libproven.so` via FFI. The binding code handles:

1. Data marshaling (language types to/from C types)
2. Error propagation (mapping C error codes to language exceptions)
3. Package metadata (Cargo.toml, package.json, etc.)

Bindings **never** contain safety logic or algorithm implementations.

## Safety guarantees

- All functions are total (no partial functions, no crashes)
- Dependent types enforce preconditions at compile time
- Overflow, underflow, and bounds violations are impossible by construction
- Zero use of `believe_me`, `assert_total`, or `unsafeCoerce`

## Common tasks

| Task | Command |
|------|---------|
| Build from scratch | `just build` |
| Verify types | `just typecheck` |
| Run tests | `just test` |
| Check environment | `just doctor` |
| Security scan | `just scan` |
| Install library | `just install /usr/local` |

## License

PMPL-1.0-or-later. Author: Jonathan D.A. Jewell.

## Ecosystem position

- **Upstream**: Idris2 compiler, Zig toolchain
- **Downstream**: echidna (theorem prover), hypatia (CI/CD), any project needing verified safety
- **Siblings**: panic-attacker (scanning), verisimdb (data)
