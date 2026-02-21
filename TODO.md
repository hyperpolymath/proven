# Proven - Task Status

Last updated: 2026-02-12

## Completed

- [x] 104 Idris2 core modules with dependent type proofs
- [x] 89 binding targets via Zig FFI bridge
- [x] 14 new FFI exports for Zig bridge layer (v1.1.0)
- [x] SPDX headers standardised to PMPL-1.0-or-later
- [x] Copyright lines corrected across all files
- [x] Proof fixes (SafeCapability, ECHIDNA/Soundness, SafeFloat)
- [x] SafeCert FFI hostname matching aligned with source module
- [x] SafeML FFI parsePositive replaced with scalar building blocks
- [x] ipkg duplicate module entries cleaned up
- [x] TODO stubs converted to documentation notes (SafeDigest, SafeRegistry)
- [x] pack.toml version, license, and author fields corrected

## FFI Bindings (18 complete)

- [x] Python, Rust, JavaScript, Deno, ReScript, Gleam, Julia, Swift
- [x] Kotlin, Go, Elixir, Zig native, Lua, Ruby, Nim, OCaml, Haskell, Ada

## Remaining Work

### High Priority
- [ ] Wire FFI stubs to actual Idris2 RefC compiled output (currently stubs return placeholders)
- [ ] Run `idris2 --build proven.ipkg` end-to-end compilation test
- [ ] Fill remaining `believe_me` instances (~283 across 38 files) with actual proofs where feasible
- [ ] Registry publishing: crates.io, PyPI, npm, JSR, opam, pack

### Medium Priority
- [ ] Post-quantum crypto module (Dilithium, Kyber) in SafeCrypto
- [ ] WebAssembly browser bundle
- [ ] IDE plugins (VS Code, IntelliJ)
- [ ] Additional binding targets: PHP, Perl, C, C++, Shell/Bash

### Lower Priority
- [ ] Formal methods workshops and tutorials
- [ ] Hardware security module (HSM) support
- [ ] FIPS 140-3 certification path
