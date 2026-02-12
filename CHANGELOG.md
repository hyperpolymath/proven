# Changelog

All notable changes to **proven** are documented here.

## Unreleased

- Transitioning all bindings to **pure Zig ABI wrappers** that call **Idris2** only.
- Removing any non-Idris "safety logic" from language bindings.
- Performance tradeoff acknowledged: Idris-only guarantees over speed.

## 1.1.0

### Added
- 14 new Idris2 modules: SafeShell, SafePipe, SafeProcess, SafeSignal, SafeTerminal, SafeSemaphore, SafeCalculator, SafeCSV, SafeDecimal, SafeRational, SafeComplex, SafeSet, SafeHeap, SafeMatrix (104 total, up from 74)
- 14 new FFI exports in Zig bridge layer for new modules
- 6 additional data structure modules: SafeBitset, SafeInterval, SafeUnionFind (with proofs)

### Fixed
- SafeCapability: proof hole for `capSubsetRefl` filled with correct reflexivity proof
- ECHIDNA/Soundness: fixed soundness proof structure
- SafeFloat: corrected dependent type proofs
- SafeCert FFI: hostname wildcard matching aligned with source module (no-dots-in-prefix check)
- SafeML FFI: replaced `parsePositive` with scalar building blocks
- ipkg: removed duplicate module entries

### Changed
- SPDX license headers standardised to Apache-2.0 across all 245+ source files
- Copyright lines updated to "Jonathan D.A. Jewell (hyperpolymath)" format
- All TODO stubs in SafeDigest and SafeRegistry converted to documentation notes
- pack.toml version, license, and author fields corrected

## 1.0.0

- Initial public release of Idris2 verified modules.
- 74 core modules with dependent type proofs.
- 89 binding targets via Zig FFI bridge.
- ECHIDNA integration for proof verification.
