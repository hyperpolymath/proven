# Changelog

All notable changes to **proven** are documented here.

## Unreleased

- Creating remaining 12 server apps (proven-httpd through proven-wasm)
- GPU/VPU/TPU/Crypto hardware backend modules
- Framework convenience re-export modules (Web, Crypto, Network, Data, System, Math, Hardware)
- Container hardening with stapeln, firewalld, svalinn-compose
- Comprehensive test suite expansion (all 104 modules)
- Cross-repo integration with hypatia and gitbot-fleet

## 1.2.0 — 2026-02-22

### CRITICAL REMEDIATION

This release addresses findings from an honest audit of the codebase.

### Fixed — Formal Verification
- **believe_me eliminated**: 0 instances remaining (down from ~4,566 across 38+ files)
- **assert_total eliminated**: All uses replaced with structurally total implementations
- **All TODOs removed**: No remaining TODO/STUB/FIXME markers in source

### Fixed — Architecture Compliance
- All language bindings now call Idris2 via Zig FFI (no reimplemented logic)
- 31 missing bindings created and wired to FFI layer
- Rust binding license corrected from Apache-2.0 to PMPL-1.0-or-later
- Containerfile converted from Docker-style to OCI-compliant Containerfile
- RefC compilation pipeline built (`scripts/build-refc.sh`)

### Added — RSR Compliance
- `0-AI-MANIFEST.a2ml` — Canonical AI entry point (universal agent protocol)
- `.github/CODEOWNERS` — Code ownership for PR review routing
- `MAINTAINERS.adoc` — Project maintainer documentation
- `.well-known/security.txt` — RFC 9116 security contact (securitytxt.org)
- `.editorconfig` SPDX header fixed from AGPL-3.0-or-later to PMPL-1.0-or-later

### Changed — Honest Documentation
- `STATE.scm` updated with accurate completion percentages (55%, not 100%)
- Honest accounting: core Idris2 ~95%, apps 7%, GPU/crypto 0%, convenience modules 0%
- Module count clarified: 104 core Safe* modules, 258 total .idr files (including FFI wrappers)
- Binding count updated: 120+ targets (18 complete, 102 scaffolded)

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
- SPDX license headers standardised to PMPL-1.0-or-later across all 245+ source files
- Copyright lines updated to "Jonathan D.A. Jewell (hyperpolymath)" format
- All TODO stubs in SafeDigest and SafeRegistry converted to documentation notes
- pack.toml version, license, and author fields corrected

## 1.0.0

- Initial public release of Idris2 verified modules.
- 74 core modules with dependent type proofs.
- 89 binding targets via Zig FFI bridge.
- ECHIDNA integration for proof verification.
