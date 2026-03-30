# PROOF-NEEDS.md — proven

## Current State

- **src/abi/*.idr**: YES — 80+ Idris2 ABI files covering Safe* types (SafeCrypto, SafeEmail, SafeMath, SafePath, SafeNetwork, SafeSQL, SafeSSH, SafeJWT, SafeHTTP, etc.)
- **Dangerous patterns**: 0 (1 reference is documentation in SafeFiniteField.idr explaining how it eliminates need for believe_me)
- **LOC**: ~33,900 (Idris2 + Zig FFI + Rust/Gleam/Go/Haskell/etc. bindings)
- **Existing proofs**: Multiple `Proofs.idr` files — SafeCrypto/Proofs.idr, SafeEmail/Proofs.idr, SafeMath/Proofs.idr, SafeNetwork/Proofs.idr, SafePassword/Proofs.idr, SafePath/Proofs.idr, SafeString/Proofs.idr, SafeBase64/Proofs.idr, SafeYAML/Proofs.idr, SafeArgs/Proofs.idr
- **ABI layer**: Comprehensive — the most complete Idris2 proof infrastructure in the ecosystem

## What Needs Proving

| Component | What | Why |
|-----------|------|-----|
| Remaining Safe* modules without Proofs.idr | SafeSQL, SafeSSH, SafeJWT, SafeHTTP, SafeCORS, SafeCSP, SafeHeader, SafeStateMachine, SafeRegistry, SafeBitset, SafeSet, SafeFiniteField, SafePromptInjection, SafeRedirect, SafeHtml, SafeCommand, SafeRational, SafeInterval, SafeSemaphore | ~30 modules lack dedicated proof files |
| Zig FFI integration proofs | Prove Zig FFI layer faithfully implements Idris2 ABI | FFI boundary is where type safety can be lost |
| Binding correctness | Rust, Gleam, Go, Haskell, etc. bindings correctly wrap proven types | Wrong binding defeats the purpose of proven types |
| SafePromptInjection completeness | Injection detection covers all known attack vectors | LLM prompt injection is a rapidly evolving threat |
| SafeSQL injection proofs | SQL construction prevents all injection attacks | SQL injection is the classic web vulnerability |

## Recommended Prover

**Idris2** — This IS the Idris2 proof library. Continue adding `Proofs.idr` files for the ~30 modules that lack them. The existing proof infrastructure is the template.

## Priority

**HIGH** — proven is the foundational safety library used by the entire ecosystem. Every Safe* module without proofs is a gap in the formal verification chain. The existing proof files demonstrate the pattern — the remaining modules need the same treatment.
