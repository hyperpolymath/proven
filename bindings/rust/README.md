# Rust Bindings for proven (Idris2 Formal Verification Library)

> ⚠️ **ARCHITECTURE CHANGE (2026-01-25)**: This directory now contains ONLY FFI wrappers that call verified Idris2 code. All Rust reimplementations have been removed.

## What is proven?

**proven** is a formally verified safety library using **Idris2 dependent types and totality checking**.

- ✅ Core implementation: **Idris2** (`../../src/Proven/*.idr`)
- ✅ FFI bridge: **Zig** (`../../ffi/zig/`)
- ✅ Language bindings: Thin wrappers (this directory)

**All computation happens in Idris2. This is not negotiable.**

## Architecture Flow

```
Rust Application
      ↓
   Rust Binding (this directory - FFI wrapper ONLY)
      ↓
   Zig FFI Bridge (../../ffi/zig/)
      ↓
   Idris2 RefC Compiled Output
      ↓
   Idris2 Source Code (../../src/Proven/*.idr) ← MATHEMATICAL PROOFS HERE
```

## What Belongs in This Directory

✅ **FFI declarations** - Calling `ffi/zig/` C ABI functions
✅ **Type conversions** - Rust types ↔ C ABI types
✅ **Error handling wrappers** - C errors → Rust `Result<T, E>`
✅ **Rust-idiomatic API** - Builder patterns, iterators, etc. (wrapping FFI calls)

## What Does NOT Belong Here

❌ **Logic/algorithms** - These belong in Idris2 (`../../src/Proven/`)
❌ **`safe_*.rs` reimplementations** - DELETED on 2026-01-25
❌ **Unsafe patterns** - `unwrap()`, `expect()`, `panic!()`
❌ **Mathematical operations** - Let Idris2 prove they're correct

## Why This Architecture?

### The Problem (What We Had Before)

```rust
// ❌ WRONG: Rust reimplementation (what we deleted)
pub fn safe_add(a: i32, b: i32) -> Result<i32> {
    a.checked_add(b).ok_or(Error::Overflow)  // NOT VERIFIED!
}
```

**Issues:**
- No formal proof this is correct
- Can have bugs (and did - see opam rejection)
- Defeats the entire purpose of "formally verified"

### The Solution (What We Have Now)

```rust
// ✅ CORRECT: FFI wrapper calling verified Idris2 code
use crate::ffi;

pub fn safe_add(a: i32, b: i32) -> Result<i32, Error> {
    unsafe {
        let result = ffi::proven_safe_math_add(a, b);
        if result.is_error {
            Err(Error::from_c(result.error_code))
        } else {
            Ok(result.value)
        }
    }
}
```

**Benefits:**
- Calls Idris2 code with **mathematical proof** it cannot crash
- FFI wrapper is simple (hard to get wrong)
- Single source of truth (Idris2)
- Same verification for all 89 language bindings

## Deleted Files (2026-01-25 Cleanup)

**38 `safe_*.rs` files removed:**

These were Rust reimplementations that bypassed formal verification and contained unsafe patterns (`unwrap()`, `expect()`, etc.).

**Backup location:** `../../.UNSAFE-RUST-DELETED-20260125/`

**DO NOT restore these files.** They violated ADR-008 and ADR-009 in `../../META.scm`.

## Installation (Future State)

Once FFI bindings are properly implemented:

```toml
[dependencies]
proven = "1.0"  # Calls verified Idris2 code via FFI
```

## Current Status

🚧 **Under reconstruction** - FFI layer being built

- ✅ Unsafe reimplementations deleted
- 🔄 FFI bridge implementation in progress
- 🔄 Rust wrapper generation in progress

## For Rust-Native Safe Code

If you want Rust implementations (not verified by Idris2):

- Create a **separate repository**
- Do NOT name it "proven" (that name is reserved for Idris2-verified code)
- Do NOT put it in any hyperpolymath repo with "proven" in the name

The "proven" library is about **mathematical proof via Idris2 dependent types**, not Rust safety patterns.

## References

- Idris2 source: `../../src/Proven/`
- Architecture decisions: `../../META.scm` (see ADR-008, ADR-009)
- FFI bridge: `../../ffi/zig/`
- Project instructions: `../../.claude/CLAUDE.md`

## License

Apache-2.0

Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
