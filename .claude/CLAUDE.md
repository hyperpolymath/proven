# Claude Instructions for proven Repository

## CRITICAL: Architecture Principle

**proven** is a formally verified safety library. This means:

1. ✅ **Core implementation**: Idris 2 with dependent types and totality checking
2. ✅ **FFI layer**: Zig bridges Idris to C ABI
3. ✅ **Language bindings**: Thin wrappers that **CALL** Idris via FFI

## What You MUST NOT Do

❌ **NEVER write language binding code that reimplements logic**
- Bindings in `bindings/rust/`, `bindings/rescript/`, `bindings/python/`, etc.
- These must ONLY wrap Idris FFI calls, NOT reimplement algorithms

❌ **NEVER use unsafe patterns in bindings**
- Rust: No `unwrap()`, `expect()`, `panic!()`
- ReScript: No `getExn`, `Obj.magic`
- Any pattern hypatia scanner flags as HIGH or CRITICAL

❌ **NEVER claim code is "verified" or "proven safe" if it doesn't call Idris**

## What You MUST Do

✅ **All computation happens in Idris**
- Bindings call `ffi/zig/` functions
- Zig calls compiled Idris RefC code
- Only data marshaling happens in bindings

✅ **Pre-submission validation**
- Run: `./hypatia-v2 bindings/ --severity=critical --severity=high`
- MUST show 0 findings before external publication
- Run: `./pre-submit.sh` before submitting to opam, crates.io, npm, etc.

✅ **Keep META.scm ADRs updated**
- Architecture decisions go in `META.scm`
- Reference ADR-008 (FFI-only bindings) when writing binding code

## Author Attribution

**ALL files must use:**
```
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
SPDX-License-Identifier: PMPL-1.0-or-later
```

**NEVER use:**
- "Hyperpolymath" alone as author (must be "Jonathan D.A. Jewell (hyperpolymath)")
- Apache-2.0 license (this project uses PMPL-1.0-or-later)
- Missing or incorrect email addresses

## File Structure

```
proven/
├── src/                    # Idris 2 source (THE TRUTH)
│   ├── Proven.idr          # Main module
│   └── Proven/             # Submodules with proofs
├── ffi/zig/                # Zig FFI bridge layer
│   ├── src/main.zig        # Wraps Idris RefC output
│   └── build.zig           # Compiles Idris + Zig to C ABI
├── bindings/               # Language-specific thin wrappers
│   ├── rust/               # Calls ffi/zig via FFI
│   ├── rescript/           # Calls ffi/zig via FFI
│   ├── python/             # Calls ffi/zig via FFI
│   └── ...                 # All 89 targets
└── docs/                   # Documentation
```

## Pre-Submission Checklist

Before submitting to **any** external repository:

1. ✅ Run hypatia scan: `./hypatia-v2 . --severity=critical --severity=high`
   - Expected: 0 findings (excluding test code)
2. ✅ Verify license headers: All files have PMPL-1.0-or-later SPDX
3. ✅ Author attribution: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
4. ✅ Tests pass: `dune test` or language-specific test command
5. ✅ Build succeeds: `idris2 --build proven.ipkg`
6. ✅ Documentation updated: README reflects current state

## Common Mistakes (DO NOT REPEAT)

### Mistake 1: Reimplementing in bindings
```rust
// ❌ WRONG: Reimplemented in Rust
pub fn safe_add(a: i32, b: i32) -> Result<i32> {
    a.checked_add(b).ok_or(Error::Overflow)
}

// ✅ CORRECT: Calls Idris via FFI
pub fn safe_add(a: i32, b: i32) -> Result<i32> {
    unsafe {
        proven_ffi_safe_add(a, b)  // Calls Zig FFI → Idris
    }
}
```

### Mistake 2: Using unwrap/getExn
```rescript
// ❌ WRONG: Can crash
let fields = Belt.Array.getExn(parts, 0)

// ✅ CORRECT: Safe pattern matching
switch parts {
| [field1, field2, ...] => // Handle
| _ => Error(InvalidInput)
}
```

### Mistake 3: Submitting without validation
```bash
# ❌ WRONG: Direct submission
opam publish

# ✅ CORRECT: Validate first
./hypatia-v2 . --severity=critical --severity=high
dune test
opam publish
```

## Lesson Learned (2026-01-25)

**proven v0.9.0 was REJECTED from opam-repository** because:
- Bindings had 130 unsafe patterns (5 CRITICAL, 124 HIGH)
- ReScript SafeCron used `getExn` that crashes on malformed input
- Rust safe_*.rs files used `unwrap()` despite claiming "safe"
- Code claimed "formally verified" but bypassed verification

**This MUST NOT happen again.**

## CI/CD Integration

Add to `.github/workflows/security-scan.yml`:
```yaml
- name: Hypatia Security Scan
  run: |
    ./hypatia-v2 . --severity=critical --severity=high --exclude=bindings/*/tests
    if [ $? -ne 0 ]; then
      echo "❌ Security issues found - fix before merging"
      exit 1
    fi
```

## Questions?

If uncertain whether binding code should be written:
1. Check if Idris implementation exists in `src/Proven/`
2. If yes: Call it via FFI (see `ffi/zig/README.md`)
3. If no: Implement in Idris FIRST, then wrap it
4. When in doubt: ASK before writing code

## References

- Architecture: `META.scm` ADR-008, ADR-009
- Ecosystem context: `ECOSYSTEM.scm`
- Current state: `STATE.scm`
- Idris 2 docs: https://idris2.readthedocs.io/
- hypatia scanner: `../hypatia/`
