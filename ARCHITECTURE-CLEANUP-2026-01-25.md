# Architecture Cleanup - 2026-01-25

## Problem Statement

proven v0.9.0 was rejected from opam-repository due to **fundamental architecture violations**:

- **130 security issues** (5 CRITICAL, 124 HIGH) in language bindings
- **Rust bindings** contained full reimplementations instead of FFI wrappers
- **ReScript bindings** used `getExn` that crashes on malformed input
- Code claimed "formally verified" but bypassed Idris2 verification entirely

**Root Cause:** LLMs kept adding native implementations in binding languages, defeating the purpose of formal verification.

## Actions Taken

### 1. Deleted All Rust Reimplementations ✅

```bash
# Backed up to .UNSAFE-RUST-DELETED-20260125/
# Then deleted:
rm bindings/rust/src/safe_*.rs  # 38 files
```

**Rationale:** These were NOT calling Idris2. They were pure Rust code with unsafe patterns.

### 2. Fixed ReScript Critical Issues ✅

Fixed 5 CRITICAL `getExn` calls in `Proven_SafeCron.res`:

```rescript
// BEFORE (crashes on invalid input):
let fields = [
  Belt.Array.getExn(parts, 0),  // CRITICAL
  Belt.Array.getExn(parts, 1),  // CRITICAL
  // ...
]

// AFTER (safe pattern matching):
switch parts {
| [field1, field2, field3, field4, field5] => // Handle
| _ => Error(InvalidFieldCount)
}
```

### 3. Updated All Documentation ✅

- `README.adoc` - Emphasizes Idris2 first, architecture diagram
- `bindings/rust/README.md` - Explains FFI-only architecture
- `.claude/CLAUDE.md` - AI instructions to prevent recurrence
- `META.scm` - Added ADR-008 (FFI-only bindings), ADR-009 (pre-submission validation)

### 4. Created CI/CD Enforcement ✅

`.github/workflows/architecture-enforcement.yml`:
- Blocks `safe_*.rs` files in Rust bindings
- Blocks `unwrap()`, `getExn`, `Obj.magic` patterns
- Requires Idris2 build to pass
- Runs hypatia security scan (must show 0 critical/high)

### 5. Configured echidnabot Auto-Cleanup ✅

`.echidnabot.toml`:
- `enforcement_level = "strict"`
- `auto_cleanup.enabled = true`
- Auto-deletes forbidden patterns in bindings/
- Creates PR with cleanup changes

## Architecture Now Enforced

```
Application (89 languages)
        ↓
  Language Binding (FFI wrapper ONLY) ← No logic allowed
        ↓
  Zig FFI Bridge (C ABI)
        ↓
  Idris2 RefC Compiled Code
        ↓
  Idris2 Source (src/Proven/*.idr) ← ALL LOGIC + PROOFS HERE
```

### What Belongs Where

| Component | Allowed Content | Forbidden |
|-----------|-----------------|-----------|
| `src/Proven/*.idr` | Logic, algorithms, proofs | N/A (Idris2 only) |
| `ffi/zig/` | C ABI bridge, FFI wrappers | Logic, algorithms |
| `bindings/*/` | Type conversions, error handling | `safe_*.rs`, logic, `unwrap()`, `getExn` |

## Comparison: Before vs After

### Before (WRONG - What We Deleted)

```rust
// bindings/rust/src/safe_math.rs - DELETED
pub fn safe_add(a: i32, b: i32) -> Result<i32> {
    a.checked_add(b).ok_or(Error::Overflow)  // NOT VERIFIED
}
```

**Issues:**
- No formal proof
- Bypasses Idris2 verification
- Had bugs (unwrap calls that could panic)

### After (CORRECT - What We Need To Build)

```rust
// bindings/rust/src/math.rs - FFI wrapper only
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
- Calls Idris2 code with **mathematical proof**
- FFI wrapper is simple (hard to get wrong)
- Same guarantee for all 89 language bindings

## Why Idris2?

**Idris2 provides:**
- **Dependent types** - Types can depend on values
- **Totality checking** - Compiler proves functions terminate
- **No runtime exceptions** - All errors explicit in types
- **Mathematical proofs** - Code correctness verified at compile time

**Example: Proven Safe Division**

```idris
-- Idris2 code with dependent type proof
data NonZero : Nat -> Type where
  IsNonZero : {n : Nat} -> (n /= 0) -> NonZero n

safeDiv : (a : Int) -> (b : Int) -> {auto prf : NonZero b} -> Int
safeDiv a b = a `div` b  -- Cannot call with b=0 (compiler prevents it)
```

This is **impossible** in Rust, ReScript, or any other binding language without dependent types.

## Lessons Learned

### What Went Wrong

1. **LLMs added native code** instead of FFI wrappers
2. **No CI enforcement** to catch violations
3. **Unclear documentation** about architecture
4. **No auto-cleanup** when violations occurred

### What We Fixed

1. ✅ Deleted all reimplementations
2. ✅ CI blocks unsafe patterns
3. ✅ Documentation emphasizes Idris2 first
4. ✅ echidnabot auto-cleans violations

### Prevention Going Forward

- **echidnabot scans daily** - Removes non-Idris code automatically
- **CI blocks merges** - Cannot merge if unsafe patterns present
- **Documentation prominent** - README, META.scm, .claude/CLAUDE.md all emphasize architecture
- **Pre-submission validation** - Run hypatia scan before publishing to registries

## Package Registry Impact

### Current Status

**DO NOT publish yet** - Repo is in transitional state:
- ✅ Unsafe code deleted
- ❌ FFI wrappers not yet implemented
- Status: **Broken** (bindings don't work)

### Re-Publication Plan

1. **Yank v0.9.x** from registries with deprecation notice
2. **Build FFI layer** - Implement Zig bridge properly
3. **Create FFI wrappers** - Thin wrappers in each language
4. **Test thoroughly** - Verify bindings call Idris2
5. **Publish v1.0.0** - Breaking change, FFI-based architecture

## Files Changed

### Deleted
- `bindings/rust/src/safe_*.rs` (38 files) - Backup in `.UNSAFE-RUST-DELETED-20260125/`

### Created
- `.github/workflows/architecture-enforcement.yml`
- `.claude/CLAUDE.md`
- `bindings/rust/README.md` (rewritten)
- `ARCHITECTURE-CLEANUP-2026-01-25.md` (this file)

### Modified
- `README.adoc` - Idris2-first messaging
- `META.scm` - ADR-008, ADR-009
- `.echidnabot.toml` - Auto-cleanup configuration
- `bindings/rescript/src/Proven_SafeCron.res` - Fixed 5 critical `getExn` calls

## Statistics

### Security Issues Fixed

- **Before:** 130 findings (5 CRITICAL, 124 HIGH, 1 MEDIUM)
- **After:** 0 critical in ReScript, Rust bindings deleted

### Code Removed

- 38 Rust files (~15,000 lines)
- All contained `unwrap()` and other unsafe patterns

### Architecture Violations

- **Before:** 34 modules reimplemented in Rust, 5 with no Idris backing
- **After:** 0 reimplementations allowed

## Next Steps

### Immediate (This Week)

1. ✅ Document cleanup (DONE - this file)
2. ⏳ Build Zig FFI bridge
3. ⏳ Create Rust FFI wrapper template
4. ⏳ Test Rust binding calls Idris2

### Short-Term (This Month)

1. Replicate Rust FFI pattern for other languages
2. Add fuzzing for FFI boundary
3. Complete FFI wrappers for top 10 languages

### Long-Term (This Quarter)

1. Publish v1.0.0 to all package registries
2. Auto-generate FFI wrappers from Idris2 types
3. Add more Idris2 modules with proofs

## Validation Checklist

Before re-publishing to any registry:

- [ ] Zig FFI bridge implemented
- [ ] Language bindings call FFI (no logic in bindings)
- [ ] hypatia scan shows 0 critical/high issues
- [ ] All Idris2 modules build with totality checking
- [ ] Tests pass for FFI boundary
- [ ] Documentation updated
- [ ] CI passes all architecture enforcement checks

## References

- Architecture decisions: `META.scm` (ADR-008, ADR-009)
- AI instructions: `.claude/CLAUDE.md`
- Binding template: `bindings/rust/README.md`
- CI enforcement: `.github/workflows/architecture-enforcement.yml`

---

**Summary:** proven is now correctly architected as an **Idris2 formal verification library with FFI bindings**, not a collection of native implementations. This cleanup prevents the architecture violations that led to the opam rejection and ensures all code claiming "proven" is actually mathematically verified.
