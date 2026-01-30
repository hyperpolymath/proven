# proven Architecture - Root Cause Analysis
**Date:** 2026-01-30
**Critical Discovery:** The architecture is DISCONNECTED

---

## 🚨 CRITICAL FINDING: Idris2 and Zig Are NOT Connected

### The Devastating Discovery

**What we found:**
```bash
# Idris2 FFI exports to C
$ grep -r "^export" src/Proven/*.idr
0 results

# Idris2 foreign function declarations
$ grep -r "%foreign" src/Proven/*.idr
0 results
```

**What this means:**
- ✅ Idris2 code exists with proofs
- ❌ Idris2 code is NOT exported to C/Zig
- ❌ Zig does NOT call Idris2
- ⚠️ Zig reimplemented everything natively (WITHOUT PROOFS!)

**The architecture is BROKEN.**

---

## Current Reality vs. Intended Architecture

### What We THOUGHT Was Happening:

```
User Code
  ↓
Language Binding (Python, Rust, etc.)
  ↓
Zig FFI (C ABI bridge)
  ↓
Idris2 Compiled Code ← PROOFS HERE ✓
```

### What's ACTUALLY Happening:

```
User Code
  ↓
Language Binding (Python, Rust, etc.)
  ↓
Zig FFI (Native implementations - NO PROOFS!) ← PROBLEM!

[Idris2 Code with Proofs] ← UNUSED, DISCONNECTED!
```

**The Idris2 proofs exist but are completely disconnected from the FFI!**

---

## Evidence

### 1. Idris2 SafeMath.idr

**Has proofs:**
```idris
-- File: src/Proven/SafeMath.idr
module Proven.SafeMath

%default total  -- ✓ Totality checking enabled

public export  -- ✓ Exported for Idris2 use
div : Integer -> Integer -> Maybe Integer
div _ 0 = Nothing
div n d = Just (n `div` d)

-- NO export for C FFI
-- NO %foreign declaration
```

**Functions are proven total but NOT exported to C!**

### 2. Zig FFI main.zig

**Native reimplementation:**
```zig
// File: ffi/zig/src/main.zig
export fn proven_math_div(numerator: i64, denominator: i64) IntResult {
    if (denominator == 0) {
        return .{ .status = .err_division_by_zero, .value = 0 };
    }
    return .{ .status = .ok, .value = @divTrunc(numerator, denominator) };
}

// This is ZIG CODE, not calling Idris2!
// No proofs!
```

### 3. Python Binding

**Calls Zig (not Idris2):**
```python
# File: bindings/python/proven/safe_math.py
lib = get_lib()
result = lib.proven_math_div(numerator, denominator)
# ↑ Calls Zig native code, NOT Idris2!
```

**The chain is broken!**

---

## Root Cause: Missing FFI Export Layer

### What's Missing

**Idris2 modules need FFI export declarations:**

```idris
-- What SafeMath.idr SHOULD have:
module Proven.SafeMath

-- Export to C ABI
export
proven_safe_div : Int -> Int -> Int
proven_safe_div n d =
  case div (cast n) (cast d) of
    Just result => cast result
    Nothing => 0  -- Error sentinel
```

**Then Zig wraps the Idris2 C export:**

```zig
// Zig calls Idris2-generated C function
extern fn proven_safe_div(i64, i64) i64;

export fn proven_math_div(numerator: i64, denominator: i64) IntResult {
    const result = proven_safe_div(numerator, denominator);
    // Handle error sentinel, wrap in Result
}
```

**Status:** ❌ **NOT IMPLEMENTED**

---

## Why This Happened

### Theory: Development Evolution

**Phase 1:** Created Idris2 proofs
- Wrote proven modules in Idris2
- Proved totality, safety properties
- ✓ This part is complete

**Phase 2:** Wanted to use from other languages
- Needed FFI layer
- Idris2 FFI is complex (RefC backend, C codegen)
- **Shortcut taken:** Zig reimplemented functions natively

**Phase 3:** Bindings call Zig
- Python, Rust, etc. call Zig
- ✓ This works
- ❌ But Zig doesn't call Idris2!

**Result:** The proven guarantees don't reach the bindings.

---

## The Honest Truth About Current Status

### What IS Formally Verified

**Idris2 modules (79 files):**
- ✓ Proven total (cannot crash/hang)
- ✓ Type-level invariants enforced
- ✓ Dependent type proofs
- ❌ **BUT: Not used by bindings!**

### What Users Actually Get

**When calling from Python/Rust/etc:**
- They call Zig native code
- 14 functions call Idris2 (path, json, url, network)
- 141 functions are pure Zig (safe but unproven)

**Verification status:**
- 9% formally proven (the 14 that call Idris2)
- 91% Zig-safe (builtin overflow detection, but NO formal proofs)

---

## Architecture Options Analysis

### Option 1: Idris2 ABI + Zig FFI (INTENDED - FIX IT)

**How it should work:**
```
Idris2 (proven)
  → compiles to C via RefC backend
  → Zig imports C functions
  → Zig exports C ABI to languages
  → Language bindings call Zig
  → Zig calls Idris2-generated C
```

**Pros:**
- ✓ Formal proofs reach the user
- ✓ Single source of truth (Idris2)
- ✓ Zig is thin wrapper (no logic)

**Cons:**
- ⚠ FFI overhead (crossing boundary)
- ⚠ Complex build (Idris2 + Zig + bindings)
- ⚠ Requires careful type marshalling

**Recommendation:** ✅ **FIX THIS - IT'S THE RIGHT ARCHITECTURE**

**What needs to happen:**
1. Add `export` declarations to all Idris2 modules
2. Update Zig to call Idris2-generated C functions
3. Remove Zig native implementations (keep only wrappers)

**Estimated effort:** 2-4 weeks full-time

---

### Option 2: Idris2 for Both ABI and FFI

**How it would work:**
```
Idris2 (proven)
  → directly generates language bindings
  → No Zig layer
```

**Pros:**
- ✓ No FFI overhead
- ✓ Simpler build (no Zig)

**Cons:**
- ❌ Idris2 doesn't support this
- ❌ Would need custom codegen for 89 languages
- ❌ Massive engineering effort

**Recommendation:** ❌ **NOT FEASIBLE**

---

### Option 3: Zig for Both ABI and FFI

**How it would work:**
```
Zig (reimplemented logic)
  → exports to all languages
  → No Idris2 in production
```

**Pros:**
- ✓ Simpler (single language)
- ✓ Better performance (no FFI crossing)
- ✓ Easier debugging

**Cons:**
- ❌ Loses all formal verification
- ❌ Zig has no dependent types
- ❌ No totality checking
- ❌ **Defeats the entire purpose of proven!**

**Recommendation:** ❌ **ABSOLUTELY NOT**

---

### Option 4: Hybrid (Current Accidental State)

**What's happening now:**
```
14 functions: Zig → Idris2 (proven) ✓
141 functions: Pure Zig (safe but unproven) ⚠
```

**Pros:**
- ✓ Works for users (functionally)
- ✓ Zig overflow detection is safe-ish
- ✓ Critical operations (path, json) ARE proven

**Cons:**
- ❌ Misleading (claims proven, mostly isn't)
- ❌ Inconsistent (some proven, some not)
- ❌ Maintenance burden (two implementations)

**Recommendation:** ⚠ **ACCEPTABLE FOR v1.0 WITH HONEST DISCLOSURE**
**Long-term:** ❌ **MUST FIX FOR v1.1**

---

## Recommended Path Forward

### Immediate (v1.0 - Honest Disclosure)

**Accept current state with transparency:**

1. **Update README.adoc:**
```adoc
== Verification Status

IMPORTANT: proven uses a hybrid architecture:

* **Fully Verified (9%):** Path traversal, JSON parsing, URL parsing,
  Network validation - These call Idris2 with formal proofs

* **Zig-Safe (91%):** Arithmetic, checksums, string operations, etc. -
  These use Zig builtin safety (overflow detection) but lack formal proofs

See link:FFI-ARCHITECTURE-AUDIT-2026-01-30.md[Architecture Audit] for details.
```

2. **Update docs/VERIFICATION-STATUS.md:**
```markdown
| Module | Implementation | Status |
|--------|----------------|--------|
| SafePath | Idris2 | ✓ Formally Proven |
| SafeJson | Idris2 | ✓ Formally Proven |
| SafeUrl | Idris2 | ✓ Formally Proven |
| SafeNetwork | Idris2 | ✓ Formally Proven |
| SafeMath | Zig | ⚠ Safe (overflow detection) |
| SafeGeo | Zig | ⚠ Safe (bounds checks) |
| ... | ... | ... |
```

3. **Release v1.0** with honest claims

### Short-term (v1.1 - Connect Architecture)

**Fix the disconnection:**

**Week 1-2: Add Idris2 FFI exports**
```idris
-- For every module in src/Proven/SafeMath.idr
module Proven.SafeMath

-- Existing proven functions
public export
div : Integer -> Integer -> Maybe Integer
div _ 0 = Nothing
div n d = Just (n `div` d)

-- NEW: FFI export wrapper
export
ffi_safe_div : Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> PrimIO ()
ffi_safe_div ptrNum ptrDen ptrResult = toPrim $ do
  num <- readWord64 ptrNum
  den <- readWord64 ptrDen
  case div (cast num) (cast den) of
    Just res => writeWord64 ptrResult (cast res)
    Nothing => writeWord64 ptrResult 0  -- Sentinel
```

**Week 3: Update Zig to call Idris2**
```zig
// Import Idris2-generated C function
extern fn proven_Proven_SafeMath_ffi_safe_div(
    num: *u64,
    den: *u64,
    result: *u64
) callconv(.C) void;

// Zig wrapper (no logic!)
export fn proven_math_div(numerator: i64, denominator: i64) IntResult {
    var num: u64 = @bitCast(numerator);
    var den: u64 = @bitCast(denominator);
    var result: u64 = 0;

    proven_Proven_SafeMath_ffi_safe_div(&num, &den, &result);

    if (result == 0 and denominator != 0) {
        return .{ .status = .err_division_by_zero, .value = 0 };
    }
    return .{ .status = .ok, .value = @bitCast(result) };
}
```

**Week 4: Test and validate**
- All tests still pass
- Benchmarks (document FFI overhead)
- Update documentation

**Estimated effort:** 4 weeks, 1 person

---

### Medium-term (v1.2 - Complete Bindings)

**Week 5-8: Complete all bindings**
- Python: 37 → 79 modules (add 42)
- Rust: TBD → 79 modules
- Deno: TBD → 79 modules
- Use binding generator to automate

---

### Long-term (v2.0 - Bidirectional FFI)

**Months 3-12: Callback support**
- Function pointers in Zig
- Idris2 can call back to language
- Event handlers, plugins, async

---

## Decision Matrix

| Approach | Proofs | Performance | Effort | Recommended |
|----------|--------|-------------|--------|-------------|
| **Fix Option 1** (Idris2 ABI + Zig FFI) | ✓ Full | ⚠ FFI overhead | Medium | ✅ **YES** |
| Option 2 (Idris2 only) | ✓ Full | ✓ Best | Very High | ❌ No |
| Option 3 (Zig only) | ❌ None | ✓ Best | Low | ❌ **Never** |
| Option 4 (Keep hybrid) | ⚠ Partial | ✓ Good | None | ⚠ v1.0 only |

---

## Honest Answer to "Should we use Idris for both, Zig for both, or split?"

### ✅ **ANSWER: ABIs in Idris, FFI in Zig (Option 1) - FIX IT**

**Why:**
1. **Idris2 MUST be the ABI** - That's where the proofs are
2. **Zig SHOULD be FFI** - C ABI bridge to all languages
3. **Zig MUST NOT have logic** - Pure passthrough only

**The problem is NOT the architecture choice.**
**The problem is the IMPLEMENTATION - Idris2 isn't exporting to Zig!**

**Fix:** Connect Idris2 → Zig properly, remove Zig native code.

---

## Verification

### To confirm this analysis:

```bash
# Check if Idris2 generates C files when built
cd ~/Documents/hyperpolymath-repos/proven
idris2 --build proven.ipkg
find build -name "*.c" -o -name "*.h"

# Expected: C files generated from Idris2
# These should be callable from Zig
```

If NO C files exist → Idris2 isn't compiling to C at all!
If C files exist → Need to import them in Zig.

---

## Summary

**Root Cause:** Idris2 modules are NOT exported to C/Zig FFI

**Impact:**
- Bindings call Zig native code (not proven)
- Only 14/155 functions actually use Idris2
- Users don't get the formal verification they expect

**Solution:**
- Fix Option 1 architecture (Idris2 ABI + Zig FFI)
- Add `export` declarations to all Idris2 modules
- Update Zig to import and call Idris2-generated C
- Remove Zig native implementations

**Timeline:**
- v1.0: Release with honest disclosure (hybrid state)
- v1.1: Fix architecture (4 weeks)
- v1.2: Complete bindings (4 weeks)
- v2.0: Bidirectional FFI (months)

**Recommendation:** Delay v1.0 until v1.1 is complete (8 weeks total)

---

_Analysis completed: 2026-01-30_
_Critical finding: Architecture is correct but disconnected_
_Estimated fix time: 4-8 weeks_
