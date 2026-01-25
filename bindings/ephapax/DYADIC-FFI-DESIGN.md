# Dyadic FFI Design - One Adapter for Both Modes

**Date:** 2026-01-24
**Question:** Does Ephapax need separate FFI adapters for affine vs linear modes?
**Answer:** **NO** - One Zig FFI adapter handles both modes.

---

## The Key Insight

The difference between affine and linear modes is in **Ephapax's type system enforcement**, not in the underlying FFI operations.

```text
                      ┌─────────────────────────────┐
                      │   Ephapax Type Checker      │
                      │                             │
                      │  Affine mode:  Drop allowed │
                      │  Linear mode:  Must consume │
                      └──────────────┬──────────────┘
                                     │
                    Same FFI calls in both modes
                                     │
                                     ▼
                      ┌─────────────────────────────┐
                      │   Zig FFI Adapter (ONE)     │
                      │                             │
                      │  lru_new, lru_put, lru_get  │
                      │  Same functions for both    │
                      └──────────────┬──────────────┘
                                     │
                                     ▼
                      ┌─────────────────────────────┐
                      │  Idris2 Proven Library      │
                      │                             │
                      │  Formally verified code     │
                      └─────────────────────────────┘
```

---

## Architecture

### FFI Layer (Zig) - SHARED

```zig
// One set of FFI functions for BOTH modes
export fn ephapax_proven_lru_new(capacity: u64) -> *LRUCacheHandle;
export fn ephapax_proven_lru_put(cache: *LRUCacheHandle, ...) -> *LRUCacheHandle;
export fn ephapax_proven_lru_get(cache: *LRUCacheHandle, ...) -> ?[*]const u8;
export fn ephapax_proven_lru_free(cache: *LRUCacheHandle) -> void;
```

**No separate affine/linear functions** - the FFI is mode-agnostic.

---

### Ephapax Type Layer - MODE-SPECIFIC

```ephapax
// Affine type (implicit cleanup allowed)
type LRUCacheAffine = ProvenLRU.LRUCache;

// Linear type (explicit consumption required)
type LRUCacheLinear! = ProvenLRU.LRUCache;
```

**Same underlying FFI type**, different Ephapax type annotations.

---

### Wrapper APIs - MODE-SPECIFIC

#### Affine Wrapper

```ephapax
module AffineAPI {
    fn new(capacity: usize) -> LRUCacheAffine {
        ProvenLRU.new(capacity)  // Calls same FFI
    }

    fn put(cache: LRUCacheAffine, key: String, value: Bytes) -> LRUCacheAffine {
        ProvenLRU.put(cache, key, value)  // Calls same FFI
    }

    fn free(cache: LRUCacheAffine) -> Unit {
        ProvenLRU.free(cache)  // Optional (can be implicit)
    }
}
```

#### Linear Wrapper

```ephapax
module LinearAPI {
    fn new(capacity: usize) -> LRUCacheLinear! {
        ProvenLRU.new(capacity)  // Calls SAME FFI as affine
    }

    fn put!(cache: LRUCacheLinear!, key: String, value: Bytes) -> LRUCacheLinear! {
        ProvenLRU.put(cache, key, value)  // Calls SAME FFI as affine
    }

    fn free!(cache: LRUCacheLinear!) -> Unit {
        ProvenLRU.free(cache)  // REQUIRED (compiler enforces)
    }
}
```

**Both call the same underlying Zig FFI functions!**

---

## How It Works

### Same FFI Call, Different Type Enforcement

```ephapax
// Affine mode
let cache = AffineAPI.new(1024);           // FFI: ephapax_proven_lru_new(1024)
let cache = AffineAPI.put(cache, "k", b);  // FFI: ephapax_proven_lru_put(...)
// Optional free (can drop implicitly)

// Linear mode
let! cache = LinearAPI.new(1024);            // FFI: ephapax_proven_lru_new(1024)
let! cache = LinearAPI.put(cache, "k", b);   // FFI: ephapax_proven_lru_put(...)
LinearAPI.free(cache);                       // REQUIRED or compiler error
```

**Same Zig functions called**, different compile-time checks in Ephapax.

---

## Benefits

### 1. Single FFI Codebase

- One `lru_adapter.zig` file for both modes
- Fewer bugs (no duplication)
- Easier maintenance

### 2. Zero Runtime Overhead

- Affine and linear are compile-time concepts
- Both compile to identical machine code
- No performance difference

### 3. Gradual Migration

```ephapax
// Start with affine for rapid prototyping
fn prototype() {
    let cache = AffineAPI.new(1024);
    // Fast iteration, implicit cleanup
}

// Migrate to linear for production
fn production() {
    let! cache = LinearAPI.new(1024);
    let! cache = LinearAPI.put(cache, ...);
    LinearAPI.free(cache);  // Compiler enforces safety
}

// SAME FFI underneath!
```

### 4. Type Safety at Both Levels

| Level | Safety Guarantee |
|-------|------------------|
| **Ephapax** | Linear types prevent use-after-free, double-free, leaks |
| **Idris2** | Dependent types prevent buffer overflow, cache overflow, logic bugs |

**Combined:** Strongest possible guarantees.

---

## Implementation Checklist

- [x] Zig FFI adapter (one for both modes)
- [x] Type definitions (LRUCacheHandle, BufferHandle, etc.)
- [x] Affine wrapper example (AffineAPI)
- [x] Linear wrapper example (LinearAPI)
- [x] Dyadic example program (shows both modes)
- [x] Test suite (Zig unit tests)
- [ ] Ephapax compiler integration (Week 1-2)
- [ ] ECHIDNA verification (Week 3)

---

## Files Created

```
proven/bindings/ephapax/
├── README.md                      # Overview of bindings
├── DYADIC-FFI-DESIGN.md           # This document
├── build.zig                      # Build configuration
├── src/
│   ├── types.zig                  # Shared opaque types
│   ├── lru_adapter.zig            # ONE adapter for both modes
│   ├── buffer_adapter.zig         # ONE adapter for both modes
│   ├── resource_adapter.zig       # ONE adapter for both modes
│   └── ephapax_proven.zig         # Main entry point
├── examples/
│   └── lru_cache_dyadic.eph       # Shows affine AND linear usage
└── tests/
    └── test_all.zig               # Zig unit tests
```

---

## Conclusion

**Question:** Do we need separate FFI adapters for affine vs linear?

**Answer:** **NO**

- ONE Zig FFI adapter handles both modes
- Difference is in Ephapax's type annotations (`LRUCache` vs `LRUCache!`)
- Same underlying FFI calls in both cases
- Zero runtime overhead
- Gradual migration path (affine → linear)

**This design maximizes code reuse while providing dual safety guarantees.**

---

## Next Steps

1. Integrate Zig FFI adapter into Ephapax compiler
2. Add `AffineAPI` and `LinearAPI` to Ephapax standard library
3. Test with real VRAM cache implementation
4. Verify proofs with ECHIDNA
5. Deploy to production
