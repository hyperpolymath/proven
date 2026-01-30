# a2ml + aLib Integration Plan for proven
**Date:** 2026-01-30
**Status:** Task #3 - In Progress
**Related Repos:** proven, a2ml, aggregate-library

---

## Overview

**Integration Goal:** Establish proven as the reference implementation for aggregate-library (aLib) operations, using a2ml for specification markup.

**Three Components:**

1. **proven** - Idris2 library with formally verified operations
2. **aggregate-library (aLib)** - Methods repository specifying overlap operations
3. **a2ml** - Attested Markup Language for specs/docs

---

## What is aLib?

**Purpose:** A stress-test and methods lab demonstrating how to specify a minimal overlap library across wildly different systems.

**NOT:**
- NOT a standard library replacement
- NOT a proposal that all languages share one stdlib
- NOT a mandatory dependency

**IS:**
- A demonstration of specification methods
- A conformance testing framework
- A stress-test for diversity

**Spec Categories:**
```
aggregate-library/specs/
├── arithmetic/    (add, subtract, multiply, divide, modulo)
├── collection/    (list operations)
├── comparison/    (equality, ordering)
├── conditional/   (if-then-else patterns)
├── logical/       (and, or, not)
└── string/        (concat, length, substring)
```

---

## What is a2ml?

**Purpose:** A lightweight, Djot-like markup that compiles into a typed, attested core.

**Features:**
- Progressive strictness (lax → checked → attested)
- Strong guarantees (required sections, resolved references, unique IDs)
- Opaque payloads preserved byte-for-byte
- Renderer portability (HTML/Markdown/PDF)

**Idris2 Core:**
```
a2ml/src/A2ML/
├── TypedCore.idr    (typed core stub)
├── Translator.idr   (translation stub)
└── Surface.idr      (surface AST)
```

---

## Current Integration Status

### proven's Role (per META.scm ADR-006)

**Decision:** "proven implements aLib core operations with formal verification proofs"

**What proven provides:**
- SafeMath implements aLib arithmetic: add, subtract, multiply, divide, modulo
- SafeString implements aLib string: concat, length, substring
- All implementations have termination proofs (totality)
- proven serves as gold standard for aLib correctness
- 89-language FFI enables aLib compliance testing across platforms

**Status:** ✓ **ALREADY IMPLEMENTED**

proven's SafeMath and SafeString already implement the aLib operations! The integration exists in code but may need:
1. Documentation mapping (proven operation → aLib spec)
2. Conformance test compliance
3. a2ml-based specification documents

### What's Missing

1. **Explicit aLib conformance testing**
   - aLib specs have conformance test vectors
   - proven should run these vectors
   - Document compliance results

2. **Specification in a2ml format**
   - proven's docs could use a2ml markup
   - Provides attested, verifiable documentation
   - Stronger structural guarantees than plain markdown

3. **ECOSYSTEM.scm linkage**
   - proven's ECOSYSTEM.scm should explicitly list aLib relationship
   - a2ml should be listed as documentation tool

---

## Integration Tasks

### Task 3.1: Update proven's ECOSYSTEM.scm ✓

Add explicit relationships:
```scheme
(related-projects
  ((project . "aggregate-library")
   (type . "specification-consumer")
   (relationship . "proven implements aLib core operations as reference implementation")
   (url . "https://github.com/hyperpolymath/aggregate-library"))

  ((project . "a2ml")
   (type . "documentation-tool")
   (relationship . "a2ml provides attested markup for proven specifications")
   (url . "https://github.com/hyperpolymath/a2ml")))
```

### Task 3.2: Create aLib Conformance Matrix

**File:** `docs/ALIB-CONFORMANCE.adoc` (or `.a2ml` when a2ml is stable)

**Content:**
```
| aLib Spec | proven Module | Function | Status | Notes |
|-----------|---------------|----------|--------|-------|
| arithmetic/add | SafeMath | add_checked | ✓ PASS | Overflow detection |
| arithmetic/subtract | SafeMath | sub_checked | ✓ PASS | Underflow detection |
| arithmetic/multiply | SafeMath | mul_checked | ✓ PASS | Overflow detection |
| arithmetic/divide | SafeMath | div | ✓ PASS | Division by zero handling |
| arithmetic/modulo | SafeMath | mod | ✓ PASS | Division by zero handling |
| string/concat | SafeString | concat | ✓ PASS | No length overflow |
| string/length | SafeString | length | ✓ PASS | Returns natural number |
| string/substring | SafeString | substring | ✓ PASS | Bounds checking |
```

### Task 3.3: Run aLib Conformance Tests

**Action:** Execute aLib test vectors against proven

**Location:** `aggregate-library/tests/` (if they exist)

**Steps:**
1. Check if aLib has test vectors
2. Create test runner that calls proven FFI
3. Document results in `docs/ALIB-CONFORMANCE.adoc`

### Task 3.4: Document a2ml Usage (Future)

**When:** After a2ml reaches v1.0

**Action:** Convert proven specs to a2ml format

**Example:**
```a2ml
# SafeMath Specification

@abstract:
SafeMath provides formally verified arithmetic operations with
overflow/underflow detection and division-by-zero prevention.
@end

@requires:
- All operations must be total (no crashes)
- Overflow detection via dependent types
- Conformance with aggregate-library arithmetic specs
@end

## Operations

@spec(id="safe-add"):
**Function:** `add_checked : Int -> Int -> Result Int`

**Semantics:** Addition with overflow detection

**Proof Obligations:**
1. Totality: ∀ a b. add_checked a b returns (OK | Error)
2. Correctness: ∀ a b. no overflow → add_checked a b = OK (a + b)
3. Safety: ∀ a b. overflow → add_checked a b = Error Overflow
@end
```

**Benefits:**
- Structural verification of docs
- References must resolve
- Sections cannot be accidentally omitted
- Attested markup provides stronger guarantees

---

## Integration Complete Checklist

For Task #3 completion:

- [ ] Update proven's ECOSYSTEM.scm with aLib + a2ml relationships
- [ ] Create `docs/ALIB-CONFORMANCE.adoc`
- [ ] Check if aLib has test vectors
- [ ] Document which proven operations map to which aLib specs
- [ ] Note a2ml integration as future enhancement (v1.1+)
- [ ] Update proven's README.adoc to mention aLib conformance

---

## Immediate Actions (for v1.0)

Since proven ALREADY implements aLib operations, the integration is mostly about documentation:

1. **Document the relationship** (ECOSYSTEM.scm update)
2. **Create conformance matrix** (which operations match which specs)
3. **Verify compliance** (if aLib has test vectors, run them)

This is NOT blocking v1.0 release - it's documentation work that clarifies existing compliance.

---

## Future Enhancements (v1.1+)

1. **a2ml-based specs** - Convert proven docs to a2ml when stable
2. **Automated conformance** - CI job that runs aLib test vectors
3. **Cross-language validation** - Use proven's 89 bindings to test aLib across ecosystems
4. **Contribute back to aLib** - Provide feedback on specs based on proven's formal verification insights

---

## Questions to Resolve

1. **Does aggregate-library have test vectors?**
   - Check `aggregate-library/tests/` or `aggregate-library/test/`
   - If yes: Run them against proven
   - If no: Create them based on aLib specs

2. **Which aLib specs does proven already satisfy?**
   - Arithmetic: ✓ (add, sub, mul, div, mod)
   - String: ✓ (concat, length, substring)
   - Collection: ? (check if SafeBuffer/SafeQueue satisfy)
   - Comparison: ? (check if proven has these)
   - Conditional: N/A (language feature, not library)
   - Logical: ? (check if proven has these)

3. **Should proven docs migrate to a2ml format?**
   - Current: AsciiDoc (.adoc)
   - Future: a2ml (.a2ml)
   - Timeline: After a2ml v1.0 (per a2ml ROADMAP.adoc)

---

## Next Steps

1. Update proven's ECOSYSTEM.scm ← DO THIS NOW
2. Create aLib conformance matrix ← DO THIS NOW
3. Check for aLib test vectors ← INVESTIGATE
4. Document in proven's README ← DO THIS NOW
5. Mark Task #3 complete
6. Move to Task #4 (testing documentation)

---

_Plan created: 2026-01-30_
_Next: Execute integration tasks_
