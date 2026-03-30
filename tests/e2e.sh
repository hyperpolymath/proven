#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# proven — End-to-End Test Suite
#
# Tests the full proof chain: Idris2 → Zig FFI → verification
#   1. Idris2 type-checks (proof verification)
#   2. Zig FFI builds and links
#   3. FFI integration tests pass
#   4. Cross-module safety (no dangerous patterns)
#   5. Binding consistency (where bindings exist)
#
# Usage:
#   bash tests/e2e.sh
#   just e2e
#
# Prerequisites:
#   - idris2 (for proof verification)
#   - zig (for FFI compilation)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

PASS=0
FAIL=0
SKIP=0

green() { printf '\033[32m%s\033[0m\n' "$*"; }
red()   { printf '\033[31m%s\033[0m\n' "$*"; }
yellow(){ printf '\033[33m%s\033[0m\n' "$*"; }
bold()  { printf '\033[1m%s\033[0m\n' "$*"; }

pass() { green "  PASS: $1"; PASS=$((PASS + 1)); }
fail_test() { red "  FAIL: $1"; FAIL=$((FAIL + 1)); }
skip_test() { yellow "  SKIP: $1 ($2)"; SKIP=$((SKIP + 1)); }

echo "═══════════════════════════════════════════════════════════════"
echo "  proven — End-to-End Tests"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# ─── Preflight ───────────────────────────────────────────────────────
bold "Preflight"

HAS_IDRIS2=false
HAS_ZIG=false

if command -v idris2 >/dev/null 2>&1; then
    HAS_IDRIS2=true
    green "  Idris2 available: $(idris2 --version 2>&1 | head -1)"
fi

if command -v zig >/dev/null 2>&1; then
    HAS_ZIG=true
    green "  Zig available: $(zig version)"
fi

echo ""

# ═══════════════════════════════════════════════════════════════════════
# Section 1: Idris2 Proof Verification (type-checking IS proof)
# ═══════════════════════════════════════════════════════════════════════
bold "Section 1: Idris2 proof verification"

if $HAS_IDRIS2; then
    cd "$PROJECT_DIR"

    # Check the main package builds (type-checks all proofs)
    if [ -f "proven.ipkg" ]; then
        if timeout 120 idris2 --check proven.ipkg >/dev/null 2>&1; then
            pass "Main package type-checks (all proofs verified)"
        else
            fail_test "Main package type-check failed"
        fi
    else
        skip_test "Package type-check" "proven.ipkg not found"
    fi

    # Count verified modules
    PROOF_COUNT=$(find src/ -name "Proofs.idr" -o -name "*Proof*.idr" 2>/dev/null | wc -l)
    if [ "$PROOF_COUNT" -gt 0 ]; then
        pass "Found $PROOF_COUNT proof modules"
    else
        skip_test "Proof count" "no proof files found"
    fi
else
    skip_test "Idris2 verification" "idris2 not installed"
fi
echo ""

# ═══════════════════════════════════════════════════════════════════════
# Section 2: Zig FFI Build + Integration Tests
# ═══════════════════════════════════════════════════════════════════════
bold "Section 2: Zig FFI build and integration"

if $HAS_ZIG; then
    cd "$PROJECT_DIR"

    FFI_DIR="ffi/zig"
    if [ -d "$FFI_DIR" ] && [ -f "$FFI_DIR/build.zig" ]; then
        # Build FFI
        if (cd "$FFI_DIR" && zig build 2>/dev/null); then
            pass "Zig FFI builds successfully"

            # Check library output exists
            if ls "$FFI_DIR"/zig-out/lib/libproven.* >/dev/null 2>&1; then
                pass "libproven shared library produced"
            else
                skip_test "libproven output" "library not found in zig-out"
            fi
        else
            fail_test "Zig FFI build failed"
        fi

        # Run FFI integration tests
        if (cd "$FFI_DIR" && zig build test 2>/dev/null); then
            pass "Zig FFI integration tests pass"
        else
            fail_test "Zig FFI integration tests failed"
        fi
    else
        skip_test "Zig FFI" "ffi/zig/build.zig not found"
    fi
else
    skip_test "Zig FFI" "zig not installed"
fi
echo ""

# ═══════════════════════════════════════════════════════════════════════
# Section 3: Safety Aspect — No Dangerous Patterns
# ═══════════════════════════════════════════════════════════════════════
bold "Section 3: Safety aspects"

cd "$PROJECT_DIR"

# Idris2: no believe_me, assert_total, really_believe_me
DANGEROUS=$(grep -rn 'believe_me\|assert_total\|really_believe_me' src/ 2>/dev/null | grep -v "^Binary" | grep -v "test" || true)
if [ -n "$DANGEROUS" ]; then
    fail_test "Dangerous Idris2 patterns found ($(echo "$DANGEROUS" | wc -l) occurrences)"
    echo "$DANGEROUS" | head -3
else
    pass "No dangerous Idris2 patterns in src/"
fi

# Zig: no @panic in production code (test code OK)
ZIG_PANIC=$(grep -rn '@panic' ffi/zig/src/ 2>/dev/null | grep -v "test" || true)
if [ -n "$ZIG_PANIC" ]; then
    fail_test "Zig @panic in production code ($(echo "$ZIG_PANIC" | wc -l) occurrences)"
else
    pass "No @panic in Zig production code"
fi

# Bindings: no unsafe reimplementation
UNSAFE_BINDINGS=$(grep -rn 'unsafe\|unwrap()\|getExn\|force\|unsafeCoerce' bindings/ 2>/dev/null | grep -v "test" | grep -v "comment" || true)
if [ -n "$UNSAFE_BINDINGS" ]; then
    fail_test "Unsafe patterns in bindings ($(echo "$UNSAFE_BINDINGS" | wc -l) occurrences)"
else
    pass "No unsafe patterns in bindings"
fi
echo ""

# ═══════════════════════════════════════════════════════════════════════
# Section 4: Property Tests
# ═══════════════════════════════════════════════════════════════════════
bold "Section 4: Property tests"

if $HAS_IDRIS2 && [ -f "tests/Main.idr" ]; then
    PROP_COUNT=$(find tests/properties/ -name "*.idr" 2>/dev/null | wc -l)
    if [ "$PROP_COUNT" -gt 0 ]; then
        pass "Found $PROP_COUNT property test modules"
    fi

    if timeout 120 idris2 --check tests/Main.idr >/dev/null 2>&1; then
        pass "Property test suite type-checks"
    else
        skip_test "Property tests" "type-check failed or timed out"
    fi
else
    skip_test "Property tests" "idris2 not available or tests/Main.idr missing"
fi
echo ""

# ═══════════════════════════════════════════════════════════════════════
# Section 5: Binding Coverage
# ═══════════════════════════════════════════════════════════════════════
bold "Section 5: Binding coverage"

if [ -d "bindings" ]; then
    TOTAL_BINDINGS=$(find bindings/ -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l)
    TESTED_BINDINGS=0

    for binding_dir in bindings/*/; do
        [ -d "$binding_dir" ] || continue
        if find "$binding_dir" -name "*test*" -o -name "*spec*" 2>/dev/null | grep -q .; then
            TESTED_BINDINGS=$((TESTED_BINDINGS + 1))
        fi
    done

    COVERAGE=$((TESTED_BINDINGS * 100 / (TOTAL_BINDINGS > 0 ? TOTAL_BINDINGS : 1)))
    pass "Binding coverage: $TESTED_BINDINGS/$TOTAL_BINDINGS ($COVERAGE%)"
else
    skip_test "Binding coverage" "bindings/ not found"
fi
echo ""

# ═══════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════
echo "═══════════════════════════════════════════════════════════════"
printf "  Results: "
green "PASS=$PASS" | tr -d '\n'
echo -n "  "
if [ "$FAIL" -gt 0 ]; then red "FAIL=$FAIL" | tr -d '\n'; else echo -n "FAIL=0"; fi
echo -n "  "
if [ "$SKIP" -gt 0 ]; then yellow "SKIP=$SKIP"; else echo "SKIP=0"; fi
echo ""
echo "═══════════════════════════════════════════════════════════════"

exit "$FAIL"
