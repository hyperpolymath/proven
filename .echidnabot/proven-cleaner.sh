#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# proven Architecture Enforcer - Auto-delete non-FFI code from bindings
#
# This script is run by echidnabot daily to ensure NO logic exists in language
# bindings. ALL computation must happen in Idris2 with mathematical proofs.
#
# Violations of ADR-008 and ADR-009 are automatically deleted and a PR is created.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# Colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

VIOLATIONS_FOUND=0
DELETED_FILES=()
UNSAFE_PATTERNS=()

echo "🔍 proven Architecture Enforcement - Scanning bindings/"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# ============================================================================
# RULE 1: Delete ANY safe_*.rs files in Rust bindings
# ============================================================================
echo ""
echo "📋 Rule 1: Checking for Rust reimplementations (safe_*.rs files)..."

if compgen -G "bindings/rust/src/safe_*.rs" > /dev/null; then
    echo -e "${RED}❌ CRITICAL: Found Rust reimplementation files!${NC}"
    for file in bindings/rust/src/safe_*.rs; do
        if [ -f "$file" ]; then
            VIOLATIONS_FOUND=1
            DELETED_FILES+=("$file")
            echo -e "   ${RED}Deleting: $file${NC}"

            # Backup before deletion
            backup_dir=".UNSAFE-CODE-DELETED-$(date +%Y%m%d)"
            mkdir -p "$backup_dir/$(dirname "$file")"
            cp "$file" "$backup_dir/$file"

            # Delete the violation
            rm "$file"
        fi
    done
    echo -e "${YELLOW}   Backups saved to: $backup_dir/${NC}"
else
    echo -e "${GREEN}✓ No Rust reimplementation files found${NC}"
fi

# ============================================================================
# RULE 2: Scan for unwrap() in Rust bindings (production code only)
# ============================================================================
echo ""
echo "📋 Rule 2: Checking for unwrap() in Rust bindings..."

if grep -rn "\.unwrap()" bindings/rust/src/ 2>/dev/null | grep -v "/test" | grep -v "#\[test\]" | grep -v "tests\.rs"; then
    echo -e "${RED}❌ Found unwrap() calls in Rust bindings (production code)${NC}"
    VIOLATIONS_FOUND=1
    UNSAFE_PATTERNS+=("Rust: unwrap() in production code")
else
    echo -e "${GREEN}✓ No unwrap() in Rust production code${NC}"
fi

if grep -rn "\.expect(" bindings/rust/src/ 2>/dev/null | grep -v "/test" | grep -v "#\[test\]" | grep -v "tests\.rs" | grep -v "// SAFE:"; then
    echo -e "${YELLOW}⚠ Found expect() calls (acceptable with justification)${NC}"
fi

# ============================================================================
# RULE 3: Scan for getExn in ReScript bindings
# ============================================================================
echo ""
echo "📋 Rule 3: Checking for getExn in ReScript bindings..."

if grep -rn "getExn" bindings/rescript/src/ 2>/dev/null; then
    echo -e "${RED}❌ CRITICAL: Found getExn in ReScript bindings!${NC}"
    echo -e "${RED}   This causes crashes on invalid input (proven v0.9.0 opam rejection cause)${NC}"
    VIOLATIONS_FOUND=1
    UNSAFE_PATTERNS+=("ReScript: Belt.Array.getExn (crashes on invalid input)")
else
    echo -e "${GREEN}✓ No getExn in ReScript bindings${NC}"
fi

# ============================================================================
# RULE 4: Scan for Obj.magic in ReScript/OCaml bindings
# ============================================================================
echo ""
echo "📋 Rule 4: Checking for Obj.magic in ReScript/OCaml bindings..."

if grep -rn "Obj\.magic" bindings/rescript/src/ bindings/ocaml/ 2>/dev/null; then
    echo -e "${RED}❌ CRITICAL: Found Obj.magic (bypasses type system)!${NC}"
    VIOLATIONS_FOUND=1
    UNSAFE_PATTERNS+=("ReScript/OCaml: Obj.magic (bypasses type safety)")
else
    echo -e "${GREEN}✓ No Obj.magic found${NC}"
fi

# ============================================================================
# RULE 5: Scan for panic! in Rust bindings
# ============================================================================
echo ""
echo "📋 Rule 5: Checking for panic! in Rust bindings..."

if grep -rn "panic!" bindings/rust/src/ 2>/dev/null | grep -v "/test" | grep -v "#\[test\]"; then
    echo -e "${RED}❌ Found panic! in Rust bindings (production code)${NC}"
    VIOLATIONS_FOUND=1
    UNSAFE_PATTERNS+=("Rust: panic! in production code")
else
    echo -e "${GREEN}✓ No panic! in Rust production code${NC}"
fi

# ============================================================================
# RULE 6: Check Ada bindings for HTTP.Get reimplementation
# ============================================================================
echo ""
echo "📋 Rule 6: Checking Ada bindings for logic reimplementation..."

# Check if Ada bindings have any HTTP client implementation
if [ -d "bindings/ada" ]; then
    if grep -rn "Ada\.Streams" bindings/ada/src/ 2>/dev/null | grep -v "FFI"; then
        echo -e "${YELLOW}⚠ Ada bindings may contain reimplemented logic (check manually)${NC}"
        UNSAFE_PATTERNS+=("Ada: Possible logic reimplementation (needs manual review)")
    else
        echo -e "${GREEN}✓ Ada bindings appear to be FFI wrappers only${NC}"
    fi
fi

# ============================================================================
# RULE 7: Check for native crypto implementations
# ============================================================================
echo ""
echo "📋 Rule 7: Checking for native crypto implementations..."

# Common crypto patterns that should ONLY be in Idris2
CRYPTO_PATTERNS=(
    "sha256"
    "aes_encrypt"
    "rsa_sign"
    "hmac"
    "pbkdf2"
)

for pattern in "${CRYPTO_PATTERNS[@]}"; do
    if grep -rn "fn $pattern\|def $pattern\|function $pattern" bindings/ 2>/dev/null | grep -v "ffi\|FFI"; then
        echo -e "${RED}❌ Found crypto implementation: $pattern${NC}"
        VIOLATIONS_FOUND=1
        UNSAFE_PATTERNS+=("Crypto implementation in bindings: $pattern")
    fi
done

if [ ${#UNSAFE_PATTERNS[@]} -eq 0 ]; then
    echo -e "${GREEN}✓ No crypto reimplementations found${NC}"
fi

# ============================================================================
# Generate Report
# ============================================================================
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Enforcement Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ $VIOLATIONS_FOUND -eq 0 ]; then
    echo -e "${GREEN}✅ All checks passed - proven architecture is compliant!${NC}"
    exit 0
fi

echo -e "${RED}❌ Architecture violations detected!${NC}"
echo ""

if [ ${#DELETED_FILES[@]} -gt 0 ]; then
    echo "Files deleted:"
    for file in "${DELETED_FILES[@]}"; do
        echo "  - $file"
    done
    echo ""
fi

if [ ${#UNSAFE_PATTERNS[@]} -gt 0 ]; then
    echo "Unsafe patterns found:"
    for pattern in "${UNSAFE_PATTERNS[@]}"; do
        echo "  - $pattern"
    done
    echo ""
fi

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Creating cleanup commit..."

# Create commit if running in CI
if [ -n "${CI:-}" ]; then
    git config user.name "echidnabot"
    git config user.email "echidnabot@hyperpolymath.org"

    git add -A
    git commit -m "🤖 echidnabot: Remove architecture violations

Deleted files:
$(printf '  - %s\n' "${DELETED_FILES[@]}")

Unsafe patterns detected:
$(printf '  - %s\n' "${UNSAFE_PATTERNS[@]}")

This cleanup enforces ADR-008 and ADR-009:
- Language bindings MUST use Idris FFI, not reimplementations
- Pre-submission validation prevents external repo rejections

See: ARCHITECTURE-CLEANUP-2026-01-25.md

Co-Authored-By: echidnabot <echidnabot@hyperpolymath.org>"

    echo "✓ Commit created - echidnabot will create PR"
fi

exit 1
