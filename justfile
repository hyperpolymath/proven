# SPDX-License-Identifier: PMPL-1.0-or-later
# proven - Formally Verified Safety Library
# Build system using just (https://just.systems)
#
# Author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Pipeline: Idris2 --codegen refc--> C --zig build--> libproven.{so,dylib,dll}
#
# Prerequisites:
#   - idris2 (via pack) with RefC backend support
#   - zig (via asdf)
#   - just (https://just.systems)

# Default recipe: show available recipes
default:
    @just --list

# ---------------------------------------------------------------------------
# Core build pipeline
# ---------------------------------------------------------------------------

# Full build: compile Idris2 to RefC, then build Zig FFI library
build: build-refc build-ffi
    @echo "[proven] Full build complete."

# Step 1: Compile Idris2 proven-ffi.ipkg to C via RefC code generator
build-refc:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "[proven] Step 1: Compiling Idris2 to C via RefC backend..."
    bash scripts/build-refc.sh --verbose

# Step 2: Compile Zig FFI with linked Idris2 RefC output
build-ffi:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "[proven] Step 2: Building Zig FFI library..."
    PROVEN_ROOT="$(pwd)"
    REFC_DIR="$PROVEN_ROOT/build/refc"
    IDRIS2_PREFIX="$(idris2 --prefix 2>/dev/null)"
    # Locate the versioned support directory dynamically
    REFC_RUNTIME="$(find "$IDRIS2_PREFIX" -path "*/support/refc" -type d 2>/dev/null | head -1)"
    C_SUPPORT="$(find "$IDRIS2_PREFIX" -path "*/support/c" -type d 2>/dev/null | head -1)"
    SUPPORT_LIB="$(find "$IDRIS2_PREFIX" -path "*/lib/libidris2_support.*" 2>/dev/null | head -1 | xargs dirname)"
    if [ ! -d "$REFC_DIR" ]; then
        echo "[proven] ERROR: build/refc/ not found. Run 'just build-refc' first." >&2
        exit 1
    fi
    echo "[proven] RefC output:    $REFC_DIR"
    echo "[proven] RefC runtime:   $REFC_RUNTIME"
    echo "[proven] C support:      $C_SUPPORT"
    echo "[proven] Support lib:    $SUPPORT_LIB"
    # Resolve real zig path before cd (asdf shim needs .tool-versions in parent dirs)
    ZIG="$(which zig)"
    if command -v asdf >/dev/null 2>&1; then
        ZIG="$(asdf where zig 2>/dev/null)/bin/zig"
    fi
    cd "$PROVEN_ROOT/ffi/zig"
    "$ZIG" build \
        -Didris-refc="$REFC_DIR" \
        -Didris-refc-runtime="$REFC_RUNTIME" \
        -Didris-c-support="$C_SUPPORT" \
        -Didris-support-lib="$SUPPORT_LIB"
    echo "[proven] Zig FFI build complete."
    echo "[proven] Output:"
    ls -la zig-out/lib/ 2>/dev/null || true

# ---------------------------------------------------------------------------
# Type checking and verification
# ---------------------------------------------------------------------------

# Typecheck the main proven.ipkg (full library, all modules)
typecheck:
    pack typecheck proven.ipkg

# Typecheck the FFI-only package
typecheck-ffi:
    pack typecheck proven-ffi.ipkg

# Run Idris2 totality checker on the full library
verify-totality:
    idris2 --check --total proven.ipkg

# ---------------------------------------------------------------------------
# Testing
# ---------------------------------------------------------------------------

# Run all tests (Idris2 + Zig FFI integration)
test: test-idris test-ffi

# Run Idris2 unit tests
test-idris:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "[proven] Running Idris2 tests..."
    if [ -f tests.ipkg ]; then
        pack test tests.ipkg
    elif [ -d tests/ ]; then
        pack test proven.ipkg
    else
        echo "[proven] No Idris2 test suite found. Skipping."
    fi

# Run Zig FFI integration tests
test-ffi:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "[proven] Running Zig FFI integration tests..."
    PROVEN_ROOT="$(pwd)"
    REFC_DIR="$PROVEN_ROOT/build/refc"
    IDRIS2_PREFIX="$(idris2 --prefix 2>/dev/null)"
    REFC_RUNTIME="$(find "$IDRIS2_PREFIX" -path "*/support/refc" -type d 2>/dev/null | head -1)"
    C_SUPPORT="$(find "$IDRIS2_PREFIX" -path "*/support/c" -type d 2>/dev/null | head -1)"
    SUPPORT_LIB="$(find "$IDRIS2_PREFIX" -path "*/lib/libidris2_support.*" 2>/dev/null | head -1 | xargs dirname)"
    # Resolve real zig path before cd (asdf shim needs .tool-versions in parent dirs)
    ZIG="$(which zig)"
    if command -v asdf >/dev/null 2>&1; then
        ZIG="$(asdf where zig 2>/dev/null)/bin/zig"
    fi
    cd "$PROVEN_ROOT/ffi/zig"
    "$ZIG" build test \
        -Didris-refc="$REFC_DIR" \
        -Didris-refc-runtime="$REFC_RUNTIME" \
        -Didris-c-support="$C_SUPPORT" \
        -Didris-support-lib="$SUPPORT_LIB"
    echo "[proven] FFI tests passed."

# ---------------------------------------------------------------------------
# Cleaning
# ---------------------------------------------------------------------------

# Clean all build artifacts
clean:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "[proven] Cleaning build artifacts..."
    rm -rf build/
    rm -rf ffi/zig/zig-out/
    rm -rf ffi/zig/.zig-cache/
    echo "[proven] Clean complete."

# Clean only RefC output (to force recompilation)
clean-refc:
    rm -rf build/refc/
    rm -rf build/exec/
    rm -rf build/ttc/

# Clean only Zig output
clean-ffi:
    rm -rf ffi/zig/zig-out/
    rm -rf ffi/zig/.zig-cache/

# ---------------------------------------------------------------------------
# Development helpers
# ---------------------------------------------------------------------------

# Show Idris2 and Zig environment information
env-info:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== Idris2 ==="
    idris2 --version 2>/dev/null || echo "idris2 not found"
    echo "Prefix: $(idris2 --prefix 2>/dev/null || echo 'unknown')"
    echo "Libdir: $(idris2 --libdir 2>/dev/null || echo 'unknown')"
    echo ""
    echo "=== Zig ==="
    zig version 2>/dev/null || echo "zig not found"
    echo ""
    echo "=== Pack ==="
    pack help 2>/dev/null | head -1 || echo "pack not found"
    echo ""
    echo "=== RefC Runtime ==="
    IDRIS2_PREFIX="$(idris2 --prefix 2>/dev/null)"
    REFC_DIR="$(find "$IDRIS2_PREFIX" -path "*/support/refc" -type d 2>/dev/null | head -1)"
    echo "RefC dir: ${REFC_DIR:-not found}"
    if [ -n "$REFC_DIR" ]; then
        echo "libidris2_refc.a: $(ls "$REFC_DIR/libidris2_refc.a" 2>/dev/null || echo 'not found')"
    fi

# Run hypatia security scan on bindings
scan:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ -x ./hypatia-v2 ]; then
        ./hypatia-v2 . --severity=critical --severity=high --exclude=bindings/*/tests
    else
        echo "[proven] hypatia-v2 not found in repo root. Skipping scan."
    fi

# Install generated shared library to a target prefix
install prefix="/usr/local":
    #!/usr/bin/env bash
    set -euo pipefail
    LIB_DIR="ffi/zig/zig-out/lib"
    if [ ! -d "$LIB_DIR" ]; then
        echo "[proven] ERROR: No built library found. Run 'just build' first." >&2
        exit 1
    fi
    echo "[proven] Installing to {{prefix}}/lib/"
    install -d "{{prefix}}/lib"
    install -m 0755 "$LIB_DIR"/libproven.* "{{prefix}}/lib/" 2>/dev/null || true
    echo "[proven] Installed."
