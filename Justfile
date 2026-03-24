# SPDX-License-Identifier: PMPL-1.0-or-later
# proven - Formally Verified Safety Library
# Build system using just (https://just.systems)
#
# Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
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

# Fast single-file typecheck (2-3 seconds). Usage: just check src/Proven/SafeMath.idr
check file:
    idris2 -p base -p contrib -p network --source-dir src --check {{file}}

# Quick build: core modules only (~24 modules, no totality)
check-core:
    idris2 --build core-only.ipkg

# Dev build: all modules without totality checking (faster iteration)
check-dev:
    idris2 --build proven-dev.ipkg

# Typecheck the main proven.ipkg (full library, all modules, WITH --total)
typecheck:
    pack typecheck proven.ipkg

# Typecheck the FFI-only package
typecheck-ffi:
    pack typecheck proven-ffi.ipkg

# Run Idris2 totality checker on the full library
verify-totality:
    idris2 --check --total proven.ipkg

# Full verified build (CI/release only — slow, uses --total)
check-full:
    idris2 --build proven.ipkg

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
    if [ -f test-simple.ipkg ]; then
        idris2 --build test-simple.ipkg
        echo "[proven] Idris2 build test passed"
    elif [ -f tests.ipkg ]; then
        idris2 --build tests.ipkg
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

# Run panic-attacker pre-commit scan
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"

# Format code
fmt:
    cargo fmt --all

# Check formatting without modifying
fmt-check:
    cargo fmt --all --check

# ---------------------------------------------------------------------------
# Onboarding recipes
# ---------------------------------------------------------------------------

# Check that all required tools are installed and working
doctor:
    #!/usr/bin/env bash
    set -euo pipefail
    PASS=0; FAIL=0; WARN=0
    check() {
        local name="$1" cmd="$2" min_ver="${3:-}"
        if command -v "$cmd" >/dev/null 2>&1; then
            ver=$("$cmd" --version 2>/dev/null | head -1 || echo "unknown")
            echo "  [OK] $name: $ver"
            PASS=$((PASS + 1))
        else
            echo "  [FAIL] $name: '$cmd' not found"
            FAIL=$((FAIL + 1))
        fi
    }
    check_optional() {
        local name="$1" cmd="$2"
        if command -v "$cmd" >/dev/null 2>&1; then
            ver=$("$cmd" --version 2>/dev/null | head -1 || echo "unknown")
            echo "  [OK] $name: $ver"
            PASS=$((PASS + 1))
        else
            echo "  [WARN] $name: '$cmd' not found (optional)"
            WARN=$((WARN + 1))
        fi
    }
    echo "=== proven: Doctor ==="
    echo ""
    echo "Required tools:"
    check "Idris2" "idris2"
    check "Zig" "zig"
    check "just" "just"
    # Check pack specifically (no --version flag)
    if command -v pack >/dev/null 2>&1; then
        echo "  [OK] pack: $(pack help 2>/dev/null | head -1 || echo 'installed')"
        PASS=$((PASS + 1))
    else
        echo "  [FAIL] pack: not found (install from https://github.com/stefan-hoeck/idris2-pack)"
        FAIL=$((FAIL + 1))
    fi
    echo ""
    echo "Optional tools:"
    check_optional "Rust/cargo" "cargo"
    check_optional "Deno" "deno"
    check_optional "Guile" "guile"
    check_optional "panic-attack" "panic-attack"
    echo ""
    echo "Idris2 environment:"
    if command -v idris2 >/dev/null 2>&1; then
        PREFIX="$(idris2 --prefix 2>/dev/null || echo 'unknown')"
        echo "  Prefix: $PREFIX"
        REFC="$(find "$PREFIX" -path "*/support/refc" -type d 2>/dev/null | head -1)"
        if [ -n "$REFC" ]; then
            echo "  [OK] RefC runtime: $REFC"
            PASS=$((PASS + 1))
        else
            echo "  [FAIL] RefC runtime: not found under $PREFIX"
            FAIL=$((FAIL + 1))
        fi
    fi
    echo ""
    echo "Build artefacts:"
    if [ -d "build/refc" ]; then
        echo "  [OK] build/refc/ exists (Idris2 RefC output)"
    else
        echo "  [INFO] build/refc/ not found — run 'just build-refc' first"
    fi
    if [ -f "ffi/zig/zig-out/lib/libproven.so" ] || [ -f "ffi/zig/zig-out/lib/libproven.dylib" ]; then
        echo "  [OK] libproven shared library built"
    else
        echo "  [INFO] libproven not built — run 'just build' for full pipeline"
    fi
    echo ""
    echo "=== Results: $PASS passed, $FAIL failed, $WARN warnings ==="
    if [ "$FAIL" -gt 0 ]; then
        echo "Run 'just heal' for install instructions."
        exit 1
    fi

# Show install instructions for missing tools
heal:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== proven: Heal ==="
    echo ""
    echo "Install missing tools:"
    echo ""
    if ! command -v idris2 >/dev/null 2>&1 || ! command -v pack >/dev/null 2>&1; then
        echo "  Idris2 + pack:"
        echo "    git clone https://github.com/stefan-hoeck/idris2-pack.git"
        echo "    cd idris2-pack && make micropack"
        echo "    # Then: pack install idris2"
        echo ""
    fi
    if ! command -v zig >/dev/null 2>&1; then
        echo "  Zig:"
        echo "    asdf plugin add zig"
        echo "    asdf install zig 0.13.0"
        echo "    asdf global zig 0.13.0"
        echo "    # Or download from https://ziglang.org/download/"
        echo ""
    fi
    if ! command -v just >/dev/null 2>&1; then
        echo "  just:"
        echo "    cargo install just"
        echo "    # Or: https://just.systems/man/en/installation.html"
        echo ""
    fi
    if ! command -v cargo >/dev/null 2>&1; then
        echo "  Rust (optional, for bindings):"
        echo "    asdf install rust nightly"
        echo ""
    fi
    if ! command -v panic-attack >/dev/null 2>&1; then
        echo "  panic-attack (optional, for pre-commit scans):"
        echo "    cargo install --git https://github.com/hyperpolymath/panic-attacker"
        echo ""
    fi
    echo "After installing, run 'just doctor' to verify."

# Guided tour of the repository structure
tour:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== proven: Guided Tour ==="
    echo ""
    echo "proven is a formally verified safety library."
    echo "ALL computation happens in Idris2. Everything else is a thin wrapper."
    echo ""
    echo "Architecture: Idris2 --> Zig FFI --> Language Bindings"
    echo ""
    echo "1. THE TRUTH: src/Proven/"
    echo "   258 Idris2 modules with dependent types and totality proofs."
    echo "   Key files:"
    echo "     src/Proven.idr          - Main re-export module"
    echo "     src/Proven/Core.idr     - Core types and proofs"
    echo "     src/Proven/Safe*.idr    - 104 verified safety modules"
    echo "     src/Proven/FFI/         - 65 FFI wrapper modules"
    echo ""
    echo "2. ABI BRIDGE: ffi/zig/"
    echo "   Zig compiles Idris2's RefC output into libproven.so."
    echo "   Pure data marshaling — NO safety logic here."
    echo "     ffi/zig/build.zig       - Build config"
    echo "     ffi/zig/src/main.zig    - C ABI exports"
    echo ""
    echo "3. BINDINGS: bindings/"
    echo "   120+ language targets. Each is a thin wrapper that calls"
    echo "   libproven via FFI. Bindings NEVER reimplement algorithms."
    echo "     bindings/rust/          - Rust crate"
    echo "     bindings/rescript/      - ReScript package"
    echo "     bindings/ocaml/         - OCaml opam package"
    echo ""
    echo "4. BUILD SYSTEM:"
    echo "   proven.ipkg              - Main Idris2 package (258 modules)"
    echo "   proven-ffi.ipkg          - FFI-only package"
    echo "   Justfile                 - Build automation"
    echo ""
    echo "5. METADATA: .machine_readable/6a2/"
    echo "   STATE.a2ml, META.a2ml, ECOSYSTEM.a2ml — project state"
    echo ""
    echo "Try: just build        (full pipeline)"
    echo "     just typecheck    (verify all 258 modules)"
    echo "     just test         (run test suite)"

# Show available recipes with descriptions
help-me:
    #!/usr/bin/env bash
    echo "=== proven: Help ==="
    echo ""
    echo "Onboarding:"
    echo "  just doctor     - Check required tools are installed"
    echo "  just heal       - Show install instructions for missing tools"
    echo "  just tour       - Guided walkthrough of repo structure"
    echo "  just help-me    - This help message"
    echo ""
    echo "Build pipeline (Idris2 -> RefC -> Zig FFI -> libproven.so):"
    echo "  just build      - Full build pipeline"
    echo "  just build-refc - Step 1: Idris2 to C via RefC"
    echo "  just build-ffi  - Step 2: Zig FFI with linked RefC output"
    echo ""
    echo "Verification:"
    echo "  just typecheck       - Typecheck all 258 modules"
    echo "  just typecheck-ffi   - Typecheck FFI-only package"
    echo "  just verify-totality - Run totality checker"
    echo ""
    echo "Testing:"
    echo "  just test       - Run all tests (Idris2 + Zig FFI)"
    echo "  just test-idris - Run Idris2 tests only"
    echo "  just test-ffi   - Run Zig FFI integration tests only"
    echo ""
    echo "Quality:"
    echo "  just scan       - Run hypatia security scan"
    echo "  just assail     - Run panic-attacker pre-commit scan"
    echo "  just fmt        - Format Rust code"
    echo "  just fmt-check  - Check formatting"
    echo ""
    echo "Housekeeping:"
    echo "  just clean      - Remove all build artefacts"
    echo "  just clean-refc - Remove RefC output only"
    echo "  just clean-ffi  - Remove Zig output only"
    echo "  just env-info   - Show tool versions and paths"
    echo "  just install    - Install libproven to /usr/local"
