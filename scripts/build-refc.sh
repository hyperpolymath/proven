#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# build-refc.sh - Compile Idris2 proven-ffi package to C via RefC backend
#
# This script is Step 1 of the Idris2 -> RefC -> C -> Zig compilation pipeline.
# It invokes the Idris2 RefC code generator on proven-ffi.ipkg, producing .c and .h
# files that the Zig build system then compiles and links into libproven.{so,dylib,dll}.
#
# Usage:
#   ./scripts/build-refc.sh [--clean] [--verbose]
#
# Environment variables:
#   IDRIS2        - Path to idris2 binary (default: idris2 on PATH)
#   PROVEN_ROOT   - Repository root (default: parent of scripts/)
#   IPKG          - Package file to build (default: proven-ffi.ipkg)
#
# Output:
#   build/refc/   - Generated C source files
#   The script prints REFC_OUTPUT_DIR=<path> for downstream consumption.

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROVEN_ROOT="${PROVEN_ROOT:-$(cd "$SCRIPT_DIR/.." && pwd)}"
IDRIS2="${IDRIS2:-idris2}"
IPKG="${IPKG:-proven-ffi.ipkg}"
VERBOSE="${VERBOSE:-0}"
CLEAN=0

# Parse arguments
for arg in "$@"; do
    case "$arg" in
        --clean)   CLEAN=1 ;;
        --verbose) VERBOSE=1 ;;
        --help|-h)
            echo "Usage: $0 [--clean] [--verbose]"
            echo ""
            echo "Compile Idris2 proven-ffi package to C via RefC backend."
            echo ""
            echo "Options:"
            echo "  --clean     Remove build/refc/ before compiling"
            echo "  --verbose   Show detailed compilation output"
            echo "  --help      Show this help message"
            exit 0
            ;;
        *)
            echo "Error: Unknown argument: $arg" >&2
            exit 1
            ;;
    esac
done

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

log() {
    echo "[build-refc] $*"
}

verbose() {
    if [ "$VERBOSE" -eq 1 ]; then
        echo "[build-refc:debug] $*"
    fi
}

die() {
    echo "[build-refc] ERROR: $*" >&2
    exit 1
}

# ---------------------------------------------------------------------------
# Preflight checks
# ---------------------------------------------------------------------------

command -v "$IDRIS2" >/dev/null 2>&1 || die "idris2 not found. Set IDRIS2 env var or add to PATH."

IDRIS2_VERSION=$("$IDRIS2" --version 2>/dev/null || echo "unknown")
verbose "Idris2 version: $IDRIS2_VERSION"

IDRIS2_PREFIX=$("$IDRIS2" --prefix 2>/dev/null || die "Cannot determine Idris2 prefix")
verbose "Idris2 prefix: $IDRIS2_PREFIX"

# Verify ipkg exists
if [ ! -f "$PROVEN_ROOT/$IPKG" ]; then
    die "Package file not found: $PROVEN_ROOT/$IPKG"
fi

# Verify RefC runtime is installed
IDRIS2_REFC_DIR="$IDRIS2_PREFIX/idris2-0.8.0/support/refc"
if [ ! -d "$IDRIS2_REFC_DIR" ]; then
    # Try without version suffix (for non-standard installations)
    IDRIS2_REFC_DIR=$(find "$IDRIS2_PREFIX" -path "*/support/refc" -type d 2>/dev/null | head -1)
    if [ -z "$IDRIS2_REFC_DIR" ]; then
        die "Idris2 RefC support directory not found under $IDRIS2_PREFIX"
    fi
fi

if [ ! -f "$IDRIS2_REFC_DIR/libidris2_refc.a" ]; then
    die "libidris2_refc.a not found in $IDRIS2_REFC_DIR. Install Idris2 with RefC support."
fi

verbose "RefC runtime dir: $IDRIS2_REFC_DIR"

# ---------------------------------------------------------------------------
# Build directory setup
# ---------------------------------------------------------------------------

REFC_OUTPUT_DIR="$PROVEN_ROOT/build/refc"

if [ "$CLEAN" -eq 1 ] && [ -d "$REFC_OUTPUT_DIR" ]; then
    log "Cleaning $REFC_OUTPUT_DIR"
    rm -rf "$REFC_OUTPUT_DIR"
fi

mkdir -p "$REFC_OUTPUT_DIR"

# ---------------------------------------------------------------------------
# Step 1: Compile Idris2 to C via RefC backend
# ---------------------------------------------------------------------------

log "Compiling $IPKG with RefC code generator..."
verbose "Working directory: $PROVEN_ROOT"
verbose "Command: $IDRIS2 --codegen refc --build $IPKG"

cd "$PROVEN_ROOT"

if [ "$VERBOSE" -eq 1 ]; then
    "$IDRIS2" --codegen refc --build "$IPKG"
else
    "$IDRIS2" --codegen refc --build "$IPKG" 2>&1
fi

# ---------------------------------------------------------------------------
# Step 2: Locate generated C files
# ---------------------------------------------------------------------------

# Idris2 RefC output goes to build/exec/ (per ipkg outputdir) by default.
# The generated files are typically:
#   build/exec/<package-name>  (directory or single C file)
# For library builds, RefC generates .c files in build/exec/ or build/ttc/

# Search for generated C files in the build tree
EXEC_DIR="$PROVEN_ROOT/build/exec"
BUILD_DIR="$PROVEN_ROOT/build"

# Find all generated .c files from the RefC compilation
GENERATED_C_FILES=()
GENERATED_H_FILES=()

# RefC puts generated C code in the output directory
for dir in "$EXEC_DIR" "$BUILD_DIR"; do
    if [ -d "$dir" ]; then
        while IFS= read -r -d '' f; do
            GENERATED_C_FILES+=("$f")
        done < <(find "$dir" -name "*.c" -print0 2>/dev/null)

        while IFS= read -r -d '' f; do
            GENERATED_H_FILES+=("$f")
        done < <(find "$dir" -name "*.h" -print0 2>/dev/null)
    fi
done

if [ ${#GENERATED_C_FILES[@]} -eq 0 ]; then
    log "WARNING: No .c files found in build directory."
    log "The RefC backend may have placed output in a different location."
    log "Checking alternative locations..."

    # Check if RefC created a single output binary instead of .c files
    if [ -f "$EXEC_DIR/proven-ffi" ]; then
        log "Found compiled binary at $EXEC_DIR/proven-ffi"
        log "Note: RefC may have compiled directly. Re-run with --codegen refc --cg-opt --no-compile for .c output."
    fi
else
    log "Found ${#GENERATED_C_FILES[@]} generated C file(s):"
    for f in "${GENERATED_C_FILES[@]}"; do
        verbose "  $(realpath --relative-to="$PROVEN_ROOT" "$f")"
    done
fi

if [ ${#GENERATED_H_FILES[@]} -gt 0 ]; then
    log "Found ${#GENERATED_H_FILES[@]} generated header file(s):"
    for f in "${GENERATED_H_FILES[@]}"; do
        verbose "  $(realpath --relative-to="$PROVEN_ROOT" "$f")"
    done
fi

# ---------------------------------------------------------------------------
# Step 3: Copy/symlink generated files to build/refc/ for Zig consumption
# ---------------------------------------------------------------------------

# Copy generated C files into a clean staging directory that Zig can reference
for f in "${GENERATED_C_FILES[@]}"; do
    cp -v "$f" "$REFC_OUTPUT_DIR/" 2>/dev/null || cp "$f" "$REFC_OUTPUT_DIR/"
done

for f in "${GENERATED_H_FILES[@]}"; do
    cp -v "$f" "$REFC_OUTPUT_DIR/" 2>/dev/null || cp "$f" "$REFC_OUTPUT_DIR/"
done

# ---------------------------------------------------------------------------
# Step 4: Report results for downstream pipeline
# ---------------------------------------------------------------------------

log ""
log "========================================="
log " RefC Compilation Complete"
log "========================================="
log ""
log "REFC_OUTPUT_DIR=$REFC_OUTPUT_DIR"
log "IDRIS2_REFC_RUNTIME=$IDRIS2_REFC_DIR"
log "IDRIS2_SUPPORT_LIB=$IDRIS2_PREFIX/idris2-0.8.0/lib"
log "IDRIS2_C_SUPPORT=$IDRIS2_PREFIX/idris2-0.8.0/support/c"
log ""
log "C files staged in: $REFC_OUTPUT_DIR"
ls -la "$REFC_OUTPUT_DIR/" 2>/dev/null || true
log ""
log "Next step: Run 'just build-ffi' or 'zig build -Didris-refc=$REFC_OUTPUT_DIR'"

# Export for CI/CD and pipeline scripts
echo "REFC_OUTPUT_DIR=$REFC_OUTPUT_DIR"
echo "IDRIS2_REFC_RUNTIME=$IDRIS2_REFC_DIR"
echo "IDRIS2_SUPPORT_LIB=$IDRIS2_PREFIX/idris2-0.8.0/lib"
echo "IDRIS2_C_SUPPORT=$IDRIS2_PREFIX/idris2-0.8.0/support/c"
