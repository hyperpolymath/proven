#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# Generate the full FFI package in a clean scratch tree, never from stale build/.
set -euo pipefail
root=${PROVEN_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}
root=$(cd "$root" && pwd)
ipkg=${IPKG:-proven-ffi.ipkg}
for arg in "$@"; do
  case "$arg" in
    --clean|--verbose) ;; # Every build is clean; diagnostics are always retained.
    --help|-h) echo 'Usage: build-refc.sh [--clean] [--verbose]'; exit 0 ;;
    *) echo "FAIL: unknown argument: $arg" >&2; exit 1 ;;
  esac
done
out="$root/build/refc"
mkdir -p "$out"
rm -f "$out/result.txt" "$out/sdk.env"
# This directory is the script-owned generated staging area. Remove only its
# generated C/header files so failed builds cannot leave an accepted old bundle.
find "$out" -maxdepth 1 -type f \( -name '*.c' -o -name '*.h' \) -delete
work=$(mktemp -d -t proven-full-refc.XXXXXXXX)
finish() {
  result=$?
  if [[ $result -ne 0 ]]; then printf 'FAIL: exit %s\n' "$result" > "$out/result.txt"; fi
  rm -rf -- "$work"
}
trap finish EXIT
idris=$(command -v "${IDRIS2:-idris2}")
idris=$(cd "$(dirname "$idris")" && printf '%s/%s' "$PWD" "$(basename "$idris")")
[[ $("$idris" --version) == 'Idris 2, version 0.8.0-b6cbc25d5' ]] || {
  echo 'FAIL: full RefC profile requires Idris2 b6cbc25d5cd2ef2a250556266fd7b68dfc360f8b' >&2; exit 1;
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

# A successful compiler exit is not enough: the C must be produced by THIS
# invocation. A prior run's generated file cannot serve as fresh evidence.
REFC_BUILD_STAMP=$(mktemp "$PROVEN_ROOT/build/.refc-start.XXXXXXXX")
trap 'rm -f "$REFC_BUILD_STAMP"' EXIT

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

# Only include RefC output created by the current compiler invocation.
# Do not also scan BUILD_DIR: it contains EXEC_DIR AND the previously staged
# build/refc directory, which made the old implementation double-count outputs
# and accept stale C as though Idris2 had generated it again.
GENERATED_C_FILES=()
GENERATED_H_FILES=()

if [ -d "$EXEC_DIR" ]; then
    while IFS= read -r -d '' f; do
        GENERATED_C_FILES+=("$f")
    done < <(find "$EXEC_DIR" -type f -name '*.c' -newer "$REFC_BUILD_STAMP" -print0 2>/dev/null)
    while IFS= read -r -d '' f; do
        GENERATED_H_FILES+=("$f")
    done < <(find "$EXEC_DIR" -type f -name '*.h' -newer "$REFC_BUILD_STAMP" -print0 2>/dev/null)
fi

if [ "${#GENERATED_C_FILES[@]}" -eq 0 ]; then
    die "RefC generated no fresh C source; cannot claim a build artifact. No prior staged output may be reused."
fi

log "Found ${#GENERATED_C_FILES[@]} fresh generated C file(s):"
for f in "${GENERATED_C_FILES[@]}"; do
    verbose "  $(realpath --relative-to="$PROVEN_ROOT" "$f")"
done

if [ "${#GENERATED_H_FILES[@]}" -gt 0 ]; then
    log "Found ${#GENERATED_H_FILES[@]} fresh generated header file(s):"
fi

# ---------------------------------------------------------------------------
# Step 3: Copy/symlink generated files to build/refc/ for Zig consumption
# ---------------------------------------------------------------------------

# Remove stale staged C/H only after verifying fresh RefC output exists.
# This directory is owned by this build script and must represent one run.
find "$REFC_OUTPUT_DIR" -maxdepth 1 -type f \( -name '*.c' -o -name '*.h' \) -delete

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
