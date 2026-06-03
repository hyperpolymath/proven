#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# symbol-audit.sh - Verify libproven.so exports every WIRED symbol declared
# in bindings/chapel/symbol-manifest.txt.
#
# Reads the manifest from $BINDING_ROOT/symbol-manifest.txt (one C symbol
# name per line; lines starting with '#' are comments).  Runs nm -D against
# the libproven.so located via PROVEN_LIB_PATH and reports PASS / FAIL per
# symbol.
#
# Detachable: this script does NOT traverse outside the binding's own
# directory.  The libproven.so location is supplied by the caller via
# PROVEN_LIB_PATH so the script works in the in-repo layout, in a release
# install, or in a standalone proven-chapel checkout.
#
# Environment:
#   PROVEN_LIB_PATH   directory containing libproven.so (REQUIRED)
#
# Exit codes:
#   0 - all WIRED symbols present
#   1 - one or more WIRED symbols missing
#   2 - libproven.so or manifest not found / env not set
#
# Usage:
#   PROVEN_LIB_PATH=/opt/proven/lib ./scripts/symbol-audit.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BINDING_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MANIFEST="$BINDING_ROOT/symbol-manifest.txt"

if [ -z "${PROVEN_LIB_PATH:-}" ]; then
    echo "ERROR: PROVEN_LIB_PATH is not set" >&2
    echo "       Set it to the directory containing libproven.so." >&2
    exit 2
fi

LIB="$PROVEN_LIB_PATH/libproven.so"

if [ ! -f "$LIB" ]; then
    echo "ERROR: libproven.so not found at: $LIB" >&2
    exit 2
fi

if [ ! -f "$MANIFEST" ]; then
    echo "ERROR: WIRED manifest not found at: $MANIFEST" >&2
    exit 2
fi

EXPORTED="$(nm -D --defined-only "$LIB" 2>/dev/null \
    | awk '$2 ~ /^[TWV]$/ { print $3 }' \
    | sort -u)"

PASS=0
FAIL=0
FAILED_SYMS=()

while IFS= read -r line; do
    sym="${line%%#*}"
    sym="${sym// /}"
    [ -z "$sym" ] && continue

    if echo "$EXPORTED" | grep -qx "$sym"; then
        printf "PASS  %s\n" "$sym"
        PASS=$((PASS + 1))
    else
        printf "FAIL  %s\n" "$sym"
        FAIL=$((FAIL + 1))
        FAILED_SYMS+=("$sym")
    fi
done < "$MANIFEST"

echo
echo "=== chapel-symbol-audit ==="
echo "  libproven: $LIB"
echo "  manifest:  $MANIFEST"
echo "  PASS:      $PASS"
echo "  FAIL:      $FAIL"

if [ "$FAIL" -ne 0 ]; then
    echo
    echo "Missing WIRED symbols (binding will not link against this libproven):"
    for s in "${FAILED_SYMS[@]}"; do
        printf "  - %s\n" "$s"
    done
    exit 1
fi
