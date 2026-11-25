#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Verify the *defined* exported symbols in a compiled libproven artifact
# cover the Rustler consumer's declared C ABI. This is a necessary, not
# sufficient, ABI condition: it does not establish compatible signatures,
# ownership, verified Idris provenance, or correct behaviour.
#
# Usage: bash scripts/check-native-nif-exports.sh LIBPROVEN [NIF_LIB_RS]
# Exit 0: all required names are defined; 1: missing symbols; 2: NOT RUN.
set -euo pipefail

if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo 'PROVEN_NIF_ABI_CLASS=not_run:usage' >&2
  echo 'usage: check-native-nif-exports.sh LIBPROVEN [NIF_LIB_RS]' >&2
  exit 2
fi

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
lib=$1
nif=${2:-"$root/bindings/elixir/native/proven_nif/src/lib.rs"}

if [[ ! -f $lib || ! -r $lib || ! -f $nif || ! -r $nif ]]; then
  echo 'PROVEN_NIF_ABI_CLASS=not_run:missing_input' >&2
  exit 2
fi
for cmd in awk comm nm sort wc; do
  command -v "$cmd" >/dev/null 2>&1 || {
    echo "PROVEN_NIF_ABI_CLASS=not_run:missing_tool:$cmd" >&2
    exit 2
  }
done

tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT

# Scan only the Rust extern-C block. Do not count Rust NIF wrappers or
# incidental function names elsewhere in the file.
awk '
  /^[[:space:]]*extern "C"[[:space:]]*\{/ { in_extern=1; next }
  in_extern && /^[[:space:]]*\}/ { in_extern=0 }
  in_extern && /^[[:space:]]*((pub(\([^)]*\))?|safe|unsafe)[[:space:]]+)*fn[[:space:]]+proven_[A-Za-z0-9_]+[[:space:]]*\(/ {
    line=$0
    sub(/^[[:space:]]*((pub(\([^)]*\))?|safe|unsafe)[[:space:]]+)*fn[[:space:]]+/, "", line)
    sub(/[[:space:]]*\(.*/, "", line)
    print line
  }
' "$nif" | LC_ALL=C sort -u > "$tmp/required"

if [[ ! -s $tmp/required ]]; then
  echo 'PROVEN_NIF_ABI_CLASS=not_run:empty_contract' >&2
  exit 2
fi

case $lib in
  *.a)  nm -g --defined-only "$lib" > "$tmp/nm" 2>"$tmp/nm-error" || {
    echo 'PROVEN_NIF_ABI_CLASS=not_run:unreadable_archive' >&2
    cat "$tmp/nm-error" >&2
    exit 2
  } ;;
  *.so|*.so.*) nm -D --defined-only "$lib" > "$tmp/nm" 2>"$tmp/nm-error" || {
    echo 'PROVEN_NIF_ABI_CLASS=not_run:unreadable_shared_lib' >&2
    cat "$tmp/nm-error" >&2
    exit 2
  } ;;
  *)
    echo 'PROVEN_NIF_ABI_CLASS=not_run:unsupported_artifact' >&2
    exit 2 ;;
esac

# Use only defined symbols; never mistake an undefined (U) reference for
# evidence that a function exists. nm --defined-only excludes those.
# Default-version definitions satisfy unversioned consumers; single-@ aliases do not.
awk '{
  symbol=$NF
  sub(/@@[^@[:space:]]+$/, "", symbol)
  if (symbol ~ /^proven_[A-Za-z0-9_]+$/) print symbol
}' "$tmp/nm" |
  LC_ALL=C sort -u > "$tmp/defined"

LC_ALL=C comm -23 "$tmp/required" "$tmp/defined" > "$tmp/missing"

required_count=$(wc -l < "$tmp/required")
defined_count=$(wc -l < "$tmp/defined")
missing_count=$(wc -l < "$tmp/missing")
echo "NIF ABI symbols: required=$required_count artifact_defined=$defined_count missing=$missing_count"
if [[ -s $tmp/missing ]]; then
  echo 'PROVEN_NIF_ABI_CLASS=missing_symbols' >&2
  sed 's/^/  missing C ABI symbol: /' "$tmp/missing" >&2
  exit 1
fi
echo 'PROVEN_NIF_ABI_CLASS=symbols_present_only'
echo 'NOTE: symbol coverage is NOT ABI type conformance or formal proof.'
