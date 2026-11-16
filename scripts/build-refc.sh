#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# Generate the full FFI package in a clean scratch tree, never from stale build/.
set -euo pipefail
root=${PROVEN_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}
root=$(cd "$root" && pwd)
idris=$(command -v "${IDRIS2:-idris2}")
idris=$(cd "$(dirname "$idris")" && printf '%s/%s' "$PWD" "$(basename "$idris")")
ipkg=${IPKG:-proven-ffi.ipkg}
for arg in "$@"; do
  case "$arg" in
    --clean|--verbose) ;; # Every build is clean; diagnostics are always retained.
    --help|-h) echo 'Usage: build-refc.sh [--clean] [--verbose]'; exit 0 ;;
    *) echo "FAIL: unknown argument: $arg" >&2; exit 1 ;;
  esac
done
[[ $("$idris" --version) == 'Idris 2, version 0.8.0-b6cbc25d5' ]] || {
  echo 'FAIL: full RefC profile requires Idris2 b6cbc25d5cd2ef2a250556266fd7b68dfc360f8b' >&2; exit 1;
}
[[ $(uname -sm) == 'Linux x86_64' ]] || { echo 'FAIL: native Linux x86_64 required' >&2; exit 1; }
test -f "$root/$ipkg"
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
cp -R "$root/src" "$work/src"
cp "$root/$ipkg" "$work/package.ipkg"
if ! (cd "$work"; "$idris" --codegen refc --build package.ipkg) > "$out/build.log" 2>&1; then
  cat "$out/build.log" >&2; exit 1
fi
if grep -q '^Error:' "$out/build.log"; then cat "$out/build.log" >&2; exit 1; fi
mapfile -d '' -t generated < <(find "$work/build/exec" -type f -name '*.c' -print0)
[[ ${#generated[@]} == 1 ]] || { echo 'FAIL: expected exactly one fresh RefC translation unit' >&2; exit 1; }
grep -q 'Proven_SafeHeader_ByteKernel_headerStep' "${generated[0]}" || {
  echo 'FAIL: full package did not retain the header kernel symbol' >&2; exit 1;
}
prefix=$("$idris" --prefix)
mapfile -t runtimes < <(find "$prefix" -path '*/support/refc/runtime.h' -type f)
mapfile -t supports < <(find "$prefix" -path '*/support/c/idris_support.h' -type f)
mapfile -t libraries < <(find "$prefix" -name libidris2_support.a -type f)
[[ ${#runtimes[@]} == 1 && ${#supports[@]} == 1 && ${#libraries[@]} == 1 ]] || {
  echo 'FAIL: missing or ambiguous matching RefC SDK' >&2; exit 1;
}
runtime=$(dirname "${runtimes[0]}")
support=$(dirname "${supports[0]}")
libdir=$(dirname "${libraries[0]}")
test -s "$runtime/libidris2_refc.a"
cp "${generated[0]}" "$out/proven-ffi.c"
{
  printf 'REFC_OUTPUT_DIR=%q\n' "$out"
  printf 'IDRIS2_REFC_RUNTIME=%q\n' "$runtime"
  printf 'IDRIS2_C_SUPPORT=%q\n' "$support"
  printf 'IDRIS2_SUPPORT_LIB=%q\n' "$libdir"
} > "$out/sdk.env"
{
  "$idris" --version
  sha256sum "$work/package.ipkg" "$out/proven-ffi.c" "$runtime/"*.h "$runtime/libidris2_refc.a" "$support/"*.h "${libraries[0]}"
  (cd "$work"; find src -type f -name '*.idr' -exec sha256sum {} +)
} > "$out/identity.log"
echo PASS > "$out/result.txt"
printf 'PASS: fresh full-package RefC output; staging: %s\n' "$out"
printf 'Source %s/sdk.env before the Zig build. This is build evidence, not whole-library verification.\n' "$out"
