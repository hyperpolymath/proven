#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail
[[ $# == 1 && -f "$1/libproven.so" ]]
library=$(cd "$1" && pwd)
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
out=${EVIDENCE_DIR:-"$root/build/header-evidence/ownership"}
mkdir -p "$out"; out=$(cd "$out" && pwd)
echo INCOMPLETE > "$out/result.txt"
idris=$(command -v "${IDRIS2:-idris2}")
idris=$(cd "$(dirname "$idris")" && pwd)/$(basename "$idris")
zig=$(command -v "${ZIG:-zig}")
zig=$(cd "$(dirname "$zig")" && pwd)/$(basename "$zig")
[[ $("$idris" --version) == 'Idris 2, version 0.8.0-b6cbc25d5' ]]
[[ $("$zig" version) == 0.15.2 ]]
work=$(mktemp -d)
trap 'status=$?; if ((status)); then echo FAIL > "$out/result.txt"; tail -n 40 "$out"/*.txt >&2; fi; rm -rf "$work"' EXIT
mkdir -p "$work/Proven/FFI" "$work/Proven/SafeHeader"
cp "$root/src/Proven/FFI/HeaderBorrow.idr" "$work/Proven/FFI/"
cp "$root/src/Proven/SafeHeader/ByteKernel.idr" "$work/Proven/SafeHeader/"
(cd "$work"; "$idris" --total -Werror --check Proven/FFI/HeaderBorrow.idr) > "$out/idris.txt" 2>&1
if grep -q '^Error:' "$out/idris.txt"; then echo 'FAIL: borrow compiler diagnostics' >&2; exit 1; fi
(cd "$work"; printf ':m\n:q\n' | "$idris" --no-banner Proven/FFI/HeaderBorrow.idr) > "$out/holes.txt" 2>&1
grep -q 'No holes' "$out/holes.txt"
if grep -q '^Error:' "$out/holes.txt"; then echo 'FAIL: borrow hole-check diagnostics' >&2; exit 1; fi
for mode in Debug ReleaseSafe ReleaseFast ReleaseSmall; do
  "$zig" test "$root/tests/ffi/header_borrow.zig" -lc -L "$library" -lproven -rpath "$library" -O "$mode" > "$out/native-$mode.txt" 2>&1
done
sha256sum "$root/src/Proven/FFI/HeaderBorrow.idr" "$root/src/Proven/SafeHeader/ByteKernel.idr" "$root/tests/ffi/header_borrow.zig" "$library/libproven.so" > "$out/inputs.sha256"
echo PASS > "$out/result.txt"
echo 'HEADER BORROW PASS: checked Idris fold and actual exported ABI fixtures; readable immutable span remains a premise'
