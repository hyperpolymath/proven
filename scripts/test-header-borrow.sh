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
sha256sum "$root/src/Proven/FFI/HeaderBorrow.idr" "$root/src/Proven/SafeHeader/ByteKernel.idr" \
  "$root/tests/ffi/header_borrow.zig" "$root/scripts/test-header-borrow.sh" "$library/libproven.so" > "$out/inputs.sha256"
mkdir -p "$work/Proven/FFI" "$work/Proven/SafeHeader"
cp "$root/src/Proven/FFI/HeaderBorrow.idr" "$work/Proven/FFI/"
cp "$root/src/Proven/SafeHeader/ByteKernel.idr" "$work/Proven/SafeHeader/"
cp "$root/tests/ffi/header_borrow.zig" "$work/header_borrow.zig"
cp "$library/libproven.so" "$work/libproven.so"
cmp "$root/src/Proven/FFI/HeaderBorrow.idr" "$work/Proven/FFI/HeaderBorrow.idr"
cmp "$root/src/Proven/SafeHeader/ByteKernel.idr" "$work/Proven/SafeHeader/ByteKernel.idr"
cmp "$root/tests/ffi/header_borrow.zig" "$work/header_borrow.zig"
cmp "$library/libproven.so" "$work/libproven.so"
sha256sum -c "$out/inputs.sha256" > "$out/snapshot-recheck.txt"
(cd "$work"; sha256sum Proven/FFI/HeaderBorrow.idr Proven/SafeHeader/ByteKernel.idr header_borrow.zig libproven.so) > "$out/checked-inputs.sha256"
(cd "$work"; "$idris" --total -Werror --check Proven/FFI/HeaderBorrow.idr) > "$out/idris.txt" 2>&1
if grep -q '^Error:' "$out/idris.txt"; then echo 'FAIL: borrow compiler diagnostics' >&2; exit 1; fi
(cd "$work"; printf ':m\n:q\n' | "$idris" --no-banner Proven/FFI/HeaderBorrow.idr) > "$out/holes.txt" 2>&1
grep -q 'No holes' "$out/holes.txt"
if grep -q '^Error:' "$out/holes.txt"; then echo 'FAIL: borrow hole-check diagnostics' >&2; exit 1; fi
for mode in Debug ReleaseSafe ReleaseFast ReleaseSmall; do
  "$zig" test "$work/header_borrow.zig" -lc -L "$work" -lproven -rpath "$work" -O "$mode" > "$out/native-$mode.txt" 2>&1
done
(cd "$work"; sha256sum -c "$out/checked-inputs.sha256") > "$out/checked-input-recheck.txt"
sha256sum -c "$out/inputs.sha256" > "$out/input-recheck.txt"
cp "$work/header_borrow.zig" "$work/Proven/FFI/HeaderBorrow.idr" "$work/Proven/SafeHeader/ByteKernel.idr" "$out/"
echo PASS > "$out/result.txt"
echo 'HEADER BORROW PASS: checked Idris fold and actual exported ABI fixtures; readable immutable span remains a premise'
