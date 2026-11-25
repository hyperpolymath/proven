#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail
[[ $# == 1 && -f "$1/libproven.so" ]]
library=$(cd "$1" && pwd)
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
out=${EVIDENCE_DIR:-"$root/build/header-evidence/ownership"}
mkdir -p "$out"; out=$(cd "$out" && pwd)
echo INCOMPLETE > "$out/result.txt"
work=$(mktemp -d)
trap 'status=$?; if ((status)); then echo FAIL > "$out/result.txt"; tail -n 40 "$out"/*.txt >&2; fi; rm -rf "$work"' EXIT
mkdir -p "$work/Proven/FFI" "$work/Proven/SafeHeader"
cp "$root/src/Proven/FFI/HeaderBorrow.idr" "$work/Proven/FFI/"
cp "$root/src/Proven/SafeHeader/ByteKernel.idr" "$work/Proven/SafeHeader/"
(cd "$work"; idris2 --total -Werror --check Proven/FFI/HeaderBorrow.idr) > "$out/idris.txt" 2>&1
! grep -q '^Error:' "$out/idris.txt"
(cd "$work"; printf ':m\n:q\n' | idris2 --no-banner Proven/FFI/HeaderBorrow.idr) > "$out/holes.txt" 2>&1
grep -q 'No holes' "$out/holes.txt"
! grep -q '^Error:' "$out/holes.txt"
for mode in Debug ReleaseSafe ReleaseFast ReleaseSmall; do
  "${ZIG:-zig}" test "$root/tests/ffi/header_borrow.zig" -lc -L "$library" -lproven -rpath "$library" -O "$mode" > "$out/native-$mode.txt" 2>&1
done
sha256sum "$root/src/Proven/FFI/HeaderBorrow.idr" "$root/src/Proven/SafeHeader/ByteKernel.idr" "$root/tests/ffi/header_borrow.zig" "$library/libproven.so" > "$out/inputs.sha256"
echo PASS > "$out/result.txt"
echo 'HEADER BORROW PASS: checked Idris fold and actual exported ABI fixtures; readable immutable span remains a premise'
