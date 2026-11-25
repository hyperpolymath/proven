#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
out=${EVIDENCE_DIR:-"$root/build/header-evidence/boundary"}
mkdir -p "$out"; out=$(cd "$out" && pwd)
echo INCOMPLETE > "$out/result.txt"
work=$(mktemp -d)
trap 'status=$?; if ((status)); then echo FAIL > "$out/result.txt"; tail -n 30 "$out"/*.txt >&2; fi; rm -rf "$work"' EXIT
idris=$(command -v "${IDRIS2:-idris2}")
idris=$(cd "$(dirname "$idris")" && pwd)/$(basename "$idris")
zig=$(command -v "${ZIG:-zig}")
zig=$(cd "$(dirname "$zig")" && pwd)/$(basename "$zig")
[[ $("$idris" --version) == 'Idris 2, version 0.8.0-b6cbc25d5' ]]
[[ $("$zig" version) == 0.15.2 ]]

find "$root/ffi/zig/src" -name '*.zig' -type f -exec sha256sum {} + > "$out/inputs.sha256"
sha256sum "$root/ffi/zig/build_standalone.zig" "$root/scripts/test-header-boundary.sh" \
  "$root/src/Proven/FFI/HeaderBoundary.idr" "$root/src/Proven/FFI/HeaderBorrow.idr" \
  "$root/src/Proven/SafeHeader/ByteKernel.idr" >> "$out/inputs.sha256"
mkdir -p "$work/zig" "$work/idris/Proven/FFI" "$work/idris/Proven/SafeHeader"
cp -R "$root/ffi/zig/src" "$work/zig/"
cp "$root/ffi/zig/build_standalone.zig" "$work/zig/"
cp "$root/src/Proven/FFI/"{HeaderBoundary,HeaderBorrow}.idr "$work/idris/Proven/FFI/"
cp "$root/src/Proven/SafeHeader/ByteKernel.idr" "$work/idris/Proven/SafeHeader/"
while IFS= read -r -d '' source; do cmp "$source" "$work/zig/src/${source#"$root/ffi/zig/src/"}"; done \
  < <(find "$root/ffi/zig/src" -name '*.zig' -type f -print0)
cmp "$root/ffi/zig/build_standalone.zig" "$work/zig/build_standalone.zig"
for module in HeaderBoundary HeaderBorrow; do cmp "$root/src/Proven/FFI/$module.idr" "$work/idris/Proven/FFI/$module.idr"; done
cmp "$root/src/Proven/SafeHeader/ByteKernel.idr" "$work/idris/Proven/SafeHeader/ByteKernel.idr"
sha256sum -c "$out/inputs.sha256" > "$out/snapshot-recheck.txt"
(cd "$work"; find zig/src idris -type f \( -name '*.zig' -o -name '*.idr' \) -exec sha256sum {} +; sha256sum zig/build_standalone.zig) > "$out/checked-inputs.sha256"
(cd "$work/idris"; "$idris" --total -Werror --check Proven/FFI/HeaderBoundary.idr) > "$out/idris.txt" 2>&1
if grep -q '^Error:' "$out/idris.txt"; then echo 'FAIL: boundary compiler diagnostics' >&2; exit 1; fi
(cd "$work/idris"; printf ':m\n:q\n' | "$idris" --no-banner Proven/FFI/HeaderBoundary.idr) > "$out/holes.txt" 2>&1
grep -q 'No holes' "$out/holes.txt"
if grep -q '^Error:' "$out/holes.txt"; then echo 'FAIL: boundary hole-check diagnostics' >&2; exit 1; fi
cat > "$work/idris/BadSpan.idr" <<'IDRIS'
module BadSpan
import Data.Nat
import Proven.FFI.HeaderBoundary
%default total
bad : Span []
bad = MkSpan 0 1 LTEZero
IDRIS
(cd "$work/idris"; "$idris" --total -Werror --check BadSpan.idr) > "$out/reject-span.txt" 2>&1 || true
grep -q '^Error:' "$out/reject-span.txt"
grep -q 'BadSpan' "$out/reject-span.txt"
grep -Eiq 'mismatch|constraint|LTE|unify' "$out/reject-span.txt"
for mode in Debug ReleaseSafe ReleaseFast ReleaseSmall; do
  (cd "$work/zig"; "$zig" build header-boundary-test --build-file build_standalone.zig -Doptimize="$mode" --summary all) > "$out/native-$mode.txt" 2>&1
done
cp "$work/zig/src/core.zig" "$work/original-core.zig"
for mutation in extent null_status error_class null_order; do
  case "$mutation" in
    extent) sed '/^pub fn headerHasCrlf/,/^}/s/header_backend.detect(p\[0\.\.len\])/header_backend.detect(p[0..len - len])/' "$work/original-core.zig" ;;
    null_status) sed '/^pub fn headerHasCrlf/,/^}/s/status = PROVEN_ERR_NULL_POINTER/status = PROVEN_OK/' "$work/original-core.zig" ;;
    error_class) sed '/^pub fn headerHasCrlf/,/^}/s/) -99 else -7/) -7 else -99/' "$work/original-core.zig" ;;
    null_order) awk '{print} /^pub fn headerHasCrlf/ {print "    if (ptr == null) _ = header_backend.detect(&.{}) catch false;"}' "$work/original-core.zig" ;;
  esac > "$work/zig/src/core.zig"
  if cmp -s "$work/original-core.zig" "$work/zig/src/core.zig"; then echo 'FAIL unchanged boundary mutant' >&2; exit 1; fi
  (cd "$work/zig"; "$zig" build header-boundary-build --build-file build_standalone.zig -Doptimize=ReleaseSafe --prefix "$work/$mutation") > "$out/build-$mutation.txt" 2>&1
  if "$work/$mutation/bin/header-boundary-tests" > "$out/reject-$mutation.txt" 2>&1; then
    echo "FAIL $mutation escaped public-boundary tests" >&2; exit 1
  fi
  grep -q 'TestExpectedEqual' "$out/reject-$mutation.txt"
  grep -q 'header public boundary' "$out/reject-$mutation.txt"
done
cp "$work/original-core.zig" "$work/zig/src/core.zig"
(cd "$work"; sha256sum -c "$out/checked-inputs.sha256") > "$out/checked-input-recheck.txt"
sha256sum -c "$out/inputs.sha256" > "$out/input-recheck.txt"
cp "$work/zig/src/"{core,main_refc,header_boundary_tests}.zig "$work/idris/Proven/FFI/HeaderBoundary.idr" "$out/"
echo PASS > "$out/result.txt"
echo 'HEADER BOUNDARY PASS: actual entry dispatch, exact bounded spans and rejection controls; live readable machine-span correspondence remains a premise'
