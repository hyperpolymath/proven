#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
[[ $# == 1 && -f $1/result.txt && $(cat "$1/result.txt") == PASS ]]
bundle=$(cd "$1" && pwd)
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
out=${EVIDENCE_DIR:-"$root/build/header-evidence/export-controls"}
mkdir -p "$out"; out=$(cd "$out" && pwd)
echo INCOMPLETE > "$out/result.txt"
work=$(mktemp -d)
trap 'status=$?; if ((status)); then echo FAIL > "$out/result.txt"; tail -n 30 "$out"/*.txt >&2; fi; rm -rf "$work"' EXIT
zig=$(command -v "${ZIG:-zig}")
zig=$(cd "$(dirname "$zig")" && pwd)/$(basename "$zig")
[[ $("$zig" version) == 0.15.2 ]]
find "$root/ffi/zig/src" -name '*.zig' -type f -exec sha256sum {} + > "$out/inputs.sha256"
sha256sum "$root/ffi/zig/build_standalone.zig" "$root/tests/ffi/header_borrow.zig" \
  "$root/scripts/test-header-export-controls.sh" >> "$out/inputs.sha256"
find "$bundle" -type f -exec sha256sum {} + >> "$out/inputs.sha256"
mkdir "$work/zig"
cp -R "$root/ffi/zig/src" "$work/zig/"
cp "$root/ffi/zig/build_standalone.zig" "$work/zig/"
cp "$root/tests/ffi/header_borrow.zig" "$work/header_borrow.zig"
while IFS= read -r -d '' source; do cmp "$source" "$work/zig/src/${source#"$root/ffi/zig/src/"}"; done \
  < <(find "$root/ffi/zig/src" -name '*.zig' -type f -print0)
cmp "$root/ffi/zig/build_standalone.zig" "$work/zig/build_standalone.zig"
cmp "$root/tests/ffi/header_borrow.zig" "$work/header_borrow.zig"
sha256sum -c "$out/inputs.sha256" > "$out/snapshot-recheck.txt"
(cd "$work"; find zig/src -name '*.zig' -type f -exec sha256sum {} +; sha256sum zig/build_standalone.zig header_borrow.zig) > "$out/checked-inputs.sha256"
cp "$work/zig/src/main_refc.zig" "$work/original-main.zig"
for mutation in length pointer null_guard; do
  case "$mutation" in
    length) sed '/^pub export fn proven_header_has_crlf/,/^}/s/, ptr, len)/, ptr, len - len)/' "$work/original-main.zig" ;;
    pointer) sed '/^pub export fn proven_header_has_crlf/,/^}/s/, ptr, len)/, if (ptr) |p| (if (len > 0) p + 1 else p) else null, if (len > 0) len - 1 else 0)/' "$work/original-main.zig" ;;
    null_guard) awk '{print} /^pub export fn proven_header_has_crlf/ {print "    if (ptr == null) return core.headerHasCrlf(@import(\"header_refc.zig\"), \"\".ptr, 0);"}' "$work/original-main.zig" ;;
  esac > "$work/zig/src/main_refc.zig"
  if cmp -s "$work/original-main.zig" "$work/zig/src/main_refc.zig"; then echo 'FAIL unchanged export mutant' >&2; exit 1; fi
  (cd "$work/zig"; "$zig" build --build-file build_standalone.zig -Dshared=true -Doptimize=ReleaseSafe \
    -Dheader-refc="$bundle" --prefix "$work/$mutation") > "$out/build-$mutation.txt" 2>&1
  "$zig" test "$work/header_borrow.zig" -lc -L "$work/$mutation/lib" -lproven -rpath "$work/$mutation/lib" \
    -O ReleaseSafe --test-no-exec -femit-bin="$work/caller" > "$out/caller-$mutation.txt" 2>&1
  if "$work/caller" > "$out/reject-$mutation.txt" 2>&1; then echo "FAIL $mutation export escaped" >&2; exit 1; fi
  grep -q 'TestExpectedEqual' "$out/reject-$mutation.txt"
done
cp "$work/original-main.zig" "$work/zig/src/main_refc.zig"
(cd "$work"; sha256sum -c "$out/checked-inputs.sha256") > "$out/checked-input-recheck.txt"
sha256sum -c "$out/inputs.sha256" > "$out/input-recheck.txt"
echo PASS > "$out/result.txt"
echo 'HEADER EXPORT CONTROLS PASS: compiled actual export pointer, length and null-guard mutations rejected through the real RefC-backed ABI'
