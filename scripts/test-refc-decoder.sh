#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Fresh Idris2 contract and actual pure Zig decoder/import, in every native mode.
set -euo pipefail
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
out=${EVIDENCE_DIR:-"$root/build/header-evidence/decoder"}
mkdir -p "$out"
out=$(cd "$out" && pwd)
echo INCOMPLETE > "$out/result.txt"
work=$(mktemp -d)
trap 'status=$?; if ((status)); then echo FAIL > "$out/result.txt"; tail -n 35 "$out"/*.txt >&2; fi; rm -rf "$work"' EXIT
[[ $(zig version) == 0.15.2 ]]
mkdir -p "$work/Proven/FFI"
cp "$root/src/Proven/FFI/RefCBits8.idr" "$work/Proven/FFI/"
cp "$root/verification/header/DecoderOracle.idr" "$work/Main.idr"
cp "$root/verification/header/decoder_runner.zig" "$work/runner.zig"
cp "$root/ffi/zig/src/refc/"{bits8_decode_word,bits8_word}.zig "$work/"
(cd "$work"; sha256sum Main.idr Proven/FFI/RefCBits8.idr *.zig) > "$out/inputs.sha256"
(cd "$work"; idris2 --total -Werror --codegen refc -o oracle Main.idr) > "$out/idris-build.txt" 2>&1
if grep -q '^Error:' "$out/idris-build.txt"; then exit 1; fi
(cd "$work"; printf ':m\n:q\n' | idris2 --no-banner Main.idr) > "$out/idris-holes.txt" 2>&1
grep -q 'No holes' "$out/idris-holes.txt"
if grep -q '^Error:' "$out/idris-holes.txt"; then exit 1; fi
"$work/build/exec/oracle" > "$out/contract.tsv"
[[ $(wc -l < "$out/contract.tsv") == 1026 ]]
for mode in Debug ReleaseSafe ReleaseFast ReleaseSmall; do
    zig build-exe "$work/runner.zig" -O "$mode" -femit-bin="$work/native" > "$out/build-$mode.txt" 2>&1
    "$work/native" > "$out/native-$mode.tsv"
    cmp "$out/contract.tsv" "$out/native-$mode.tsv"
done
# Compile a wrong decoder, then require differential rejection at execution.
cp "$work/bits8_decode_word.zig" "$work/original"
sed '/if (raw != word.encodeWord(byte))/d' "$work/original" > "$work/bits8_decode_word.zig"
zig build-exe "$work/runner.zig" -O ReleaseSafe -femit-bin="$work/mutant" > "$out/mutant-build.txt" 2>&1
"$work/mutant" > "$out/mutant.tsv"
if cmp -s "$out/contract.tsv" "$out/mutant.tsv"; then echo 'FAIL equality mutant escaped'; exit 1; fi
cp "$work/original" "$work/bits8_decode_word.zig"
(cd "$work"; sha256sum -c "$out/inputs.sha256") > "$out/recheck.txt"
cp "$work/"*.zig "$out/"
cp "$work/Proven/FFI/RefCBits8.idr" "$out/"
idris2 --version > "$out/idris-version.txt"
zig version > "$out/zig-version.txt"
echo PASS > "$out/result.txt"
echo 'DECODER PASS: fresh total Idris2 oracle, 1026 witnesses, four modes, rejected compiled mutant'

