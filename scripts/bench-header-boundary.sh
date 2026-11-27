#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
[[ $# == 1 && -f $1/libproven.so ]]
library=$(cd "$1" && pwd)
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
out=${EVIDENCE_DIR:-"$root/build/header-evidence/benchmark"}
mkdir -p "$out"; out=$(cd "$out" && pwd)
echo INCOMPLETE > "$out/result.txt"
work=$(mktemp -d)
trap 'status=$?; if ((status)); then echo FAIL > "$out/result.txt"; tail -n 25 "$out"/*.txt >&2; fi; rm -rf "$work"' EXIT
zig=$(command -v "${ZIG:-zig}")
zig=$(cd "$(dirname "$zig")" && pwd)/$(basename "$zig")
idris=$(command -v "${IDRIS2:-idris2}")
idris=$(cd "$(dirname "$idris")" && pwd)/$(basename "$idris")
[[ $("$zig" version) == 0.15.2 ]]
[[ $("$idris" --version) == 'Idris 2, version 0.8.0-b6cbc25d5' ]]
sha256sum "$root/benchmarks/header_boundary.zig" "$root/scripts/bench-header-boundary.sh" \
  "$library/libproven.so" > "$out/inputs.sha256"
cp "$root/benchmarks/header_boundary.zig" "$work/header_boundary.zig"
cp "$library/libproven.so" "$work/libproven.so"
cmp "$root/benchmarks/header_boundary.zig" "$work/header_boundary.zig"
cmp "$library/libproven.so" "$work/libproven.so"
sha256sum -c "$out/inputs.sha256" > "$out/snapshot-recheck.txt"
(cd "$work"; sha256sum header_boundary.zig libproven.so) > "$out/checked-inputs.sha256"
{
  date -u '+UTC %Y-%m-%dT%H:%M:%SZ'
  uname -a
  "$zig" version
  "$idris" --version
  if command -v lscpu >/dev/null; then lscpu; fi
  printf '%s\n' 'Caller modes vary; all use the same qualified library binary.' \
    'Timings include per-call correctness guards. Samples only; no regression baseline or threshold.'
} > "$out/environment.txt"
for mode in Debug ReleaseSafe ReleaseFast ReleaseSmall; do
  "$zig" build-exe "$work/header_boundary.zig" -lc -L "$work" -lproven -rpath "$work" \
    -O "$mode" -femit-bin="$work/benchmark" > "$out/build-$mode.txt" 2>&1
  timeout -k 5 60 "$work/benchmark" > "$out/$mode.jsonl" 2> "$out/run-$mode.txt"
  jq -e -s --arg mode "$mode" '
    length == 15 and all(.[];
      .schema == "proven.header.benchmark/1" and .backend == "actual-exported-refc-abi" and
      .mode == $mode and .zig == "0.15.2" and .correctness == "PASS" and
      .timed_correctness_guards == true and .warmup_iterations == 32 and
      .iterations > 0 and .elapsed_ns > 0 and (.sample >= 0 and .sample < 3) and
      (.input_sha256 | test("^[0-9a-f]{64}$")) and
      .checksum == (if .expected then .iterations else 0 end)) and
    (group_by(.case) | length == 5 and all(.[]; length == 3 and ([.[].sample] | sort) == [0,1,2]))
  ' "$out/$mode.jsonl" > "$out/validate-$mode.txt"
done
# A compiled wrong exported implementation must fail correctness before its
# timing rows can be treated as a qualified benchmark. This is a Zig fixture.
cat > "$work/always-clean.zig" <<'ZIG'
const BoolResult = extern struct { status: c_int, value: bool };
export fn proven_header_has_crlf(_: ?[*]const u8, _: usize) callconv(.c) BoolResult {
    return .{ .status = 0, .value = false };
}
ZIG
"$zig" build-obj "$work/always-clean.zig" -O ReleaseSafe -femit-bin="$work/always-clean.o" > "$out/build-control.txt" 2>&1
"$zig" build-exe "$work/header_boundary.zig" "$work/always-clean.o" -lc -O ReleaseSafe \
  -femit-bin="$work/wrong-benchmark" > "$out/build-control-caller.txt" 2>&1
if timeout -k 5 60 "$work/wrong-benchmark" > "$out/reject-always-clean.txt" 2>&1; then
  echo 'FAIL benchmark accepted incorrect library' >&2; exit 1
fi
grep -q 'WrongHeaderResult' "$out/reject-always-clean.txt"
(cd "$work"; sha256sum -c "$out/checked-inputs.sha256") > "$out/checked-input-recheck.txt"
sha256sum -c "$out/inputs.sha256" > "$out/input-recheck.txt"
cp "$work/header_boundary.zig" "$out/"
echo PASS > "$out/result.txt"
echo 'HEADER BENCHMARK PASS: correctness-guarded real library samples; no performance regression baseline claimed'
