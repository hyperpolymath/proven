#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
set -euo pipefail
root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
idris="${IDRIS2:-idris2}"
zig="${ZIG:-zig}"
scratch="$(mktemp -d -t proven-native-remediation.XXXXXXXX)"
trap 'rm -rf -- "$scratch"' EXIT
cp -R "$root/src/Proven" "$scratch/Proven"
cp "$root/tests/remediation/NativeNonceFixture.idr" "$scratch/"
cd "$scratch"
"$idris" -p contrib -p network --no-color --total --cg refc -o nonce-fixture NativeNonceFixture.idr 2>&1 | tee compiler.log
if grep -Eq '^Error:|Warning: compiling hole|Unsolved holes' compiler.log; then exit 1; fi
[[ -s build/exec/nonce-fixture.c ]]
prefix="$("$idris" --prefix)"
runtime="$(find "$prefix" -path '*/support/refc' -type d -print -quit)"
support="$(find "$prefix" -path '*/support/c' -type d -print -quit)"
support_lib="$(find "$prefix" -name libidris2_support.a -type f -print -quit)"
[[ -f "$runtime/libidris2_refc.a" ]]
[[ -f "$support/idris_support.h" ]]
[[ -n "$support_lib" ]]
cd "$root/ffi/zig"
"$zig" build remediation-test -j1 -Didris-refc="$scratch/build/exec" \
  -Didris-refc-runtime="$runtime" -Didris-c-support="$support" \
  -Didris-support-lib="$(dirname "$support_lib")" --summary all

# A missing/empty source tree must never yield a misleading native build.
# Check the diagnostic as well as the exit code so an unrelated failure is not
# accepted as the expected control.
ulimit -c 0
expect_build_rejection() {
  local label="$1" expected="$2"
  shift 2
  if "$zig" build -j1 "$@" > "$scratch/$label.log" 2>&1; then
    echo "FAIL: $label unexpectedly built" >&2
    exit 1
  fi
  grep -Fq -- "$expected" "$scratch/$label.log"
  echo "PASS: $label rejected with the required diagnostic"
}
common=("-Didris-refc-runtime=$runtime" "-Didris-c-support=$support"
        "-Didris-support-lib=$(dirname "$support_lib")")
mkdir "$scratch/empty"
expect_build_rejection missing-options "requires all four Idris RefC paths"
expect_build_rejection empty-sources "empty RefC source directory" \
  "-Didris-refc=$scratch/empty" "${common[@]}"
expect_build_rejection missing-sources "missing RefC source directory" \
  "-Didris-refc=$scratch/missing" "${common[@]}"
