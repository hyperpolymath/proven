#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# Fresh source-only staging prevents installed proven packages / old TTC files
# from standing in for the implementation under review.
set -euo pipefail
repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="${IDRIS2:-idris2}"
scratch="$(mktemp -d -t proven-critical.XXXXXXXX)"
trap 'rm -rf -- "$scratch"' EXIT
cp -R -- "$repo_root/src/Proven" "$scratch/Proven"
cp -- "$repo_root"/tests/remediation/*.idr "$scratch/"
cd -- "$scratch"
checked_compile() {
  "$compiler" -p contrib -p network --no-color "$@" 2>&1 | tee "$scratch/compiler.log"
  # Some Idris import failures have returned zero; diagnostics must fail closed.
  if grep -Eq '^Error:|Warning: compiling hole|Unsolved holes' "$scratch/compiler.log"; then
    echo "FAIL: compiler reported an error or an unfilled hole" >&2
    exit 1
  fi
}
checked_compile --total --check ProofRegressions.idr
checked_compile --total --check JWTSQLProofRegressions.idr
checked_compile --total --check RandomProofRegressions.idr
checked_compile --total --check LifecycleProofRegressions.idr
checked_compile --total -o critical-regressions Main.idr
./build/exec/critical-regressions
checked_compile --total -o jwt-sql-regressions JWTSQLMain.idr
./build/exec/jwt-sql-regressions
checked_compile --total -o random-regressions RandomMain.idr
./build/exec/random-regressions
# Exercise native RefC generation too; this does not certify the Zig/C ABI.
checked_compile --cg refc --total -o random-regressions-refc RandomMain.idr
./build/exec/random-regressions-refc
checked_compile --total -o entropy-failure-probe EntropyFailureProbe.idr
./build/exec/entropy-failure-probe
checked_compile --total -o lifecycle-regressions LifecycleMain.idr
./build/exec/lifecycle-regressions
./build/exec/jwt-sql-regressions --sqlite > "$scratch/literals.sql"
julia="${JULIA:-julia}"
"$julia" --startup-file=no "$repo_root/tests/remediation/sqlite-literals.jl" "$scratch/literals.sql"
for fixture in "$repo_root"/tests/remediation/negative/*.idr; do
  name="$(basename -- "$fixture")"
  cp -- "$fixture" "$scratch/$name"
  if "$compiler" -p contrib -p network --no-color --total --check "$name" > "$scratch/negative.log" 2>&1; then
    echo "FAIL: forged evidence typechecked: $name" >&2
    exit 1
  fi
  # Reject type/constructor mismatches, not a missing compiler/import/syntax error.
  if ! grep -Eq "Mismatch between:|Can't solve constraint between:" "$scratch/negative.log"; then
    cat "$scratch/negative.log" >&2
    echo "FAIL: unexpected negative-test diagnostic: $name" >&2
    exit 1
  fi
  echo "PASS: type checker rejected $name"
done
