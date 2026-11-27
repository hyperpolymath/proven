#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Run only under a validation slot or on a hosted runner.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
idris="${IDRIS2:-idris2}"
base="${PATH_PROOF_EVIDENCE:-$root/build/path-proof-evidence}"
mkdir -p "$base"
out="$(mktemp -d "$base/run.XXXXXXXX")"
printf 'Path proof evidence: %s\n' "$out"
exec > >(tee "$out/run.log") 2>&1
mkdir -p "$out/source/Proven/SafePath"
cd "$root"
sha256sum src/Proven/SafePath/ByteKernel.idr tests/remediation/PathProofRegressions.idr \
  tests/remediation/PathMain.idr tests/remediation/negative/ForgedPath.idr \
  tests/remediation/negative/MalformedPathState.idr scripts/test-path-proofs.sh > "$out/inputs.sha256"
cp src/Proven/SafePath/ByteKernel.idr "$out/source/Proven/SafePath/"
cp tests/remediation/PathProofRegressions.idr tests/remediation/PathMain.idr \
  tests/remediation/negative/ForgedPath.idr tests/remediation/negative/MalformedPathState.idr "$out/source/"
"$idris" --version > "$out/toolchain.txt"
cd "$out/source"
check_compiler_log() {
  if grep -Eq '^Error:|Warning: compiling hole|Unsolved holes' "$1"; then
    cat "$1" >&2; exit 1
  fi
}
"$idris" --no-color --total -Werror --check PathProofRegressions.idr > "$out/proofs.log" 2>&1
check_compiler_log "$out/proofs.log"
printf ':m\n:q\n' | "$idris" --no-banner PathProofRegressions.idr > "$out/holes.log" 2>&1
check_compiler_log "$out/holes.log"
grep -q 'No holes' "$out/holes.log"
for fixture in ForgedPath MalformedPathState; do
  if "$idris" --no-color --total -Werror --check "$fixture.idr" > "$out/$fixture.log" 2>&1; then
    echo "FAIL: $fixture unexpectedly proved"; exit 1
  else
    code=$?
  fi
  if [[ $code -ne 1 ]]; then echo "FAIL: $fixture exited $code, not semantic-error exit 1"; exit 1; fi
  grep -Eq "Mismatch between:|Can't solve constraint between:" "$out/$fixture.log"
  echo "PASS: semantic rejection $fixture"
done
"$idris" --no-color --total -Werror -o path-regressions PathMain.idr > "$out/runtime-build.log" 2>&1
check_compiler_log "$out/runtime-build.log"
./build/exec/path-regressions | tee "$out/runtime.log"
cd "$root"
sha256sum --check --quiet "$out/inputs.sha256"
echo 'PASS: total path policy/refinement proofs, two semantic refusals and runtime component oracle'
