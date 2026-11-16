#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
sholes=${1:?usage: check-header-proofs.sh /path/to/sholes-git-checkout}
pin=c28993a73d1c78830c6d92f563fae638d22bb7f7
evidence=${EVIDENCE_DIR:-"$root/build/header-proof-evidence"}
mkdir -p "$evidence"
evidence=$(cd "$evidence" && pwd)
rm -f "$evidence/result.txt"
work=''
finish() {
  result=$?
  if [[ $result -ne 0 ]]; then printf 'FAIL: exit %s\n' "$result" > "$evidence/result.txt"; fi
  if [[ -n "$work" ]]; then rm -rf -- "$work"; fi
}
trap finish EXIT
for tool in agda lean coqc coqchk git; do command -v "$tool" >/dev/null; done
[[ $(agda --version) == 'Agda version 2.6.4.3' ]]
lean --version | grep -q 'version 4.15.0,'
[[ $(coqc --version | head -1) == 'The Rocq Prover, version 9.2' ]]
work=$(mktemp -d -t proven-header-proof.XXXXXXXX)
# Only committed, pinned upstream proof sources enter the proof environment.
git -C "$sholes" archive "$pin" verification/proofs | tar -x -C "$work"
cp -R "$root/verification/header" "$work/header"
find "$work/header" -type f \( -name '*.agdai' -o -name '*.olean' -o -name '*.vo' \) -delete
base="$work/verification/proofs"
agda --safe --without-K --cubical-compatible --double-check --warning=error -i "$base/agda" -i "$work/header/agda" "$work/header/agda/ProvenHeader.agda" > "$evidence/agda.log" 2>&1
agda --safe --cubical --double-check --warning=error -i "$base/agda" -i "$work/header/agda" -i "$work/header/cubical" "$work/header/cubical/ProvenHeaderCubical.agda" > "$evidence/cubical.log" 2>&1
cp "$work/header/lean4/ProvenHeader.lean" "$base/lean4/"
(cd "$base/lean4"; lean -o Sholes/Core.olean Sholes/Core.lean) > "$evidence/lean-upstream.log" 2>&1
(cd "$base/lean4"; LEAN_PATH="$base/lean4" lean -o ProvenHeader.olean ProvenHeader.lean) > "$evidence/lean.log" 2>&1
[[ $(grep -c 'does not depend on any axioms' "$evidence/lean.log") == 4 ]]
if grep -Eq 'sorry|warning:|error:' "$evidence/lean.log"; then
  echo 'FAIL: Lean diagnostic audit' >&2; exit 1
fi
cp "$work/header/coq/ProvenHeader.v" "$base/coq/"
(cd "$base/coq"; coqc Sholes_Core.v; coqc Sholes_Wire.v) > "$evidence/coq-upstream.log" 2>&1
(cd "$base/coq"; coqc ProvenHeader.v) > "$evidence/coq.log" 2>&1
[[ $(grep -c 'Closed under the global context' "$evidence/coq.log") == 4 ]]
(cd "$base/coq"; coqchk -silent ProvenHeader) > "$evidence/coqchk.log" 2>&1

# Explicitly false witnesses must be rejected, not skipped or admitted.
cat > "$work/Reject.agda" <<'AGDA'
{-# OPTIONS --safe --without-K --cubical-compatible #-}
module Reject where
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality
bad : true ≡ false
bad = refl
AGDA
if agda --safe --cubical-compatible --double-check -i "$work" "$work/Reject.agda" > "$evidence/agda-reject.log" 2>&1; then exit 1; fi
grep -q 'true != false' "$evidence/agda-reject.log"
echo 'example : True = False := rfl' > "$work/Reject.lean"
if lean "$work/Reject.lean" > "$evidence/lean-reject.log" 2>&1; then exit 1; fi
grep -q 'error:' "$evidence/lean-reject.log"
echo 'Goal true = false. Proof. reflexivity. Qed.' > "$work/Reject.v"
if coqc "$work/Reject.v" > "$evidence/coq-reject.log" 2>&1; then exit 1; fi
grep -q 'Unable to unify' "$evidence/coq-reject.log"
{
  printf 'Sholes: %s\n' "$pin"
  agda --version; lean --version; coqc --version
  (cd "$work/header"; find . -type f \( -name '*.agda' -o -name '*.lean' -o -name '*.v' \) -exec sha256sum {} +)
} > "$evidence/identity.log"
echo PASS > "$evidence/result.txt"
printf 'PASS: compatible Agda; full Cubical; Lean axiom audit; Rocq assumption audit and coqchk; three rejecting controls.\nEvidence: %s\n' "$evidence"
