#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
mkdir -p "$work/src"
cat > "$work/src/Example.idr" <<'IDR'
module Example
||| DISCHARGED: an annotation is not a proof
ok : 1 = 1
ok = Refl
||| OWED: a missing proof
0 missing : 2 = 2
||| OWED-AXIOM: external dependency
%default covering
IDR
bash "$here/census-idris-proof-claims.sh" "$work/src" "$work/report.tsv" > "$work/report.log"
for marker in CLAIMED_DISCHARGED CLAIMED_OWED CLAIMED_OWED_AXIOM ERASED_SIGNATURE_CANDIDATE DEFAULT_COVERING; do
  test "$(awk -F '\t' -v kind="$marker" '$1 == kind {n++} END {print n+0}' "$work/report.tsv")" = 1
done
test "$(grep -c '^# NOTE: lexical evidence only' "$work/report.tsv")" = 1
rm "$work/src/Example.idr"
if bash "$here/census-idris-proof-claims.sh" "$work/src" "$work/report.tsv" >/dev/null 2>&1; then
  echo "FAIL: empty input was accepted" >&2
  exit 1
fi
echo "PASS: lexical proof claims census positive and negative controls"
