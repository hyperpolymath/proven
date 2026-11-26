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
cat > "$work/src/Literals.idr" <<'IDR'
module Literals
{-
  {- believe_me -}
  ||| idris_crash -}
afterComment = assert_smaller 2 1
multi = """
  assert_total {- " \"" -- idris_crash
  ||| believe_me
  """
afterMulti = assert_smaller 2 1
raw = ##"""
  assert_total """# believe_me -- \{idris_crash}
  """## ++ idris_crash "after the matching delimiter"
interp = "assert_total \{assert_total "believe_me"} idris_crash"
escaped = "\\{assert_total x}"
rawInterp = #"\#{ "idris_crash \{(assert_smaller 2 1)}" } believe_me"#
multilineInterp = """
  \{
    {- believe_me -} show (the Nat (f {n = 1} (assert_smaller 2 1)))
  } assert_total
  """
char = ('"', believe_me 1)
escapedChar = ('\"', believe_me 1)
primed' = assert_smaller 2 1
IDR
bash "$here/census-idris-proof-claims.sh" "$work/src" "$work/report.tsv" > "$work/report.log"
awk -F '\t' 'NR > 6 {print $1, $3}' "$work/report.tsv" > "$work/actual"
diff -u - "$work/actual" <<'EXPECTED'
ASSERT_SMALLER_ACTIVE 5
ASSERT_SMALLER_ACTIVE 10
IDRIS_CRASH_ACTIVE 13
ASSERT_TOTAL_ACTIVE 14
ASSERT_SMALLER_ACTIVE 16
ASSERT_SMALLER_ACTIVE 19
BELIEVE_ME_ACTIVE 22
BELIEVE_ME_ACTIVE 23
ASSERT_SMALLER_ACTIVE 24
EXPECTED
rm "$work/src/Literals.idr"
cat > "$work/src/Inactive.idr" <<'IDR'
module Inactive
{-
||| DISCHARGED: inside a block comment
||| OWED: inside a block comment
||| x @ Assumed: inside a block comment
-}
s = """
||| OWED-AXIOM: inside a string
"""
||| DISCHARGED: this one is active
y = 1
IDR
bash "$here/census-idris-proof-claims.sh" "$work/src" "$work/report.tsv" > "$work/report.log"
awk -F '\t' 'NR > 6 {print $1, $3}' "$work/report.tsv" | diff -u - <(printf 'CLAIMED_DISCHARGED 10\n')
rm "$work/src/Inactive.idr"
if bash "$here/census-idris-proof-claims.sh" "$work/src" "$work/report.tsv" >/dev/null 2>&1; then
  echo "FAIL: empty input was accepted" >&2
  exit 1
fi
echo "PASS: lexical proof claims census positive and negative controls"
