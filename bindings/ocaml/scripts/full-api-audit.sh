#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Full-API audit for the eager OCaml binding against the C ABI status ledger
# (proven-abi.tsv beside proven.h) and a real libproven.so.
#
# Fails when the binding binds a symbol that is not a ledger `exported` row,
# a bound symbol is absent from the library, a ledger `exported` row is
# absent from the library, or a ledger `owed` row is present in it (the
# ledger is stale and must be reclassified together with the code).
# Presence alone does not prove a C signature, ownership contract or Idris
# implementation; full_api_probe stays as the load test.
set -euo pipefail
unset CDPATH
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
: "${PROVEN_LIB_PATH:?directory containing the actual libproven.so required}"
include=${PROVEN_INCLUDE_PATH:-"$root/../c/include"}
library="$PROVEN_LIB_PATH/libproven.so"
ledger="$include/proven-abi.tsv"
test -s "$library"
test -s "$ledger"
out="$root/_build/full-api-audit"
mkdir -p "$out"
echo INCOMPLETE > "$out/result.txt"
trap 'status=$?; if ((status)); then echo FAIL > "$out/result.txt"; fi' EXIT
sed -nE 's/.*foreign ~from:lib "([^"]+)".*/\1/p' "$root/lib/proven_ffi.ml" | sort -u > "$out/required.txt"
test -s "$out/required.txt"
grep -vE '^(#|[[:space:]]*$)' "$ledger" | awk -F'\t' '$2 == "exported" { print $1 }' | sort -u > "$out/ledger-exported.txt"
grep -vE '^(#|[[:space:]]*$)' "$ledger" | awk -F'\t' '$2 == "owed" { print $1 }' | sort -u > "$out/ledger-owed.txt"
test -s "$out/ledger-exported.txt"
nm -D --defined-only "$library" | awk '$2 ~ /^[TW]$/ {print $3}' | sort -u > "$out/exported.txt"
comm -23 "$out/required.txt" "$out/ledger-exported.txt" > "$out/bound-not-ledger-exported.txt"
comm -23 "$out/required.txt" "$out/exported.txt" > "$out/missing.txt"
comm -23 "$out/ledger-exported.txt" "$out/exported.txt" > "$out/ledger-exported-missing.txt"
comm -12 "$out/ledger-owed.txt" "$out/exported.txt" > "$out/owed-present.txt"
comm -12 "$out/required.txt" "$out/exported.txt" > "$out/present.txt"
sha256sum "$root/lib/proven_ffi.ml" "$ledger" "$library" > "$out/identity.sha256"
printf 'Required=%s Present=%s Missing=%s LedgerExported=%s LedgerOwed=%s\n' \
  "$(wc -l < "$out/required.txt")" "$(wc -l < "$out/present.txt")" "$(wc -l < "$out/missing.txt")" \
  "$(wc -l < "$out/ledger-exported.txt")" "$(wc -l < "$out/ledger-owed.txt")" | tee "$out/summary.txt"
fail=0
for f in bound-not-ledger-exported missing ledger-exported-missing owed-present; do
  if [[ -s "$out/$f.txt" ]]; then echo "FAIL: $f"; sed 's/^/  /' "$out/$f.txt"; fail=1; fi
done
((fail == 0))
echo PASS > "$out/result.txt"
