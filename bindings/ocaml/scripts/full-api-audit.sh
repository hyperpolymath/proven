#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
unset CDPATH
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
: "${PROVEN_LIB_PATH:?directory containing the actual libproven.so required}"
library="$PROVEN_LIB_PATH/libproven.so"
test -s "$library"
out="$root/_build/full-api-audit"
mkdir -p "$out"
echo INCOMPLETE > "$out/result.txt"
trap 'status=$?; if ((status)); then echo FAIL > "$out/result.txt"; fi' EXIT
# This inventory is distinct from the five-symbol qualified wired manifest.
# Every eagerly bound symbol must exist. Presence alone does not prove a C
# signature, ownership contract, or Idris implementation; full_api_probe stays.
sed -nE 's/.*foreign ~from:lib "([^"]+)".*/\1/p' "$root/lib/proven_ffi.ml" | sort -u > "$out/required.txt"
test -s "$out/required.txt"
nm -D --defined-only "$library" | awk '$2 ~ /^[TW]$/ {print $3}' | sort -u > "$out/exported.txt"
comm -23 "$out/required.txt" "$out/exported.txt" > "$out/missing.txt"
comm -12 "$out/required.txt" "$out/exported.txt" > "$out/present.txt"
sha256sum "$root/lib/proven_ffi.ml" "$library" > "$out/identity.sha256"
printf 'Required=%s Present=%s Missing=%s\n' "$(wc -l < "$out/required.txt")" \
  "$(wc -l < "$out/present.txt")" "$(wc -l < "$out/missing.txt")" | tee "$out/summary.txt"
if [[ -s "$out/missing.txt" ]]; then
  cat "$out/missing.txt"; exit 1
fi
echo PASS > "$out/result.txt"
