#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Check the C ABI status ledger against the target header and a real library.
#
#   check-abi-ledger.sh LEDGER HEADER LIBRARY [OUT_DIR]
#
# Fails when: a ledger row is malformed or duplicated; a header function has
# no exported/conflict/owed row, or such a row names no header function; an
# undeclared row is in the header; an exported, conflict or undeclared row is
# not defined by the library; an owed row IS defined (the ledger is stale and
# the row must be reclassified with its implementation); or the library
# defines a proven_* symbol with no row.
#
# Symbol presence is checked, not C signatures, layouts or ownership.
# Exit 0 PASS, 1 FAIL, 2 NOT RUN (missing or unreadable input).
set -euo pipefail
unset CDPATH
if [[ $# -lt 3 ]]; then echo 'usage: check-abi-ledger.sh LEDGER HEADER LIBRARY [OUT_DIR]' >&2; exit 2; fi
ledger=$1 header=$2 library=$3
out=${4:-$(mktemp -d -t proven-abi-ledger.XXXXXXXX)}
for f in "$ledger" "$header" "$library"; do
  [[ -s $f ]] || { echo "NOT RUN: missing or empty input: $f" >&2; exit 2; }
done
mkdir -p "$out"
cc=${CC:-cc}
fail=0
bad() { echo "FAIL: $*" >&2; fail=1; }

# Rows: symbol, status, backend, note. Comments and blank lines are ignored.
grep -vE '^(#|[[:space:]]*$)' "$ledger" > "$out/rows.tsv" || true
[[ -s $out/rows.tsv ]] || { echo "NOT RUN: ledger has no rows" >&2; exit 2; }
awk -F'\t' '
  NF != 4 { printf "line %d: expected 4 tab-separated columns, got %d\n", NR, NF; next }
  $1 !~ /^proven_[a-z0-9_]+$/ { printf "%s: malformed symbol\n", $1 }
  $2 !~ /^(exported|conflict|owed|undeclared)$/ { printf "%s: unknown status %s\n", $1, $2 }
  $3 !~ /^(idris-refc|zig-legacy|-)$/ { printf "%s: unknown backend %s\n", $1, $3 }
  $2 == "owed" && $3 != "-" { printf "%s: owed row names a backend\n", $1 }
  $2 != "owed" && $3 == "-" { printf "%s: implemented row has no backend\n", $1 }
  seen[$1]++ == 1 { printf "%s: duplicate row\n", $1 }
' "$out/rows.tsv" > "$out/malformed.txt"
if [[ -s $out/malformed.txt ]]; then bad "malformed ledger"; cat "$out/malformed.txt" >&2; fi

pick() { awk -F'\t' -v s="$1" '$2 == s { print $1 }' "$out/rows.tsv" | sort -u; }
pick exported > "$out/exported.txt"
pick conflict > "$out/conflict.txt"
pick owed > "$out/owed.txt"
pick undeclared > "$out/undeclared.txt"
sort -u "$out/exported.txt" "$out/conflict.txt" "$out/owed.txt" > "$out/header-rows.txt"
sort -u "$out/exported.txt" "$out/conflict.txt" "$out/undeclared.txt" > "$out/present-rows.txt"
cut -f1 "$out/rows.tsv" | sort -u > "$out/all-rows.txt"

# Preprocess so comments and prose mentions are not mistaken for declarations.
if ! "$cc" -E -P -x c "$header" -o "$out/header.i" 2> "$out/header-cpp.log"; then
  cat "$out/header-cpp.log" >&2; echo "NOT RUN: header does not preprocess" >&2; exit 2
fi
grep -oE '\bproven_[a-z0-9_]+[[:space:]]*\(' "$out/header.i" | sed -E 's/[[:space:]]*\($//' | sort -u > "$out/header.txt"
[[ -s $out/header.txt ]] || { echo "NOT RUN: header declares no proven_ functions" >&2; exit 2; }

# GNU nm -D can succeed with an empty result for an archive. Select the symbol
# table by artifact kind; never fall back to non-dynamic symbols of a shared
# library, where hidden definitions must not satisfy the public ABI ledger.
if ar t "$library" > "$out/archive-members.txt" 2> "$out/archive.log"; then
  nm_args=(--defined-only)
else
  nm_args=(-D --defined-only)
fi
if ! nm "${nm_args[@]}" "$library" > "$out/nm.txt" 2> "$out/nm.log"; then
  cat "$out/nm.log" >&2; echo "NOT RUN: nm cannot read $library" >&2; exit 2
fi
awk '$2 ~ /^[TW]$/ && $3 ~ /^proven_/ { print $3 }' "$out/nm.txt" | sort -u > "$out/library.txt"
[[ -s $out/library.txt ]] || { echo "NOT RUN: library defines no proven_ symbols" >&2; exit 2; }

check() { # name, comm flags, left, right
  comm "$2" "$3" "$4" > "$out/$1.txt"
  if [[ -s $out/$1.txt ]]; then bad "$1 ($(wc -l < "$out/$1.txt"))"; sed 's/^/  /' "$out/$1.txt" >&2; fi
}
check header-without-row       -23 "$out/header.txt"       "$out/header-rows.txt"
check row-not-in-header        -13 "$out/header.txt"       "$out/header-rows.txt"
check undeclared-in-header     -12 "$out/header.txt"       "$out/undeclared.txt"
check present-row-not-exported -13 "$out/library.txt"      "$out/present-rows.txt"
check owed-row-exported        -12 "$out/library.txt"      "$out/owed.txt"
check export-without-row       -23 "$out/library.txt"      "$out/all-rows.txt"

printf 'header=%s library=%s exported=%s conflict=%s owed=%s undeclared=%s\n' \
  "$(wc -l < "$out/header.txt")" "$(wc -l < "$out/library.txt")" "$(wc -l < "$out/exported.txt")" \
  "$(wc -l < "$out/conflict.txt")" "$(wc -l < "$out/owed.txt")" "$(wc -l < "$out/undeclared.txt")" | tee "$out/summary.txt"
sha256sum "$ledger" "$header" "$library" > "$out/identity.sha256"
if ((fail)); then echo FAIL > "$out/result.txt"; exit 1; fi
echo PASS > "$out/result.txt"
echo "PASS: ABI ledger matches $(basename "$header") and $(basename "$library")"
