#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Planted controls for check-abi-ledger.sh: one accepting fixture and one
# rejecting fixture per failure class, each against a really compiled library.
set -euo pipefail
unset CDPATH
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
checker="$root/scripts/check-abi-ledger.sh"
cc=${CC:-cc}
work=$(mktemp -d -t proven-abi-ledger-test.XXXXXXXX)
trap 'rm -rf -- "$work"' EXIT

cat > "$work/h.h" <<'C'
/* proven_mentioned_only(void) appears in a comment and is not declared. */
int proven_a(void);
int proven_b(void);
int proven_owed_one(void);
C
lib() { # name, symbols...
  local name=$1; shift
  : > "$work/$name.c"
  for s in "$@"; do printf 'int %s(void) { return 0; }\n' "$s" >> "$work/$name.c"; done
  "$cc" -shared -fPIC -o "$work/$name.so" "$work/$name.c"
}
ledger() { # name, rows (symbol:status:backend)...
  local name=$1; shift
  printf '# fixture\n' > "$work/$name.tsv"
  for r in "$@"; do IFS=: read -r s st b <<< "$r"; printf '%s\t%s\t%s\t-\n' "$s" "$st" "$b" >> "$work/$name.tsv"; done
}
expect() { # expected-exit, label, ledger, library
  local want=$1 label=$2 got=0
  bash "$checker" "$work/$3.tsv" "$work/h.h" "$work/$4.so" "$work/out-$label" > "$work/$label.log" 2>&1 || got=$?
  if [[ $got != "$want" ]]; then cat "$work/$label.log" >&2; echo "FAIL: control $label exited $got, expected $want" >&2; exit 1; fi
  echo "ok   $label (exit $got)"
}
good=(proven_a:exported:idris-refc proven_b:exported:zig-legacy proven_owed_one:owed:- proven_extra:undeclared:zig-legacy)
lib real proven_a proven_b proven_extra
ledger good "${good[@]}"
expect 0 accepts-consistent-ledger good real

lib missing proven_a proven_extra
expect 1 rejects-missing-export good missing

lib stale proven_a proven_b proven_extra proven_owed_one
expect 1 rejects-owed-symbol-present good stale

lib unledgered proven_a proven_b proven_extra proven_surprise
expect 1 rejects-unledgered-export good unledgered

ledger norow proven_a:exported:idris-refc proven_b:exported:zig-legacy proven_extra:undeclared:zig-legacy
expect 1 rejects-header-function-without-row norow real

ledger badstatus proven_a:exported:idris-refc proven_b:proven:zig-legacy proven_owed_one:owed:- proven_extra:undeclared:zig-legacy
expect 1 rejects-unknown-status badstatus real

ledger owedbackend proven_a:exported:idris-refc proven_b:exported:zig-legacy proven_owed_one:owed:zig-legacy proven_extra:undeclared:zig-legacy
expect 1 rejects-owed-with-backend owedbackend real

expect 2 not-run-without-library good absent
echo 'PASS: ABI ledger checker accepts 1 consistent fixture, rejects 6 planted faults, reports NOT RUN for 1 missing input'
