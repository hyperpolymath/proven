#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Deliberately planted false-negative controls for the Idris escape gate.
set -euo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
scanner="$here/check-idris-escape-hatches.sh"
work=$(mktemp -d -t proven-escape-controls.XXXXXXXX)
trap 'rm -rf -- "$work"' EXIT
mkdir -p "$work/src"
source_file="$work/src/Control.idr"

expect_pass() {
  if ! bash "$scanner" "$work/src" > "$work/stdout" 2> "$work/stderr"; then
    printf 'FAIL: expected scanner success for %s\n' "$1" >&2
    cat "$work/stderr" >&2
    exit 1
  fi
}
expect_fail() {
  if bash "$scanner" "$work/src" > "$work/stdout" 2> "$work/stderr"; then
    printf 'FAIL: scanner accepted planted %s\n' "$1" >&2
    exit 1
  fi
  grep -q 'active Idris trust escape' "$work/stderr" || {
    printf 'FAIL: wrong failure mechanism for %s\n' "$1" >&2
    cat "$work/stderr" >&2
    exit 1
  }
}

cat > "$source_file" <<'IDR'
module Control
||| Documentation mentioning believe_me, really_believe_me and idris_crash
-- Ordinary comment: believe_me
{- block comment: believe_me
   {- nested: idris_crash -}
   really_believe_me -}
x = "believe_me and idris_crash"
y = x -- really_believe_me
IDR
expect_pass 'comments and string literals'

for token in believe_me really_believe_me idris_crash; do
  printf 'module Control\nvalue = %s 42\n' "$token" > "$source_file"
  expect_fail "$token"
done

cat > "$source_file" <<'IDR'
module Control
value = pure 42 -- a mention: believe_me
{- comment open
   really_believe_me
   comment close -}
value2 = "not an idris_crash call"
IDR
expect_pass 'inline comments and multiline block comments'

rm -f "$source_file"
if bash "$scanner" "$work/src" >/dev/null 2>&1; then
  echo 'FAIL: scanner accepts a missing source census' >&2
  exit 1
fi
printf 'PASS: Idris escape gate negative and positive controls\n'
