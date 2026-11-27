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

for token in believe_me really_believe_me idris_crash prim__believe_me prim__crash; do
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


# Controls for each lexer hazard: active escapes that a naive scanner hides.
plant_fail() {  # name, body
  printf 'module Control\n%s\n' "$2" > "$source_file"
  expect_fail "$1"
}
plant_pass() {
  printf 'module Control\n%s\n' "$2" > "$source_file"
  expect_pass "$1"
}
plant_fail 'escape after a double-quote character literal' "f : Char -> Nat
f '\"' = believe_me 42"
plant_fail 'escape inside string interpolation' 'x = "forged: \{believe_me value}"'
plant_fail 'direct primitive with erased arguments' 'v = prim__believe_me _ _ value'
plant_fail 'escape after a multiline string closes' 's = """
text
"""
v = believe_me 1'
plant_pass 'forbidden name inside a multiline string interior' 's = """
believe_me idris_crash
"""'
plant_pass 'primed identifier is not a character literal' "g' = x'"
printf 'module Control\nv = believe_me 1\n' > "$work/Real.idr"
rm -f "$source_file"
mkdir -p "$work/outside"
mv "$work/Real.idr" "$work/outside/Real.idr"
ln -s ../outside/Real.idr "$work/src/Evil.idr"
expect_fail 'symlinked source outside the root'
rm -f "$work/src/Evil.idr"
ln -s ../outside/Missing.idr "$work/src/Dangling.idr"
if bash "$scanner" "$work/src" >/dev/null 2>&1; then echo 'FAIL: dangling symlink accepted' >&2; exit 1; fi
rm -f "$work/src/Dangling.idr"
printf 'Prose.\n\n> v = believe_me 1\n' > "$work/src/Lit.lidr"
expect_fail 'literate Idris source'
rm -f "$work/src/Lit.lidr"
printf 'module Control\n' > "$source_file"
rm -f "$source_file"
if bash "$scanner" "$work/src" >/dev/null 2>&1; then
  echo 'FAIL: scanner accepts a missing source census' >&2
  exit 1
fi
printf 'PASS: Idris escape gate negative and positive controls\n'
