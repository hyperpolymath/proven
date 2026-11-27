#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Fail closed on active Idris2 trust escapes. This is a lexical gate,
# not a theorem checker; a clean run does not discharge any proof.
set -euo pipefail

source_root=${1:-src}
if [[ ! -d "$source_root" ]]; then
  printf 'ERROR: Idris source root not found: %s\n' "$source_root" >&2
  exit 2
fi

# Follow symlinks (-L): a package-listed source may be a link to a tracked file
# outside the root. A dangling link is an error, never a silent skip. Literate
# Idris (.lidr) is included and scanned raw (every line is treated as code), which
# over-approximates and so fails closed.
mapfile -d '' -t dangling < <(find -L "$source_root" -type l -print0)
if (( ${#dangling[@]} > 0 )); then
  printf 'ERROR: dangling symlink under %s: %s\n' "$source_root" "${dangling[0]}" >&2
  exit 2
fi
mapfile -d '' -t sources < <(find -L "$source_root" -type f \( -name '*.idr' -o -name '*.lidr' \) -print0 | LC_ALL=C sort -z)
if (( ${#sources[@]} == 0 )); then
  printf 'ERROR: no Idris2 sources found under %s\n' "$source_root" >&2
  exit 2
fi

# Lexer state persists across lines: nested block comments, single-line strings,
# triple-quoted multiline strings, and \{ ... } interpolation (code inside a
# string, with its own brace depth and nested strings). Character literals are
# consumed before strings so '"' cannot open one. Comments, doc comments and
# string text are dropped; every remaining token is scanned. No waiver.
awk '
function reset_file() { depth = 0; instr = 0; sp = 0 }
function code_of(line, lit,    i, n, ch, pair, tri, out, prev, j) {
  out = ""; n = length(line)
  if (lit) return line
  if (!instr && depth == 0 && line ~ /^[[:space:]]*\|\|\|/) return ""
  for (i = 1; i <= n; i++) {
    ch = substr(line, i, 1); pair = substr(line, i, 2); tri = substr(line, i, 3)
    if (depth > 0) {
      if (pair == "{-") { depth++; i++ } else if (pair == "-}") { depth--; i++ }
      continue
    }
    if (instr == 1 || instr == 2) {
      if (ch == "\\") {
        if (substr(line, i + 1, 1) == "{") {          # interpolation: back to code
          stk[++sp] = instr; brace[sp] = 1; instr = 0; i++
        } else i++
        continue
      }
      if (instr == 2 && tri == "\"\"\"") { instr = 0; i += 2; continue }
      if (instr == 1 && ch == "\"") { instr = 0 }
      continue
    }
    if (pair == "--") break
    if (pair == "{-") { depth++; i++; continue }
    if (sp > 0 && ch == "{") { brace[sp]++ }
    if (sp > 0 && ch == "}") { brace[sp]--; if (brace[sp] == 0) { instr = stk[sp]; sp--; continue } }
    if (tri == "\"\"\"") { instr = 2; i += 2; continue }
    if (ch == "\"") { instr = 1; continue }
    if (ch == "\047") {                                  # character literal, not f'"'"'
      prev = (i > 1) ? substr(line, i - 1, 1) : " "
      if (prev !~ /[[:alnum:]_\047]/) {
        if (substr(line, i + 1, 1) == "\\") {
          j = index(substr(line, i + 2), "\047")
          if (j > 0) { i += j + 1; continue }
        } else if (substr(line, i + 2, 1) == "\047") { i += 2; continue }
      }
    }
    out = out ch
  }
  if (instr == 1 && sp == 0) instr = 0                   # unterminated single-line string ends at EOL
  return out
}
FNR == 1 {
  if (NR > 1 && (depth != 0 || instr != 0)) {
    print "ERROR: unterminated Idris comment or string in previous file" > "/dev/stderr"
    errors++
  }
  reset_file(); files++
  literate = (FILENAME ~ /\.lidr$/)
}
{
  executable = code_of($0, literate)
  if (executable ~ /(^|[^[:alnum:]_])(really_believe_me|believe_me|idris_crash|prim__believe_me|prim__crash)([^[:alnum:]_]|$)/) {
    printf "ERROR: active Idris trust escape at %s:%d: %s\n", FILENAME, FNR, executable > "/dev/stderr"
    errors++
  }
}
END {
  if (depth != 0 || instr != 0) {
    print "ERROR: unterminated Idris comment or string in final file" > "/dev/stderr"
    errors++
  }
  if (errors) {
    printf "FAIL: %d active trust escape(s) or lexical error(s)\n", errors > "/dev/stderr"
    exit 1
  }
  print "PASS: no active trust-escape tokens in " files " Idris2 files"
}
' "${sources[@]}"
