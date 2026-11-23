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

mapfile -d '' -t sources < <(find "$source_root" -type f -name '*.idr' -print0 | LC_ALL=C sort -z)
if (( ${#sources[@]} == 0 )); then
  printf 'ERROR: no Idris2 sources found under %s\n' "$source_root" >&2
  exit 2
fi

# Track Idris block comments across lines, including nested comments. Remove
# comments and strings before scanning tokens to avoid counting documentation,
# inline comments or harmless quoted text as executable code. Any active token
# remains forbidden, with no permissive threshold or implicit waiver.
awk '
function code_of(line,    i, pair, ch, out, quoted, escaped) {
  out = ""; quoted = 0; escaped = 0
  if (line ~ /^[[:space:]]*\|\|\|/) return ""
  for (i = 1; i <= length(line); i++) {
    ch = substr(line, i, 1)
    pair = substr(line, i, 2)
    if (depth > 0) {
      if (pair == "{-") { depth++; i++; continue }
      if (pair == "-}") { depth--; i++; continue }
      continue
    }
    if (quoted) {
      if (escaped) { escaped = 0; continue }
      if (ch == "\\") { escaped = 1; continue }
      if (ch == "\"") { quoted = 0; continue }
      continue
    }
    if (pair == "--") break
    if (pair == "{-") { depth++; i++; continue }
    if (ch == "\"") { quoted = 1; continue }
    out = out ch
  }
  return out
}
FNR == 1 {
  if (NR > 1 && depth != 0) {
    print "ERROR: unterminated Idris block comment in previous file" > "/dev/stderr"
    errors++
  }
  depth = 0
  files++
}
{
  executable = code_of($0)
  if (executable ~ /(^|[^[:alnum:]_])(really_believe_me|believe_me|idris_crash)([^[:alnum:]_]|$)/) {
    printf "ERROR: active Idris trust escape at %s:%d: %s\n", FILENAME, FNR, executable > "/dev/stderr"
    errors++
  }
}
END {
  if (depth != 0) {
    print "ERROR: unterminated Idris block comment in final file" > "/dev/stderr"
    errors++
  }
  if (errors) {
    printf "FAIL: %d active trust escape(s) or lexical error(s)\n", errors > "/dev/stderr"
    exit 1
  }
  print "PASS: no active believe_me, really_believe_me, or idris_crash tokens in " files " Idris2 files"
}
' "${sources[@]}"
