#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Inventory source annotations and lexical proof-risk candidates; NOT proof results.
# Usage: bash scripts/census-idris-proof-claims.sh [src-directory] [report.tsv]
set -euo pipefail
root=${1:-src}
output=${2:-build/proof-census/idris-source.tsv}
[[ -d "$root" ]] || { echo "ERROR: missing source directory: $root" >&2; exit 2; }
mapfile -d '' -t sources < <(find "$root" -type f -name '*.idr' -print0 | LC_ALL=C sort -z)
(( ${#sources[@]} > 0 )) || { echo "ERROR: empty Idris source population" >&2; exit 2; }
commit=$(git rev-parse HEAD 2>/dev/null || printf 'unknown')
mkdir -p "$(dirname "$output")"
{
  printf '# proven-idris-proof-claims-v1\n'
  printf '# commit\t%s\n' "$commit"
  printf '# root\t%s\n' "$root"
  printf '# files\t%s\n' "${#sources[@]}"
  printf '# NOTE: lexical evidence only; claimed DISCHARGED is never proof evidence\n'
  printf 'kind\tfile\tline\ttext\n'
} > "$output"
for file in "${sources[@]}"; do
  [[ "$file" != *$'\n'* && "$file" != *$'\t'* ]] || { echo 'ERROR: unsupported filename for TSV report' >&2; exit 2; }
  awk -v file="$file" '
    function emit(kind, line,   x) {
      x = line
      gsub(/\t/, " ", x)
      sub(/\r$/, "", x)
      printf "%s\t%s\t%d\t%s\n", kind, file, FNR, substr(x, 1, 220)
    }
    function code_of(line, i, pair, ch, out, quote, escaped) {
      out = ""; quote = 0; escaped = 0
      if (line ~ /^[[:space:]]*\|\|\|/) return ""
      for (i = 1; i <= length(line); i++) {
        pair = substr(line, i, 2)
        ch = substr(line, i, 1)
        if (depth > 0) {
          if (pair == "{-") { depth++; i++; continue }
          if (pair == "-}") { depth--; i++; continue }
          continue
        }
        if (quote) {
          if (escaped) { escaped = 0; continue }
          if (ch == "\\") { escaped = 1; continue }
          if (ch == "\"") { quote = 0; continue }
          continue
        }
        if (pair == "--") break
        if (pair == "{-") { depth++; i++; continue }
        if (ch == "\"") { quote = 1; continue }
        out = out ch
      }
      return out
    }
    {
      if ($0 ~ /^[[:space:]]*\|\|\|[[:space:]]*DISCHARGED[[:space:]]*:/) emit("CLAIMED_DISCHARGED", $0)
      if ($0 ~ /^[[:space:]]*\|\|\|[[:space:]]*OWED-AXIOM[[:space:]]*:/) emit("CLAIMED_OWED_AXIOM", $0)
      else if ($0 ~ /^[[:space:]]*\|\|\|[[:space:]]*OWED[[:space:]]*:/) emit("CLAIMED_OWED", $0)
      if ($0 ~ /^[[:space:]]*\|\|\|.*@[[:space:]]*Assumed[[:space:]]*:/) emit("ASSUMPTION_ANNOTATION", $0)
      code = code_of($0)
      if (code ~ /^[[:space:]]*0[[:space:]]+[A-Za-z_][[:alnum:]_\047]*[[:space:]]*:/) emit("ERASED_SIGNATURE_CANDIDATE", $0)
      if (code ~ /(^|[^[:alnum:]_])assert_total([^[:alnum:]_]|$)/) emit("ASSERT_TOTAL_ACTIVE", $0)
      if (code ~ /(^|[^[:alnum:]_])assert_smaller([^[:alnum:]_]|$)/) emit("ASSERT_SMALLER_ACTIVE", $0)
      if (code ~ /(^|[^[:alnum:]_])(really_believe_me|believe_me)([^[:alnum:]_]|$)/) emit("BELIEVE_ME_ACTIVE", $0)
      if (code ~ /(^|[^[:alnum:]_])idris_crash([^[:alnum:]_]|$)/) emit("IDRIS_CRASH_ACTIVE", $0)
      if (code ~ /^[[:space:]]*%default[[:space:]]+covering/) emit("DEFAULT_COVERING", $0)
      if (code ~ /(^|[^[:alnum:]_])\?[A-Za-z_][[:alnum:]_\047]*/) emit("HOLE_LEXICAL_CANDIDATE", $0)
    }
    END {
      if (depth != 0) {
        printf "ERROR: unterminated block comment in %s\n", file > "/dev/stderr"
        exit 3
      }
    }
  ' "$file" >> "$output"
done
printf 'Census: %s Idris sources, commit %s\n' "${#sources[@]}" "$commit"
awk -F '\t' 'NR > 6 && $1 !~ /^#/ { count[$1]++ } END { for (k in count) printf "%s\t%d\n", k, count[k] }' "$output" | LC_ALL=C sort
printf 'Report: %s\n' "$output"
