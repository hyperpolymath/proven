#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
out=${EVIDENCE_DIR:-"$root/build/header-evidence/boundary-controls"}
mkdir -p "$out"; out=$(cd "$out" && pwd)
echo INCOMPLETE > "$out/result.txt"
work=$(mktemp -d)
trap 'status=$?; if ((status)); then echo FAIL > "$out/result.txt"; fi; rm -rf "$work"' EXIT
real_idris=$(command -v "${IDRIS2:-idris2}")
real_idris=$(cd "$(dirname "$real_idris")" && pwd)/$(basename "$real_idris")
cat > "$work/diagnostic-compiler" <<'SH'
#!/usr/bin/env bash
if [[ $1 == --version ]]; then exec "$REAL_IDRIS" "$@"; fi
printf 'Error: planted zero-exit boundary compiler failure\n'
exit 0
SH
chmod +x "$work/diagnostic-compiler"
mkdir -p "$out/refused"
echo PASS > "$out/refused/result.txt"
if REAL_IDRIS="$real_idris" IDRIS2="$work/diagnostic-compiler" EVIDENCE_DIR="$out/refused" \
  bash "$root/scripts/test-header-boundary.sh" > "$out/reject-diagnostic.txt" 2>&1; then
  echo 'FAIL: zero-exit boundary compiler diagnostic accepted' >&2; exit 1
fi
grep -q 'FAIL: boundary compiler diagnostics' "$out/reject-diagnostic.txt"
[[ $(cat "$out/refused/result.txt") == FAIL ]]
echo PASS > "$out/result.txt"
