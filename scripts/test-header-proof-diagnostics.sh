#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Exercise the gate with a real compiler that emits an extra bad diagnostic.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
sholes=${1:?usage: test-header-proof-diagnostics.sh /path/to/sholes}
real_lean=$(command -v lean)
evidence=${EVIDENCE_DIR:-"$root/build/header-proof-evidence/diagnostics"}
mkdir -p "$evidence"
evidence=$(cd "$evidence" && pwd)
work=$(mktemp -d -t proven-header-diagnostics.XXXXXXXX)
trap 'rm -rf -- "$work"' EXIT
cat > "$work/lean" <<'WRAPPER'
#!/usr/bin/env bash
set -euo pipefail
if [[ ${HEADER_BAD_VERSION:-0} == 1 && ${1:-} == --version ]]; then
  echo 'Lean incompatible test version'; exit 0
fi
"$HEADER_REAL_LEAN" "$@"
for arg in "$@"; do
  if [[ $arg == ProvenHeader.lean ]]; then echo 'warning: injected gate regression control'; fi
done
WRAPPER
chmod +x "$work/lean"
if PATH="$work:$PATH" HEADER_REAL_LEAN="$real_lean" EVIDENCE_DIR="$evidence/warning" \
  bash "$root/scripts/check-header-proofs.sh" "$sholes" > "$evidence/warning-driver.log" 2>&1; then
  echo 'FAIL: gate accepted a compiler warning' >&2; exit 1
fi
grep -q 'FAIL: Lean diagnostic audit' "$evidence/warning-driver.log"
grep -q '^FAIL:' "$evidence/warning/result.txt"
if PATH="$work:$PATH" HEADER_REAL_LEAN="$real_lean" HEADER_BAD_VERSION=1 EVIDENCE_DIR="$evidence/version" \
  bash "$root/scripts/check-header-proofs.sh" "$sholes" > "$evidence/version-driver.log" 2>&1; then
  echo 'FAIL: gate accepted an incompatible compiler' >&2; exit 1
fi
grep -q '^FAIL:' "$evidence/version/result.txt"
echo 'PASS: successful compilation with a warning is rejected; early tool-profile failure leaves a failure receipt'
