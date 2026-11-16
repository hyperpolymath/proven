#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Adversarial compiler controls: stale output must never rescue a failed build.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
evidence=${EVIDENCE_DIR:-"$root/build/refc/staging-controls"}
mkdir -p "$evidence"
evidence=$(cd "$evidence" && pwd)
rm -f "$evidence/result.txt"
work=$(mktemp -d -t proven-refc-staging.XXXXXXXX)
trap 'rm -rf -- "$work"' EXIT
mkdir -p "$work/fixture/src" "$work/fixture/build/refc"
cp "$root/proven-ffi.ipkg" "$work/fixture/"
cat > "$work/idris2" <<'COMPILER'
#!/usr/bin/env bash
if [[ $1 == --version ]]; then
  if [[ $PROVEN_REFC_TEST_MODE == version ]]; then echo 'wrong compiler';
  else echo 'Idris 2, version 0.8.0-b6cbc25d5'; fi
elif [[ $PROVEN_REFC_TEST_MODE == diagnostic ]]; then
  echo 'Error: injected compiler diagnostic with successful exit'
fi
exit 0
COMPILER
chmod +x "$work/idris2"
for mode in diagnostic empty version; do
  printf '/* stale */ void Proven_SafeHeader_ByteKernel_headerStep(void) {}\n' > "$work/fixture/build/refc/stale.c"
  echo PASS > "$work/fixture/build/refc/result.txt"
  if PROVEN_REFC_TEST_MODE="$mode" PROVEN_ROOT="$work/fixture" IDRIS2="$work/idris2" bash "$root/scripts/build-refc.sh" > "$evidence/$mode.log" 2>&1; then
    echo "FAIL: accepted $mode compiler with stale C output" >&2; exit 1
  fi
  grep -q '^FAIL:' "$work/fixture/build/refc/result.txt"
  test ! -e "$work/fixture/build/refc/stale.c"
  test ! -e "$work/fixture/build/refc/sdk.env"
done
grep -q 'Error: injected compiler diagnostic' "$evidence/diagnostic.log"
grep -q 'expected exactly one fresh RefC translation unit' "$evidence/empty.log"
grep -q 'requires Idris2' "$evidence/version.log"
echo PASS > "$evidence/result.txt"
echo 'PASS: zero-exit diagnostics, missing output and wrong compiler cannot reuse stale C or a PASS receipt'
