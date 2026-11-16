#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Failed producers must invalidate old header bundles, including early failure.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
zig=$(command -v "${ZIG:-zig}")
zig=$(cd "$(dirname "$zig")" && printf '%s/%s' "$PWD" "$(basename "$zig")")
evidence=${EVIDENCE_DIR:-"$root/build/header-evidence/staging-controls"}
mkdir -p "$evidence"
evidence=$(cd "$evidence" && pwd)
rm -f "$evidence/result.txt"
work=$(mktemp -d -t proven-header-staging.XXXXXXXX)
trap 'rm -rf -- "$work"' EXIT
mkdir -p "$work/bundle/refc" "$work/bundle/support"
cat > "$work/idris2" <<'COMPILER'
#!/usr/bin/env bash
if [[ $1 == --version ]]; then
  if [[ $PROVEN_HEADER_TEST_MODE == version ]]; then echo 'wrong compiler';
  else echo 'Idris 2, version 0.8.0-b6cbc25d5'; fi
elif [[ $PROVEN_HEADER_TEST_MODE == diagnostic ]]; then
  mkdir -p build/exec
  echo 'Proven_SafeHeader_ByteKernel_headerStep' > build/exec/header-kernel.c
  echo 'Error: injected header compiler diagnostic with successful exit'
fi
exit 0
COMPILER
chmod +x "$work/idris2"
for mode in version diagnostic empty; do
  for file in kernel.c libidris2_refc.a libidris2_support.a identity.log refc/stale.h support/stale.h; do
    echo stale > "$work/bundle/$file"
  done
  echo PASS > "$work/bundle/result.txt"
  if PROVEN_HEADER_TEST_MODE="$mode" IDRIS2="$work/idris2" bash "$root/scripts/build-header-refc.sh" "$work/bundle" > "$evidence/$mode.log" 2>&1; then
    echo "FAIL: accepted $mode header compiler with stale output" >&2; exit 1
  fi
  grep -q '^FAIL:' "$work/bundle/result.txt"
  for file in kernel.c libidris2_refc.a libidris2_support.a identity.log refc/stale.h support/stale.h; do
    test ! -e "$work/bundle/$file"
  done
  if (cd "$root/ffi/zig"; "$zig" build --build-file build_standalone.zig -Dheader-refc="$work/bundle") > "$evidence/$mode-consumer.log" 2>&1; then
    echo 'FAIL: Zig accepted an incomplete header bundle' >&2; exit 1
  fi
  grep -q 'header RefC bundle is incomplete' "$evidence/$mode-consumer.log"
done
grep -q 'requires Idris2' "$evidence/version.log"
grep -q '^FAIL: header compiler diagnostics' "$evidence/diagnostic.log"
grep -q 'no valid RefC output' "$evidence/empty.log"
echo PASS > "$evidence/result.txt"
echo 'PASS: failed header producers remove stale artifacts and Zig refuses their bundles'
