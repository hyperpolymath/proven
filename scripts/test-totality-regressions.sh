#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
unset CDPATH
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
idris=$(command -v "${IDRIS2:-idris2}")
idris=$(cd "$(dirname "$idris")" && pwd)/$(basename "$idris")
out="$root/build/totality-regressions"
mkdir -p "$out"
printf 'INCOMPLETE\n' > "$out/result.txt"
work=$(mktemp -d -t proven-totality.XXXXXXXX)
trap 'status=$?; rm -rf -- "$work"; if ((status)); then printf "FAIL\n" > "$out/result.txt"; fi' EXIT
cp -R "$root/src" "$work/src"
cp "$root/tests/totality/BoundaryRegression.idr" "$work/src/"
cp "$root/tests/properties/SafeMathProps.idr" "$work/src/"
# Exclude copied compiler artefacts: only this fresh source may satisfy imports.
find "$work/src" -type d -name build -prune -exec rm -rf -- {} +
(
  cd "$work"
  find src -type f -name '*.idr' -print0 | sort -z | xargs -0 sha256sum
) > "$out/source-sha256.txt"
"$idris" --version > "$out/toolchain.txt"
if ! (cd "$work"; "$idris" --total -Werror -p contrib -p network --source-dir src \
    -o boundary-regression src/BoundaryRegression.idr) > "$out/build.log" 2>&1; then
  cat "$out/build.log"; exit 1
fi
if grep -q '^Error:' "$out/build.log" || [[ ! -x "$work/build/exec/boundary-regression" ]]; then
  cat "$out/build.log"; exit 1
fi
(cd "$work"; printf ':m\n:q\n' | "$idris" --no-banner -p contrib -p network \
    --source-dir src src/BoundaryRegression.idr) > "$out/holes.log" 2>&1
grep -q 'No holes' "$out/holes.log"
if grep -q '^Error:' "$out/holes.log"; then cat "$out/holes.log"; exit 1; fi
"$work/build/exec/boundary-regression" | tee "$out/execution.log"
printf 'PASS\n' > "$out/result.txt"
