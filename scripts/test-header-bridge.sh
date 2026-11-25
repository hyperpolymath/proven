#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
zig=${ZIG:-zig}
zig=$(command -v "$zig")
zig=$(cd "$(dirname "$zig")" && printf '%s/%s' "$PWD" "$(basename "$zig")")
evidence=${EVIDENCE_DIR:-"$root/build/header-evidence"}
mkdir -p "$evidence"
evidence=$(cd "$evidence" && pwd)
rm -f "$evidence/result.txt"
find "$root/ffi/zig/src" -name '*.zig' -type f -exec sha256sum {} + > "$evidence/inputs.sha256"
sha256sum "$root/ffi/zig/build_standalone.zig" "$root/src/Proven/SafeHeader/ByteKernel.idr" "$root/src/Proven/FFI/HeaderKernelMain.idr" >> "$evidence/inputs.sha256"
sha256sum "$root/src/Proven/SafePath/ByteKernel.idr" >> "$evidence/inputs.sha256"
sha256sum "$root/tests/ffi/header_byte_bridge.c" "$root/bindings/c/include/proven.h" "$root/bindings/c/include/proven-abi.tsv" >> "$evidence/inputs.sha256"
sha256sum "$root/scripts/check-abi-ledger.sh" "$root/scripts/test-abi-ledger.sh" >> "$evidence/inputs.sha256"
work=$(mktemp -d -t proven-header-check.XXXXXXXX)
trap 'rm -rf -- "$work"' EXIT
bash "$root/scripts/build-header-refc.sh" "$work/bundle"
cp "$work/bundle/identity.log" "$work/bundle/holes.log" "$evidence/"
cp "$work/bundle/build.log" "$evidence/kernel-build.log"
"$zig" version > "$evidence/zig-version.log"
cd "$root/ffi/zig"
"$zig" build test --build-file build_standalone.zig --summary all > "$evidence/unavailable.log" 2>&1
"$zig" build test --build-file build_standalone.zig -Dheader-refc="$work/bundle" --summary all > "$evidence/refc.log" 2>&1
"$zig" build --build-file build_standalone.zig -Dheader-refc="$work/bundle" > "$evidence/build.log" 2>&1
"$zig" build --build-file build_standalone.zig -Dshared=true -Dheader-refc="$work/bundle" --prefix "$work/shared" > "$evidence/shared-build.log" 2>&1
cc -Wall -Wextra -Werror -I "$root/bindings/c/include" "$root/tests/ffi/header_byte_bridge.c" "$work/shared/lib/libproven.so" -Wl,-rpath,"$work/shared/lib" -o "$work/c-abi" > "$evidence/c-build.log" 2>&1
"$work/c-abi" > "$evidence/c-abi.log" 2>&1
EVIDENCE_DIR="$evidence/ownership" ZIG="$zig" bash "$root/scripts/test-header-borrow.sh" "$work/shared/lib"
EVIDENCE_DIR="$evidence/ownership-controls" ZIG="$zig" bash "$root/scripts/test-header-borrow-controls.sh" "$work/shared/lib"

# Every proven.h function and every library export must have a ledger row, and
# the qualified library must define exactly the rows that claim presence.
bash "$root/scripts/test-abi-ledger.sh" > "$evidence/abi-ledger-controls.log" 2>&1 ||
  { cat "$evidence/abi-ledger-controls.log" >&2; exit 1; }
bash "$root/scripts/check-abi-ledger.sh" "$root/bindings/c/include/proven-abi.tsv" "$root/bindings/c/include/proven.h" \
  "$work/shared/lib/libproven.so" "$evidence/abi-ledger" > "$evidence/abi-ledger.log" 2>&1 ||
  { cat "$evidence/abi-ledger.log" >&2; exit 1; }

# A valid but wrong implementation must fail the exported-ABI tests.
# Rename only within this private generated bundle and supply an ABI-compatible
# mutant that always keeps the old state. Production sources remain untouched.
sed -i 's/Proven_SafeHeader_ByteKernel_headerStep/proven_original_headerStep/g' "$work/bundle/kernel.c"
cat >> "$work/bundle/kernel.c" <<'C'
void *Proven_SafeHeader_ByteKernel_headerStep(void *state, void *byte) {
    (void)byte;
    return state;
}
C
if "$zig" build test --build-file build_standalone.zig -Dheader-refc="$work/bundle" > "$evidence/mutant.log" 2>&1; then
  echo 'FAIL: ABI tests accepted the always-clean kernel' >&2; exit 1
fi
grep -q 'TestExpectedEqual' "$evidence/mutant.log"
grep -q '/support/idris_support.h' "$evidence/identity.log"
EVIDENCE_DIR="$evidence/staging-controls" ZIG="$zig" bash "$root/scripts/test-header-staging.sh"
sha256sum -c "$evidence/inputs.sha256" > "$evidence/input-recheck.log"
echo PASS > "$evidence/result.txt"
printf 'PASS: Idris totality/holes; unavailable backend refusal; actual RefC ABI; 256 single bytes; 65536 pairs; boundary corpus; rejecting mutant; ABI ledger and its planted controls.\nEvidence: %s\n' "$evidence"
