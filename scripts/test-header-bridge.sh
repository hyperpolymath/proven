#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
zig=${ZIG:-zig}
command -v "$zig" >/dev/null
evidence=${EVIDENCE_DIR:-"$root/build/header-evidence"}
mkdir -p "$evidence"
evidence=$(cd "$evidence" && pwd)
rm -f "$evidence/result.txt"
find "$root/ffi/zig/src" -maxdepth 1 -name '*.zig' -type f -exec sha256sum {} + > "$evidence/inputs.sha256"
sha256sum "$root/ffi/zig/build_standalone.zig" "$root/src/Proven/SafeHeader/ByteKernel.idr" "$root/src/Proven/FFI/HeaderKernelMain.idr" >> "$evidence/inputs.sha256"
sha256sum "$root/tests/ffi/header_byte_bridge.c" "$root/bindings/c/include/proven.h" >> "$evidence/inputs.sha256"
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
sha256sum -c "$evidence/inputs.sha256" > "$evidence/input-recheck.log"
echo PASS > "$evidence/result.txt"
printf 'PASS: Idris totality/holes; unavailable backend refusal; actual RefC ABI; 256 single bytes; 65536 pairs; boundary corpus; rejecting mutant.\nEvidence: %s\n' "$evidence"
