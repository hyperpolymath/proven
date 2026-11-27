#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Hosted validation, or a separately admitted native slot. No implicit fetch.
set -euo pipefail
export UBSAN_OPTIONS=halt_on_error=1
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
libdir="${1:?pass the qualified full RefC library directory}"
zig="${ZIG:-zig}"
cc="${CC:-cc}"
rustc="${RUSTC:-rustc}"
base="${PATH_NATIVE_EVIDENCE:-$root/build/path-native-evidence}"
mkdir -p "$base"
out="$(mktemp -d "$base/run.XXXXXXXX")"
echo "Path native evidence: $out"
exec > >(tee "$out/run.log") 2>&1
cd "$root"
git rev-parse HEAD > "$out/source-head.txt"
{ find src/Proven ffi/zig/src -type f \( -name '*.idr' -o -name '*.zig' \) -print0
  printf '%s\0' scripts/test-native-path.sh scripts/build-header-refc.sh ffi/zig/build_standalone.zig \
    tests/remediation/native_path.c tests/remediation/native_path.rs \
    bindings/c/include/proven.h bindings/rust/src/ffi.rs
} | sort -zu | xargs -0 sha256sum > "$out/inputs.sha256"
sha256sum "$libdir/libproven.a" "$libdir/libproven.so" > "$out/artifacts.sha256"
prefix="$("${IDRIS2:-idris2}" --prefix)"
support_archive="$(find "$prefix" -name libidris2_support.a -type f -print -quit)"
[[ -n "$support_archive" ]]
sha256sum "$support_archive" > "$out/support.sha256"
flags=(-std=c11 -Wall -Wextra -Werror "-I$root/bindings/c/include")
"$cc" "${flags[@]}" "$root/tests/remediation/native_path.c" -L"$libdir" \
  -Wl,-rpath,"$libdir" -lproven -o "$out/c-shared"
"$out/c-shared" | tee "$out/c-shared.log"
"$cc" "${flags[@]}" -fsanitize=undefined -fno-sanitize-recover=all \
  "$root/tests/remediation/native_path.c" "$libdir/libproven.a" "$support_archive" \
  -lgmp -lm -pthread -o "$out/c-static"
"$out/c-static" | tee "$out/c-static.log"
"$rustc" --edition=2021 -C codegen-units=1 "$root/tests/remediation/native_path.rs" \
  -L "native=$libdir" -l dylib=proven -C "link-arg=-Wl,-rpath,$libdir" -o "$out/rust-consumer"
"$out/rust-consumer" | tee "$out/rust.log"

work="$(mktemp -d -t proven-path-native.XXXXXXXX)"
trap 'rm -rf -- "$work"' EXIT
cd "$root/ffi/zig"
"$zig" build -j1 --build-file build_standalone.zig --prefix "$work/unavailable" > "$out/unavailable-build.log" 2>&1
"$cc" "${flags[@]}" "$root/tests/remediation/native_path.c" \
  "$work/unavailable/lib/libproven_ffi.a" -pthread -o "$work/unavailable-consumer"
"$work/unavailable-consumer" unavailable | tee "$out/unavailable.log"

# The same independent C oracle must fail against the unchanged old artifact.
baseline=d5eb0bf66538c15d8ed88ced95edf7c2b70cbc81
mkdir -p "$work/baseline"
git -C "$root" archive "$baseline" ffi/zig | tar -x -C "$work/baseline"
printf '%s\n' "$baseline" > "$out/baseline-commit.txt"
(cd "$work/baseline/ffi/zig"; "$zig" build -j1 --build-file build_standalone.zig --prefix "$work/baseline-lib") > "$out/baseline-build.log" 2>&1
"$cc" "${flags[@]}" "$root/tests/remediation/native_path.c" \
  "$work/baseline-lib/lib/libproven_ffi.a" -pthread -o "$work/baseline-consumer"
status=0
"$work/baseline-consumer" > "$out/baseline-refusal.log" 2>&1 || status=$?
[[ "$status" -eq 1 ]]
grep -Fq 'FAIL: path oracle at case 1' "$out/baseline-refusal.log"

# Mutate only generated code, and exercise the already linked foreign consumer.
bash "$root/scripts/build-header-refc.sh" "$work/bundle" > "$out/kernel-build.log" 2>&1
cp "$work/bundle/identity.log" "$out/sdk-kernel-identity.log"
cp "$work/bundle/kernel.c" "$work/original.c"
for mutant in clean invalid-step invalid-finish; do
  cp "$work/original.c" "$work/bundle/kernel.c"
  symbol=Proven_SafePath_ByteKernel_pathStep
  signature='void *state, void *byte'
  body='(void)byte; return state;'
  if [[ $mutant == invalid-step ]]; then
    body='(void)state; (void)byte; return (void *)(uintptr_t)UINT64_C(0xff00000001);'
  elif [[ $mutant == invalid-finish ]]; then
    symbol=Proven_SafePath_ByteKernel_pathFinish
    signature='void *state'
    body='(void)state; return (void *)(uintptr_t)UINT64_C(0xff00000001);'
  fi
  grep -q "$symbol" "$work/bundle/kernel.c"
  sed -i "s/$symbol/proven_original_path_symbol/g" "$work/bundle/kernel.c"
  printf '\nvoid *%s(%s) { %s }\n' "$symbol" "$signature" "$body" >> "$work/bundle/kernel.c"
  "$zig" build -j1 --build-file build_standalone.zig -Dshared=true \
    "-Dheader-refc=$work/bundle" --prefix "$work/$mutant" > "$out/$mutant-build.log" 2>&1
  status=0
  LD_LIBRARY_PATH="$work/$mutant/lib" "$out/c-shared" > "$out/$mutant-oracle.log" 2>&1 || status=$?
  [[ "$status" -eq 1 ]]
  grep -Fq 'FAIL: path oracle at case 1' "$out/$mutant-oracle.log"
  if [[ $mutant != clean ]]; then
    LD_LIBRARY_PATH="$work/$mutant/lib" "$out/c-shared" invalid > "$out/$mutant-refusal.log" 2>&1
  fi
done
"$out/c-shared" | tee "$out/original-after-controls.log"
cd "$root"
sha256sum --check --quiet "$out/inputs.sha256"
sha256sum --check --quiet "$out/artifacts.sha256"
sha256sum --check --quiet "$out/support.sha256"
echo 'PASS: actual static/shared C/Rust path scan; baseline failure; unavailable and malformed replies refused; semantic mutant rejected'
