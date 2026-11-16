#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Build the small header kernel with its actual RefC SDK. No private dependency.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
out=${1:-"$root/build/header-refc"}
idris=${IDRIS2:-idris2}
idris=$(command -v "$idris")
idris=$(cd "$(dirname "$idris")" && printf '%s/%s' "$PWD" "$(basename "$idris")")
[[ $("$idris" --version) == 'Idris 2, version 0.8.0-b6cbc25d5' ]] || {
  echo 'FAIL: header ABI profile requires Idris2 b6cbc25d5cd2ef2a250556266fd7b68dfc360f8b' >&2; exit 1;
}
[[ $(uname -sm) == 'Linux x86_64' ]] || { echo 'FAIL: only native Linux x86_64 is qualified' >&2; exit 1; }
mkdir -p "$out"
out=$(cd "$out" && pwd)
work=$(mktemp -d -t proven-header-refc.XXXXXXXX)
trap 'rm -rf -- "$work"' EXIT
mkdir -p "$work/src/Proven/SafeHeader" "$work/src/Proven/FFI"
cp "$root/src/Proven/SafeHeader/ByteKernel.idr" "$work/src/Proven/SafeHeader/"
cp "$root/src/Proven/FFI/HeaderKernelMain.idr" "$work/src/Proven/FFI/"
if ! (cd "$work"; "$idris" -Werror --total --source-dir src --codegen refc -o header-kernel src/Proven/FFI/HeaderKernelMain.idr) > "$out/build.log" 2>&1; then
  cat "$out/build.log" >&2; exit 1
fi
if grep -q '^Error:' "$out/build.log" || [[ ! -s "$work/build/exec/header-kernel.c" ]]; then
  cat "$out/build.log" >&2; echo 'FAIL: no valid RefC output' >&2; exit 1
fi
grep -q 'Proven_SafeHeader_ByteKernel_headerStep' "$work/build/exec/header-kernel.c"
(cd "$work"; printf ':m\n:q\n' | "$idris" --no-banner --source-dir src src/Proven/SafeHeader/ByteKernel.idr) > "$out/holes.log" 2>&1
grep -q 'No holes' "$out/holes.log"
if grep -q '^Error:' "$out/holes.log"; then cat "$out/holes.log" >&2; exit 1; fi
prefix=$("$idris" --prefix)
mapfile -t runtimes < <(find "$prefix" -path '*/support/refc/runtime.h' -type f)
mapfile -t supports < <(find "$prefix" -path '*/support/c/idris_support.h' -type f)
mapfile -t support_libs < <(find "$prefix" -name libidris2_support.a -type f)
[[ ${#runtimes[@]} == 1 && ${#supports[@]} == 1 && ${#support_libs[@]} == 1 ]] || {
  echo 'FAIL: expected one matching installed RefC SDK; ambiguous or missing SDK' >&2; exit 1;
}
runtime=$(dirname "${runtimes[0]}")
support=$(dirname "${supports[0]}")
test -s "$runtime/libidris2_refc.a"
mkdir -p "$out/refc" "$out/support"
cp "$runtime/"*.h "$out/refc/"
cp "$support/"*.h "$out/support/"
cp "$runtime/libidris2_refc.a" "${support_libs[0]}" "$out/"
cp "$work/build/exec/header-kernel.c" "$out/kernel.c"
{
  "$idris" --version
  sha256sum "$root/src/Proven/SafeHeader/ByteKernel.idr" "$root/src/Proven/FFI/HeaderKernelMain.idr" "$out/kernel.c" "$out/"*.a "$out/refc/"*.h
} > "$out/identity.log"
printf 'RefC header kernel: %s\n' "$out"
