#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# Heavy validation: launch only when this batch has a resource-queue slot.
# Scope: lifecycle ABI/borrowed errors, not the complete native/Rustler surface.
set -euo pipefail
export UBSAN_OPTIONS=halt_on_error=1
root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
idris="${IDRIS2:-idris2}"
zig="${ZIG:-zig}"
rustc="${RUSTC:-rustc}"
cc="${CC:-cc}"
for tool in "$idris" "$zig" "$rustc" "$cc" readelf; do command -v "$tool" >/dev/null; done
evidence_root="${NATIVE_ABI_EVIDENCE:-$root/build/native-abi-evidence}"
mkdir -p "$evidence_root"
evidence="$(mktemp -d "$evidence_root/run.XXXXXXXX")"
scratch="$(mktemp -d -t proven-lifecycle.XXXXXXXX)"
trap 'rm -rf -- "$scratch"' EXIT
echo "Lifecycle evidence: $evidence"
exec > >(tee "$evidence/run.log") 2>&1
# Bind this run to file contents, including uncommitted remediation and the
# compiler SDK. A passing run must still have the same inputs at its end.
cd "$root"
git rev-parse HEAD > "$evidence/base-commit.txt"
git status --porcelain=v1 > "$evidence/worktree-status.txt"
{
  printf '%s\0' proven-ffi.ipkg scripts/build-refc.sh scripts/test-native-lifecycle.sh \
    ffi/zig/build.zig ffi/zig/build_standalone.zig ffi/zig/build.zig.zon \
    bindings/c/include/proven.h bindings/c/include/proven-abi.tsv scripts/check-abi-ledger.sh bindings/rust/src/ffi.rs
  find src/Proven ffi/zig/src tests/remediation \
    -type f \( -name '*.idr' -o -name '*.zig' -o -name '*.zon' \
      -o -name '*.c' -o -name '*.h' -o -name '*.rs' \) \
    -not -path '*/.zig-cache/*' -not -path '*/zig-out/*' -print0
} | sort -zu | xargs -0 sha256sum > "$evidence/inputs.sha256"
{
  "$idris" --version
  "$zig" version
  "$rustc" --version
  "$cc" --version
} > "$evidence/toolchains.log"
if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  printf 'artifact-dir=%s\n' "$evidence/native/lib" >> "$GITHUB_OUTPUT"
fi

mkdir -p "$scratch/package/src"
cp -R "$root/src/Proven" "$scratch/package/src/"
cp "$root/proven-ffi.ipkg" "$scratch/package/"
cp "$root/tests/remediation/LifecycleProofRegressions.idr" "$scratch/package/src/"
cp "$root/tests/remediation/LifecycleMain.idr" "$scratch/package/src/"
cd "$scratch/package/src"
checked_compile() {
  "$idris" -p contrib --no-color --total "$@" 2>&1 | tee "$evidence/compiler.log"
  if grep -Eq '^Error:|Warning: compiling hole|Unsolved holes' "$evidence/compiler.log"; then
    echo "FAIL: compiler reported an error or hole" >&2
    exit 1
  fi
}
checked_compile --check LifecycleProofRegressions.idr
cp "$evidence/compiler.log" "$evidence/proofs.log"
checked_compile -o lifecycle-regressions LifecycleMain.idr
./build/exec/lifecycle-regressions | tee "$evidence/idris-runtime.log"
for fixture in ForgedLifecycle MalformedLifecycle; do
  cp "$root/tests/remediation/negative/$fixture.idr" .
  if "$idris" -p contrib --no-color --total --check "$fixture.idr" > "$evidence/$fixture.log" 2>&1; then
    echo "FAIL: $fixture unexpectedly typechecked" >&2
    exit 1
  fi
  grep -Eq "Mismatch between:|Can't solve constraint between:" "$evidence/$fixture.log"
  echo "PASS: rejected $fixture"
done

IDRIS2="$idris" PROVEN_ROOT="$scratch/package" bash "$root/scripts/build-refc.sh" --verbose 2>&1 | tee "$evidence/refc-build.log"
if grep -Eq '^Error:|Warning: compiling hole|Unsolved holes' "$evidence/refc-build.log"; then exit 1; fi
prefix="$("$idris" --prefix)"
runtime="$(find "$prefix" -path '*/support/refc' -type d -print -quit)"
support="$(find "$prefix" -path '*/support/c' -type d -print -quit)"
support_archive="$(find "$prefix" -name libidris2_support.a -type f -print -quit)"
[[ -f "$runtime/libidris2_refc.a" ]]
[[ -f "$support/idris_support.h" ]]
[[ -n "$support_archive" ]]
sha256sum "$runtime/"*.h "$runtime/libidris2_refc.a" "$support/"*.h "$support_archive" > "$evidence/sdk-inputs.sha256"
common=("-Didris-refc-runtime=$runtime" "-Didris-c-support=$support"
        "-Didris-support-lib=$(dirname "$support_archive")")
cd "$root/ffi/zig"
"$zig" build -j1 "-Didris-refc=$scratch/package/build/refc" "${common[@]}" --prefix "$evidence/native" --summary all 2>&1 | tee "$evidence/zig-build.log"
"$zig" build test -j1 "-Didris-refc=$scratch/package/build/refc" "${common[@]}" --summary all 2>&1 | tee "$evidence/zig-tests.log"
[[ -s "$evidence/native/lib/libproven.a" ]]
[[ -s "$evidence/native/lib/libproven.so" ]]
for artifact in "$evidence/native/lib/libproven.a" "$evidence/native/lib/libproven.so"; do
  bash "$root/scripts/check-abi-ledger.sh" "$root/bindings/c/include/proven-abi.tsv" \
    "$root/bindings/c/include/proven.h" "$artifact" "$evidence/ledger-$(basename "$artifact")"
done
"$zig" build -j1 --build-file build_standalone.zig --prefix "$scratch/standalone" --summary all
"$zig" build test -j1 --build-file build_standalone.zig --summary all

# Header-typed real consumers; shared and static links are separate checks.
cflags=(-std=c11 -Wall -Wextra -Werror "-I$root/bindings/c/include")
"$cc" "${cflags[@]}" "$root/tests/remediation/native_lifecycle.c" \
  -L"$evidence/native/lib" -Wl,-rpath,"$evidence/native/lib" -lproven -pthread -o "$evidence/c-shared"
"$evidence/c-shared" | tee "$evidence/c-shared.log"
# An SDK RPATH can hide a missing runtime dependency on the validation host.
# Reject that dependency structurally, then run a copied consumer/library with
# only an origin-relative search path and no inherited LD_LIBRARY_PATH.
readelf -d "$evidence/native/lib/libproven.so" > "$evidence/shared-dynamic.txt"
if grep -Eq 'NEEDED.*libidris2_|\((RPATH|RUNPATH)\)' "$evidence/shared-dynamic.txt"; then
  echo "FAIL: shared library depends on the compiler installation" >&2
  exit 1
fi
mkdir -p "$evidence/relocated/lib"
cp "$evidence/native/lib/libproven.so" "$evidence/relocated/lib/"
"$cc" "${cflags[@]}" "$root/tests/remediation/native_lifecycle.c" \
  -L"$evidence/relocated/lib" '-Wl,-rpath,$ORIGIN/lib' -lproven -pthread \
  -o "$evidence/relocated/consumer"
env -u LD_LIBRARY_PATH "$evidence/relocated/consumer" | tee "$evidence/c-relocated.log"
echo "PASS: relocated shared consumer; no compiler runtime dependency or embedded RPATH"
"$cc" "${cflags[@]}" -fsanitize=undefined -fno-sanitize-recover=all \
  "$root/tests/remediation/native_lifecycle.c" \
  "$evidence/native/lib/libproven.a" "$support_archive" \
  -lgmp -lm -pthread -o "$evidence/c-static"
"$evidence/c-static" | tee "$evidence/c-static.log"
"$cc" "${cflags[@]}" "$root/tests/remediation/native_lifecycle.c" \
  "$scratch/standalone/lib/libproven_ffi.a" -pthread -o "$scratch/c-unavailable"
"$scratch/c-unavailable" --unavailable | tee "$evidence/c-unavailable.log"
"$rustc" --edition=2021 -C codegen-units=1 "$root/tests/remediation/native_lifecycle.rs" \
  -L "native=$evidence/native/lib" -l dylib=proven \
  -C "link-arg=-Wl,-rpath,$evidence/native/lib" -o "$evidence/rust-consumer"
"$evidence/rust-consumer" | tee "$evidence/rust-consumer.log"

# Corrupt only generated production lifecycle code. The same already-linked C
# consumer must reach and fail its init-status assertion, not fail to load.
cp -R "$scratch/package/build/refc" "$scratch/corrupted-refc"
changed=0
for file in "$scratch/corrupted-refc"/*.c; do
  if ! grep -q '^Idris2_Value \*Proven_ABI_Lifecycle_lifecycleCommand$' "$file"; then continue; fi
  awk '
    /^Idris2_Value \*Proven_ABI_Lifecycle_lifecycleCommand$/ { candidate=1 }
    candidate && /\);/ { candidate=0 }
    candidate && /^\{/ { print; body=1; next }
    body && /^\}/ {
      print "    idris2_removeReference(var_0);"
      print "    return idris2_mkBits64(2);"
      print
      body=0; candidate=0; changed++; next
    }
    !body { print }
    END { if (changed != 1) exit 1 }
  ' "$file" > "$file.mutated"
  mv -- "$file.mutated" "$file"
  changed=$((changed + 1))
done
[[ "$changed" -eq 1 ]]
"$zig" build -j1 "-Didris-refc=$scratch/corrupted-refc" "${common[@]}" --prefix "$scratch/corrupted" 2>&1 | tee "$evidence/mutation-build.log"
status=0
LD_LIBRARY_PATH="$scratch/corrupted/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
  "$evidence/c-shared" > "$evidence/corrupted-result.log" 2>&1 || status=$?
[[ "$status" -eq 1 ]]
grep -Fq 'FAIL: init status' "$evidence/corrupted-result.log"
echo "PASS: corrupted generated lifecycle result rejected by typed C consumer"
LD_LIBRARY_PATH="$scratch/corrupted/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
  "$evidence/c-shared" --backend-failure | tee "$evidence/backend-failure.log"
sha256sum "$evidence/native/lib/libproven.a" "$evidence/native/lib/libproven.so" > "$evidence/artifacts.sha256"
cd "$root"
sha256sum --check --quiet "$evidence/inputs.sha256"
sha256sum --check --quiet "$evidence/sdk-inputs.sha256"
echo "PASS: scoped lifecycle tests; complete native ABI still requires its separate gate"
