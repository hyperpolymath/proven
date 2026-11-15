#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
# Self-test the libproven/Rustler symbol-coverage gate with planted failures.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
driver="$root/scripts/check-native-nif-exports.sh"
for cmd in cc ar; do
  command -v "$cmd" >/dev/null 2>&1 || {
    echo "native-nif-symbol-controls: NOT RUN (missing $cmd)" >&2
    exit 2
  }
done

tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT
cat > "$tmp/lib.rs" <<'EOF'
#[link(name = "proven")]
extern "C" {
    fn proven_init() -> i32;
    pub fn proven_math_div(a: i64, b: i64) -> i64;
}
fn wrapper_not_a_required_export() {}
EOF
cat > "$tmp/good.c" <<'EOF'
int proven_init(void) { return 0; }
long proven_math_div(long a, long b) { return b ? a / b : 0; }
EOF
cat > "$tmp/bad.c" <<'EOF'
int proven_init(void) { return 0; }
EOF
cc -shared -fPIC -o "$tmp/good.so" "$tmp/good.c"
cc -shared -fPIC -o "$tmp/bad.so" "$tmp/bad.c"
cc -c -o "$tmp/good.o" "$tmp/good.c"
ar rcs "$tmp/good.a" "$tmp/good.o"

expect() {
  local label=$1 want=$2 pattern=$3
  shift 3
  local got=0
  "$@" > "$tmp/log" 2>&1 || got=$?
  if [[ $got != "$want" ]] || ! grep -Fq "$pattern" "$tmp/log"; then
    echo "FAILED: $label (exit $got, expected $want, pattern $pattern)" >&2
    cat "$tmp/log" >&2
    exit 1
  fi
  echo "ok: $label"
}

expect 'all exported symbols (shared)' 0 'symbols_present_only' bash "$driver" "$tmp/good.so" "$tmp/lib.rs"
expect 'all exported symbols (static)' 0 'symbols_present_only' bash "$driver" "$tmp/good.a" "$tmp/lib.rs"
expect 'planted missing symbol is refused' 1 'missing C ABI symbol: proven_math_div' bash "$driver" "$tmp/bad.so" "$tmp/lib.rs"
# Each qualified declaration must still require the missing division export.
for qualifier in 'pub(crate)' 'pub(super)' 'pub(in crate::ffi)' safe unsafe 'pub safe' 'pub(crate) unsafe'; do
  printf 'extern "C" {\n    %s fn proven_math_div(a: i64, b: i64) -> i64;\n}\n' "$qualifier" > "$tmp/qualified.rs"
  expect "qualified declaration ($qualifier)" 1 'missing C ABI symbol: proven_math_div' bash "$driver" "$tmp/bad.so" "$tmp/qualified.rs"
done

cat > "$tmp/versions.map" <<'EOF'
PROVEN_1 { global: proven_*; local: *; };
EOF
cc -shared -fPIC -Wl,--version-script="$tmp/versions.map" -o "$tmp/default-version.so" "$tmp/good.c"
expect 'default-version exports satisfy contract' 0 'symbols_present_only' bash "$driver" "$tmp/default-version.so" "$tmp/lib.rs"

cat > "$tmp/nondefault.c" <<'EOF'
int proven_init(void) { return 0; }
long div_impl(long a, long b) { return b ? a / b : 0; }
__asm__(".symver div_impl,proven_math_div@PROVEN_1");
EOF
cc -shared -fPIC -Wl,--version-script="$tmp/versions.map" -o "$tmp/nondefault-version.so" "$tmp/nondefault.c"
expect 'single-@ export does not satisfy contract' 1 'missing C ABI symbol: proven_math_div' bash "$driver" "$tmp/nondefault-version.so" "$tmp/lib.rs"
expect 'missing library is NOT RUN' 2 'not_run:missing_input' bash "$driver" "$tmp/nonexistent.so" "$tmp/lib.rs"
: > "$tmp/empty.rs"
expect 'empty consumer contract is NOT RUN' 2 'not_run:empty_contract' bash "$driver" "$tmp/good.so" "$tmp/empty.rs"

# An invalid artifact must not be accepted merely because it has a .so suffix.
printf 'not an ELF binary\n' > "$tmp/invalid.so"
expect 'unreadable artifact is NOT RUN' 2 'not_run:unreadable_shared_lib' bash "$driver" "$tmp/invalid.so" "$tmp/lib.rs"
echo 'native-nif-symbol-controls: 15 cases passed (actual ABI not yet verified)'
