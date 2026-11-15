#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
# Test the actual RefC staging script without requiring a compiler or private
# bridge. These fixtures do NOT establish that generated C is correct Idris.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
script="$root/scripts/build-refc.sh"
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT
project="$tmp/project"
prefix="$tmp/idris-install"
mkdir -p "$project/build/exec" "$project/build/refc" "$prefix/idris2-0.8.0/support/refc"
touch "$prefix/idris2-0.8.0/support/refc/libidris2_refc.a"
printf 'package proven-ffi\n' > "$project/proven-ffi.ipkg"
cat > "$tmp/fake-idris2" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  --version) echo 'Idris 2, version 0.8.0' ;;
  --prefix) printf '%s\n' "$FAKE_IDRIS_PREFIX" ;;
  --codegen)
    [[ "${2:-}" == refc && "${3:-}" == --build ]] || exit 9
    case "${FAKE_ACTION:-}" in
      write)
        mkdir -p "$PROVEN_ROOT/build/exec"
        printf '/* generated this run */\n' > "$PROVEN_ROOT/build/exec/proven-ffi.c"
        ;;
      none) : ;;
      fail) echo 'planted RefC compiler failure' >&2; exit 13 ;;
      *) exit 10 ;;
    esac
    ;;
  *) exit 11 ;;
esac
EOF
chmod +x "$tmp/fake-idris2"

run_case() {
  local action=$1 want=$2 expected=$3 rc=0
  env IDRIS2="$tmp/fake-idris2" PROVEN_ROOT="$project" \
    FAKE_IDRIS_PREFIX="$prefix" FAKE_ACTION="$action" \
    bash "$script" --clean > "$tmp/output.log" 2>&1 || rc=$?
  if [[ "$rc" != "$want" ]] || ! grep -Fq "$expected" "$tmp/output.log"; then
    printf 'FAILED: action %s expected exit %s and text %q, got %s\n' "$action" "$want" "$expected" "$rc" >&2
    cat "$tmp/output.log" >&2
    exit 1
  fi
  echo "ok: $action -> $want ($expected)"
}

run_case none 1 'RefC generated no fresh C source'
printf '/* stale earlier build */\n' > "$project/build/exec/proven-ffi.c"
touch -t 202001010000 "$project/build/exec/proven-ffi.c"
run_case none 1 'RefC generated no fresh C source'
printf '/* unrelated stale stage */\n' > "$project/build/refc/old.c"
run_case write 0 'Found 1 fresh generated C file(s)'
[[ $(find "$project/build/refc" -maxdepth 1 -name '*.c' -type f | wc -l) == 1 ]] || {
  echo 'FAILED: stale or duplicate C persisted in output stage' >&2
  exit 1
}
grep -Fq 'generated this run' "$project/build/refc/proven-ffi.c" || {
  echo 'FAILED: expected fresh content not staged' >&2
  exit 1
}
run_case fail 13 'planted RefC compiler failure'
echo 'RefC staging: 4 negative/positive cases passed; no real codegen proof claimed.'
