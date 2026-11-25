#!/usr/bin/env bash
# Review/admission required: one bounded integrated native validation, no retry.
set -euo pipefail
repo=/home/hyperpolymath/.cache/codex-audits/proven-remediation-2026-11-16
base=/home/hyperpolymath/.cache/codex-audits/proven-integration-evidence-20261125
mkdir -p "$base"
evidence=$(mktemp -d "$base/native.XXXXXXXX")
echo "Integrated native evidence: $evidence"
exec > >(tee "$evidence/stage.log") 2>&1
export IDRIS2=/home/hyperpolymath/.local/bin/idris2
export ZIG=/home/hyperpolymath/.local/share/mise/installs/zig/0.15.2/zig
export RUSTC=/usr/bin/rustc
export CC=/usr/bin/cc
export ZIG_GLOBAL_CACHE_DIR="$evidence/zig-global-cache"
export ZIG_LOCAL_CACHE_DIR="$evidence/zig-local-cache"
export NATIVE_ABI_EVIDENCE="$evidence/lifecycle"
export EVIDENCE_DIR="$evidence/staging-controls"
cd "$repo"
git rev-parse HEAD > "$evidence/source-head.txt"
git status --porcelain=v1 > "$evidence/source-status.txt"
sha256sum scripts/test-native-lifecycle.sh scripts/build-refc.sh scripts/test-refc-staging.sh tests/check-native-nif-exports.sh scripts/check-native-nif-exports.sh scripts/check-abi-ledger.sh scripts/test-abi-ledger.sh > "$evidence/driver-inputs.sha256"
bash scripts/test-refc-staging.sh
bash tests/check-native-nif-exports.sh
bash scripts/test-abi-ledger.sh
bash scripts/test-native-lifecycle.sh
sha256sum --check --quiet "$evidence/driver-inputs.sha256"
echo 'PASS: integrated lifecycle proofs, real native consumers, ledger and corruption controls. Header-only and full-package jobs are separate.'
