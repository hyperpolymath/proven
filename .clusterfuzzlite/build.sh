#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# ClusterFuzzLite build script. Builds the standalone Zig FFI surface as
# libproven.so, then builds the cargo-fuzz targets that are backed by that
# surface.
#
# Runs inside the ClusterFuzzLite container (see Dockerfile). Outputs
# the fuzz target binaries to $OUT, the path the runner expects.
#
# Replaces the prior Zig-only build that pointed at a non-existent
# `proven/fuzz/zig/` directory. The Rust bindings are the natural FFI
# fuzz surface: panic safety + allocator handling are exactly what
# libFuzzer is good at.

set -euo pipefail

echo "=== ClusterFuzzLite build: proven ==="
mkdir -p "$OUT"

# Build the standalone Zig FFI fixture as libproven.so. The full Idris RefC
# bridge is exercised by ffi-full-integration.yml when its private dependency
# credential is configured; this PR fuzz gate must stay self-contained.
if [[ -f ffi/zig/build.zig ]]; then
  pushd ffi/zig >/dev/null
  mkdir -p zig-out/lib
  zig build-lib -static -O ReleaseSafe -fPIC --name proven \
    -femit-bin=zig-out/lib/libproven.a src/main.zig -lc || {
    echo "WARN: standalone Zig FFI build failed; fuzz targets will not link." >&2
    exit 1
  }
  popd >/dev/null
fi

# Make libproven discoverable to the cargo-fuzz linker.
export PROVEN_LIB_DIR="$PWD/ffi/zig/zig-out/lib"
export LD_LIBRARY_PATH="$PROVEN_LIB_DIR:${LD_LIBRARY_PATH:-}"
export CXXFLAGS="${CXXFLAGS:+$CXXFLAGS }-stdlib=libstdc++"

# Build cargo-fuzz targets and stage them at $OUT (ClusterFuzzLite
# convention; runner reads from there).
pushd bindings/rust/fuzz >/dev/null
cargo +nightly fuzz build safe_path_has_traversal -O --debug-assertions

# Copy each compiled fuzzer + libproven.so into $OUT.
TARGETS_DIR="target/x86_64-unknown-linux-gnu/release"
for fuzzer in safe_path_has_traversal; do
  if [[ -f "$TARGETS_DIR/$fuzzer" ]]; then
    cp "$TARGETS_DIR/$fuzzer" "$OUT/"
  else
    echo "WARN: missing fuzz binary $fuzzer (build failed)" >&2
  fi
done

# Bundle libproven.a so the fuzzer build can find it (for static linking).
# Note: ClusterFuzzLite binaries should be self-contained; static linking
# is preferred to avoid runtime library loading issues.
popd >/dev/null

echo "=== Build complete. Staged to $OUT. ==="
