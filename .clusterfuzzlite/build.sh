#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# ClusterFuzzLite build script. Builds libproven via Zig (which compiles
# Idris2 RefC output) and then builds the cargo-fuzz targets that link
# against it.
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

# Build libproven via the Zig FFI bridge. The container has Idris2 + Zig
# baked in; this step compiles Idris2 → C → libproven.so.
if [[ -f ffi/zig/build.zig ]]; then
  pushd ffi/zig >/dev/null
  zig build || {
    echo "WARN: Zig FFI build failed; fuzz targets will not link." >&2
    echo "      Investigate libproven build before re-running." >&2
    exit 1
  }
  popd >/dev/null
fi

# Make libproven discoverable to the cargo-fuzz linker.
export PROVEN_LIB_DIR="$PWD/ffi/zig/zig-out/lib"
export LD_LIBRARY_PATH="$PROVEN_LIB_DIR:${LD_LIBRARY_PATH:-}"

# Build cargo-fuzz targets and stage them at $OUT (ClusterFuzzLite
# convention; runner reads from there).
pushd bindings/rust/fuzz >/dev/null
cargo fuzz build -O --debug-assertions

# Copy each compiled fuzzer + libproven.so into $OUT.
TARGETS_DIR="target/x86_64-unknown-linux-gnu/release"
for fuzzer in safe_url_parse safe_email_is_valid safe_json_is_valid safe_path_has_traversal; do
  if [[ -f "$TARGETS_DIR/$fuzzer" ]]; then
    cp "$TARGETS_DIR/$fuzzer" "$OUT/"
  else
    echo "WARN: missing fuzz binary $fuzzer (build failed)" >&2
  fi
done

# Bundle libproven.so so the runner can find it at exec time.
mkdir -p "$OUT/lib"
if [[ -f "$PROVEN_LIB_DIR/libproven.so" ]]; then
  cp "$PROVEN_LIB_DIR/libproven.so" "$OUT/lib/"
fi
popd >/dev/null

echo "=== Build complete. Staged to $OUT. ==="
