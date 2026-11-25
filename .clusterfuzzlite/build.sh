#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# ClusterFuzzLite build script. Builds the RefC-backed Zig FFI archive,
# then links the supported cargo-fuzz target against that implementation.
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

# Fuzz the actual Idris byte kernel through RefC; unavailable-only success
# would exercise no detector semantics. Compiler provisioning is image-pinned.
bundle="$SRC/path-refc"
bash scripts/build-header-refc.sh "$bundle"
pushd ffi/zig >/dev/null
zig build -j2 --build-file build_standalone.zig -Doptimize=ReleaseSafe "-Dheader-refc=$bundle"
cp zig-out/lib/libproven_ffi.a zig-out/lib/libproven.a
popd >/dev/null
# Final-link RefC runtime and support archives, followed by their system
# dependencies. The standalone Zig archive does not absorb these archives.
export RUSTFLAGS="${RUSTFLAGS:-} -C link-arg=$bundle/libidris2_refc.a -C link-arg=$bundle/libidris2_support.a -C link-arg=-lgmp -C link-arg=-lm -C link-arg=-lpthread -C link-arg=-lubsan"
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
