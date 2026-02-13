#!/bin/bash
# SPDX-License-Identifier: Apache-2.0
# Build script for ClusterFuzzLite fuzzing

cd "$SRC"/proven/fuzz/zig

# Build fuzz targets with Zig
# Note: Zig produces native binaries compatible with libFuzzer

for target in fuzz_*.zig; do
    name="${target%.zig}"
    zig build-exe "$target" \
        -O ReleaseSafe \
        -fno-stack-check \
        -target x86_64-linux-gnu \
        --name "$name" \
        -lc

    cp "$name" $OUT/
done

echo "Fuzz targets built successfully"
