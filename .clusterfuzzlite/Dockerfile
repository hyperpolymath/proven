# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# ClusterFuzzLite fuzzing container for proven.
#
# Base: ClusterFuzzLite's rust image (libfuzzer + cargo-fuzz pre-installed)
# Adds: Zig so build.sh can compile the standalone libproven.so fixture
# prior to building the supported fuzz targets.

FROM gcr.io/oss-fuzz-base/base-builder-rust:v1@sha256:8a9f7d1ee933da04fc7b616c51d3fb99009692f6aa93ca9e972a6a39730e14f5

# Zig download dependencies.
RUN apt-get update && apt-get install -y --no-install-recommends \
      curl \
      ca-certificates \
      xz-utils \
    && rm -rf /var/lib/apt/lists/*

# Pin Zig 0.15.2 (matches ffi/zig/build.zig.zon's minimum).
ARG ZIG_VERSION=0.15.2
ARG ZIG_TARBALL="https://ziglang.org/download/${ZIG_VERSION}/zig-x86_64-linux-${ZIG_VERSION}.tar.xz"
ARG ZIG_SHA256="02aa270f183da276e5b5920b1dac44a63f1a49e55050ebde3aecc9eb82f93239"
RUN curl -fsSL "$ZIG_TARBALL" -o /tmp/zig.tar.xz \
    && echo "$ZIG_SHA256  /tmp/zig.tar.xz" | sha256sum -c - \
    && tar -xJ -C /opt -f /tmp/zig.tar.xz \
    && ln -s "/opt/zig-x86_64-linux-${ZIG_VERSION}/zig" /usr/local/bin/zig \
    && rm /tmp/zig.tar.xz

# Project source is mounted at $SRC/proven by ClusterFuzzLite.
WORKDIR $SRC/proven
COPY . $SRC/proven

# Make build.sh the container entrypoint (ClusterFuzzLite convention).
COPY .clusterfuzzlite/build.sh $SRC/build.sh
RUN chmod +x $SRC/build.sh
