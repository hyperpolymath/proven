# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# ClusterFuzzLite fuzzing container for proven.
#
# Base: ClusterFuzzLite's rust image (libfuzzer + cargo-fuzz pre-installed)
# Adds: Zig and pinned Idris/Chez so build.sh can compile the RefC-backed fixture
# prior to building the supported fuzz targets.

FROM gcr.io/oss-fuzz-base/base-builder-rust:v1@sha256:8a9f7d1ee933da04fc7b616c51d3fb99009692f6aa93ca9e972a6a39730e14f5

# Zig download dependencies.
RUN apt-get update && apt-get install -y --no-install-recommends \
      curl \
      ca-certificates \
      xz-utils \
      git make gcc libgmp-dev uuid-dev \
    && rm -rf /var/lib/apt/lists/*

# Pin Zig 0.15.2 (matches ffi/zig/build.zig.zon's minimum).
ARG ZIG_VERSION=0.15.2
# Listed at ziglang.org/download/community-mirrors.txt; content remains SHA-pinned.
ARG ZIG_TARBALL="https://pkg.hexops.org/zig/zig-x86_64-linux-${ZIG_VERSION}.tar.xz?source=proven-clusterfuzzlite"
ARG ZIG_SHA256="02aa270f183da276e5b5920b1dac44a63f1a49e55050ebde3aecc9eb82f93239"
RUN curl -fsSL --proto '=https' --proto-redir '=https' --connect-timeout 20 --max-time 180 "$ZIG_TARBALL" -o /tmp/zig.tar.xz \
    && echo "$ZIG_SHA256  /tmp/zig.tar.xz" | sha256sum -c - \
    && tar -xJ -C /opt -f /tmp/zig.tar.xz \
    && ln -s "/opt/zig-x86_64-linux-${ZIG_VERSION}/zig" /usr/local/bin/zig \
    && rm /tmp/zig.tar.xz

# The base image's Chez 9.5 lacks collect-rendezvous, required by this Idris
# bootstrap. Build Chez 9.5.8 and its exact gitlink-pinned dependencies.
RUN git init /tmp/chez \
    && git -C /tmp/chez remote add origin https://github.com/cisco/ChezScheme.git \
    && git -C /tmp/chez fetch --depth 1 origin 9df56e7b25bc523663eac3da24be33afc5f76c84 \
    && git -C /tmp/chez checkout --detach FETCH_HEAD \
    && test "$(git -C /tmp/chez rev-parse HEAD)" = 9df56e7b25bc523663eac3da24be33afc5f76c84 \
    && git -C /tmp/chez submodule update --init --recursive --depth 1 \
    && cd /tmp/chez \
    && env -u CFLAGS -u CXXFLAGS -u LDFLAGS CC=gcc ./configure --threads --disable-curses --disable-x11 --installprefix=/opt/chez \
    && env -u CFLAGS -u CXXFLAGS -u LDFLAGS make -j2 install CC=gcc \
    && printf '(import (chezscheme)) (unless (procedure? collect-rendezvous) (exit 1)) (display (scheme-version)) (newline)\n' | /opt/chez/bin/scheme -q \
    && rm -rf /tmp/chez
ENV PATH="/opt/chez/bin:${PATH}"

# Build the same pinned public Idris compiler used by native integration.
# Keep this layer before COPY so source edits do not rebuild the toolchain.
RUN git init /tmp/idris2 \
    && git -C /tmp/idris2 remote add origin https://github.com/idris-lang/Idris2.git \
    && git -C /tmp/idris2 fetch --depth 1 origin b6cbc25d5cd2ef2a250556266fd7b68dfc360f8b \
    && git -C /tmp/idris2 checkout --detach FETCH_HEAD \
    && test "$(git -C /tmp/idris2 rev-parse HEAD)" = b6cbc25d5cd2ef2a250556266fd7b68dfc360f8b \
    && cd /tmp/idris2 \
    && env -u CFLAGS -u CXXFLAGS -u LDFLAGS make bootstrap SCHEME=/opt/chez/bin/scheme CC=gcc PREFIX=/opt/idris2 \
    && env -u CFLAGS -u CXXFLAGS -u LDFLAGS make install CC=gcc PREFIX=/opt/idris2 \
    && rm -rf /tmp/idris2
ENV PATH="/opt/idris2/bin:${PATH}"

# Project source is mounted at $SRC/proven by ClusterFuzzLite.
WORKDIR $SRC/proven
COPY . $SRC/proven

# Make build.sh the container entrypoint (ClusterFuzzLite convention).
COPY .clusterfuzzlite/build.sh $SRC/build.sh
RUN chmod +x $SRC/build.sh
