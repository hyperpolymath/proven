# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# ClusterFuzzLite fuzzing container for proven.
#
# Base: ClusterFuzzLite's rust image (libfuzzer + cargo-fuzz pre-installed)
# Adds: Zig + Idris2 so build.sh can compile libproven.so prior to
# building the fuzz targets.

FROM gcr.io/oss-fuzz-base/base-builder-rust:v1@sha256:5c2cdc8a9d8b6ad0bf7e9f44fdcd0d7cd7a8b6eca5e8d3c5a89e3e5d8e1c7a3f

# Idris2 + Zig dependencies. ChezScheme is the Idris2 bootstrap target;
# gmp is the bignum dependency.
RUN apt-get update && apt-get install -y --no-install-recommends \
      chezscheme \
      libgmp-dev \
      curl \
      ca-certificates \
      git \
      build-essential \
    && rm -rf /var/lib/apt/lists/*

# Pin Zig 0.13.0 (matches proven's .github/workflows/zig-ffi.yml).
ARG ZIG_VERSION=0.13.0
RUN curl -sL "https://ziglang.org/download/${ZIG_VERSION}/zig-linux-x86_64-${ZIG_VERSION}.tar.xz" \
      | tar -xJ -C /opt \
    && ln -s "/opt/zig-linux-x86_64-${ZIG_VERSION}/zig" /usr/local/bin/zig

# Build Idris2 0.8.0 from source (matches proven's idris2-ci.yml).
ARG IDRIS2_VERSION=0.8.0
RUN git clone --depth 1 --branch "v${IDRIS2_VERSION}" \
      https://github.com/idris-lang/Idris2.git /tmp/idris2 \
    && cd /tmp/idris2 \
    && make bootstrap SCHEME=chezscheme \
    && make install \
    && rm -rf /tmp/idris2

ENV PATH="/root/.idris2/bin:${PATH}"

# Project source is mounted at $SRC/proven by ClusterFuzzLite.
WORKDIR $SRC/proven
COPY . $SRC/proven

# Make build.sh the container entrypoint (ClusterFuzzLite convention).
COPY .clusterfuzzlite/build.sh $SRC/build.sh
RUN chmod +x $SRC/build.sh
