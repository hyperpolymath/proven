# SPDX-License-Identifier: AGPL-3.0-or-later
FROM gcr.io/oss-fuzz-base/base-builder

# Install Zig
RUN apt-get update && apt-get install -y wget xz-utils
RUN wget https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz && \
    tar -xf zig-linux-x86_64-0.13.0.tar.xz && \
    mv zig-linux-x86_64-0.13.0 /usr/local/zig && \
    ln -s /usr/local/zig/zig /usr/local/bin/zig

COPY . $SRC/proven
WORKDIR $SRC/proven
