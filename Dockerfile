# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# Proven: Formally Verified Safety Library
# Multi-stage build for minimal container size

FROM debian:bookworm-slim AS runtime

LABEL org.opencontainers.image.title="Proven"
LABEL org.opencontainers.image.description="Formally verified safety library for all major programming languages"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/proven"
LABEL org.opencontainers.image.licenses="PMPL-1.0"
LABEL org.opencontainers.image.vendor="Hyperpolymath"

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libffi8 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd --create-home --shell /bin/bash proven
USER proven
WORKDIR /home/proven

# Copy pre-built libraries (when available)
# COPY --from=builder /usr/local/lib/libproven.so /usr/local/lib/
# COPY --from=builder /usr/local/include/proven.h /usr/local/include/

# Copy language bindings for reference
COPY --chown=proven:proven bindings/ /home/proven/bindings/
COPY --chown=proven:proven README.adoc /home/proven/
COPY --chown=proven:proven LICENSE /home/proven/

# Default command shows help
CMD ["cat", "/home/proven/README.adoc"]
