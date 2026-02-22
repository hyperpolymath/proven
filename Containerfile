# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven: Formally Verified Safety Library
# Multi-stage build for minimal container size
# Runtime: podman (never docker)

FROM cgr.dev/chainguard/wolfi-base:latest AS runtime

LABEL org.opencontainers.image.title="Proven"
LABEL org.opencontainers.image.description="Formally verified safety library for all major programming languages"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/proven"
LABEL org.opencontainers.image.licenses="PMPL-1.0-or-later"
LABEL org.opencontainers.image.vendor="Jonathan D.A. Jewell (hyperpolymath)"

# Install runtime dependencies
# wolfi-base uses apk (Alpine-compatible package manager)
RUN apk add --no-cache \
    ca-certificates \
    libffi

# Create non-root user
RUN adduser -D -h /home/proven -s /bin/sh proven
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
