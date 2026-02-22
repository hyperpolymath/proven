# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeCrypto.io - Cryptographic primitives for the Io language.
#
# All operations delegate to libproven via the native C addon (IoProven.c).
# Returns nil on error.
#
# Usage:
#   Proven init
#   eq := Proven constantTimeEq("secret", "secret")
#   eq println   # true
#   hex := Proven randomHex(16)
#   hex println  # 32-character hex string
#   Proven deinit

SafeCrypto := Proven clone do(
    //doc SafeCrypto category Safety
    //doc SafeCrypto description Timing-safe comparison and secure random generation.

    //doc SafeCrypto constantTimeEq(a, b) Constant-time byte comparison (timing-attack safe).
    # constantTimeEq is provided by the native C addon (IoProven.c).

    //doc SafeCrypto randomHex(nbytes) Generate cryptographically secure random bytes as hex (1-1024 bytes).
    # randomHex is provided by the native C addon (IoProven.c).

    //doc SafeCrypto hexEncode(data) Encode bytes as lowercase hex string.
    # hexEncode is provided by the native C addon (IoProven.c).

    //doc SafeCrypto hexDecode(hex_str) Decode hex string to bytes.
    # hexDecode is provided by the native C addon (IoProven.c).

    //doc SafeCrypto crc32(data) Compute CRC32 checksum.
    # crc32 is provided by the native C addon (IoProven.c).
)
