# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
Proven -- Mojo FFI bindings for libproven.

All computation is performed in the formally verified Idris 2 core via C FFI.
These bindings are thin wrappers that marshal data to and from the C ABI
exposed by the Zig FFI bridge. No logic is reimplemented here.

Architecture:
    Mojo code (this package)
        -> external_call (C FFI)
            -> libproven.so / libproven.a (Zig-compiled)
                -> Idris 2 verified core (all computation)

Modules:
    lib_proven    -- Raw C FFI declarations and result types
    safe_math     -- Overflow-safe integer and float arithmetic
    safe_string   -- Encoding-safe text operations (SQL, HTML, JS escaping)
    safe_path     -- Filesystem path traversal prevention
    safe_email    -- RFC 5321 email validation
    safe_url      -- URL parsing and component extraction
    safe_network  -- IPv4 address parsing and classification
    safe_crypto   -- Timing-safe comparison and secure random bytes
    safe_json     -- JSON validation and type detection
    safe_datetime -- ISO 8601 date/time parsing and formatting
"""
