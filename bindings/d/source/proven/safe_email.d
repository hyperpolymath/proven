// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe email validation and parsing.
 *
 * Thin FFI wrapper around libproven's SafeEmail module. Email validation
 * (RFC 5321 simplified) is performed in formally verified Idris 2 code.
 * This module only marshals data to/from the C ABI.
 */
module proven.safe_email;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Check if email format is valid (RFC 5321 simplified).
bool isValidEmail(string email) @trusted nothrow @nogc
{
    if (email.length == 0)
        return false;
    auto result = proven_email_is_valid(
        cast(const(ubyte)*) email.ptr, email.length
    );
    if (provenFailed(result.status))
        return false;
    return result.value;
}
