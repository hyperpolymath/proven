// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeEmail.cs - Email address validation.
//
// Thin P/Invoke wrapper over libproven. ALL computation is performed in
// verified Idris 2 code via the Zig FFI bridge. No logic is reimplemented here.

namespace Proven
{
    /// <summary>
    /// Safe email validation backed by formally verified Idris 2 code.
    /// Validates email addresses according to RFC 5321 (simplified).
    /// All methods delegate to the libproven FFI. Returns null on error.
    /// </summary>
    public static class SafeEmail
    {
        /// <summary>
        /// Validate whether an email address is well-formed (RFC 5321 simplified).
        /// Delegates to proven_email_is_valid via FFI.
        /// </summary>
        /// <param name="email">The email address string to validate.</param>
        /// <returns>true if valid, false if invalid, null on FFI error.</returns>
        public static bool? IsValid(string email)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(email);
            return MarshalHelpers.BoolResultToNullable(
                LibProven.proven_email_is_valid(bytes, (nuint)bytes.Length));
        }
    }
}
