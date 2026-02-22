// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeEmail - Email validation via libproven FFI.
// All computation is performed in verified Idris 2 code via the Zig FFI
// layer. This Wren class is a thin wrapper; it does NOT reimplement any logic.

// Email address validation (RFC 5321 simplified).
// Returns null on error instead of crashing.
class SafeEmail {
    // Validate an email address.
    // Returns true if valid, false if invalid, null on error.
    foreign static isValid(s)
}
