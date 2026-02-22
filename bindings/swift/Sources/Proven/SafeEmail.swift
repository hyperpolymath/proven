// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe email validation delegated to libproven FFI.
///
/// RFC 5321 compliant validation performed by the formally verified
/// Idris 2 core via the Zig FFI bridge.

import CProven

public enum SafeEmail {
    /// Validate an email address (RFC 5321 simplified).
    public static func isValid(_ email: String) -> Result<Bool, ProvenError> {
        withStringBytes(email) { ptr, len in
            let result = proven_email_is_valid(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }
}
