// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe password validation delegated to libproven FFI.
///
/// All validation and common-password checking is performed by the
/// formally verified Idris 2 core via the Zig FFI bridge.

import CProven

/// Password validation result from the FFI layer.
public struct PasswordValidationResult {
    public let score: UInt32
    public let hasUppercase: Bool
    public let hasLowercase: Bool
    public let hasDigit: Bool
    public let hasSpecial: Bool
}

public enum SafePassword {
    /// Validate a password and return its analysis (score and character class flags).
    public static func validate(_ password: String) -> Result<PasswordValidationResult, ProvenError> {
        withStringBytes(password) { ptr, len in
            let result = proven_password_validate(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(PasswordValidationResult(
                score: result.score,
                hasUppercase: result.has_uppercase,
                hasLowercase: result.has_lowercase,
                hasDigit: result.has_digit,
                hasSpecial: result.has_special
            ))
        }
    }

    /// Check if a password appears in the common passwords list.
    public static func isCommon(_ password: String) -> Bool {
        let utf8 = Array(password.utf8)
        return utf8.withUnsafeBufferPointer { buffer in
            proven_password_is_common(buffer.baseAddress, buffer.count)
        }
    }
}
