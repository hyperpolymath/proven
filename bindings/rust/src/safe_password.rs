// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe password validation via libproven FFI.
//!
//! Validates password strength and checks against common password lists.
//! All operations delegate to Idris 2 verified code.

use crate::core::Result;
use crate::ffi;

/// Re-export PasswordStrength from FFI for public use.
pub use crate::ffi::PasswordStrength;

/// Password validation details.
#[derive(Debug, Clone)]
pub struct PasswordInfo {
    /// Overall strength rating.
    pub strength: PasswordStrength,
    /// Whether the password contains lowercase letters.
    pub has_lowercase: bool,
    /// Whether the password contains uppercase letters.
    pub has_uppercase: bool,
    /// Whether the password contains digits.
    pub has_digit: bool,
    /// Whether the password contains special characters.
    pub has_special: bool,
    /// Password length.
    pub length: usize,
}

/// Safe password operations.
pub struct SafePassword;

impl SafePassword {
    /// Validate password strength.
    ///
    /// Returns detailed information about password characteristics
    /// and an overall strength rating.
    pub fn validate(password: &str) -> PasswordInfo {
        let bytes = password.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer and returns a
        // value-type struct.
        let result = unsafe {
            ffi::proven_password_validate(bytes.as_ptr(), bytes.len())
        };
        PasswordInfo {
            strength: result.strength,
            has_lowercase: result.has_lowercase,
            has_uppercase: result.has_uppercase,
            has_digit: result.has_digit,
            has_special: result.has_special,
            length: result.length,
        }
    }

    /// Check if a password is in the common passwords list.
    ///
    /// Returns `true` if the password is commonly used (and therefore weak).
    pub fn is_common(password: &str) -> bool {
        let bytes = password.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer.
        unsafe { ffi::proven_password_is_common(bytes.as_ptr(), bytes.len()) }
    }
}
