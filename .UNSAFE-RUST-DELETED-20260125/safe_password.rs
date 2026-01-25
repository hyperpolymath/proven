// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe password handling and validation.

use crate::core::{Error, Result};

/// Password strength level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PasswordStrength {
    /// Very weak password
    VeryWeak,
    /// Weak password
    Weak,
    /// Fair password
    Fair,
    /// Strong password
    Strong,
    /// Very strong password
    VeryStrong,
}

/// Password policy configuration.
#[derive(Debug, Clone)]
pub struct PasswordPolicy {
    /// Minimum length required
    pub min_length: usize,
    /// Maximum length allowed
    pub max_length: usize,
    /// Require uppercase letters
    pub require_uppercase: bool,
    /// Require lowercase letters
    pub require_lowercase: bool,
    /// Require digits
    pub require_digit: bool,
    /// Require special characters
    pub require_special: bool,
}

impl Default for PasswordPolicy {
    fn default() -> Self {
        Self::nist()
    }
}

impl PasswordPolicy {
    /// NIST SP 800-63B compliant policy.
    pub fn nist() -> Self {
        PasswordPolicy {
            min_length: 8,
            max_length: 64,
            require_uppercase: false,
            require_lowercase: false,
            require_digit: false,
            require_special: false,
        }
    }

    /// PCI-DSS compliant policy.
    pub fn pci_dss() -> Self {
        PasswordPolicy {
            min_length: 7,
            max_length: 128,
            require_uppercase: true,
            require_lowercase: true,
            require_digit: true,
            require_special: false,
        }
    }

    /// HIPAA compliant policy.
    pub fn hipaa() -> Self {
        PasswordPolicy {
            min_length: 8,
            max_length: 128,
            require_uppercase: true,
            require_lowercase: true,
            require_digit: true,
            require_special: true,
        }
    }
}

/// Safe password operations.
pub struct SafePassword;

impl SafePassword {
    /// Validate password against a policy.
    pub fn validate(password: &str, policy: &PasswordPolicy) -> Result<()> {
        if password.len() < policy.min_length {
            return Err(Error::ValidationError(format!(
                "Password must be at least {} characters",
                policy.min_length
            )));
        }

        if password.len() > policy.max_length {
            return Err(Error::ValidationError(format!(
                "Password must be at most {} characters",
                policy.max_length
            )));
        }

        if policy.require_uppercase && !password.chars().any(|c| c.is_uppercase()) {
            return Err(Error::ValidationError(
                "Password must contain uppercase letter".into(),
            ));
        }

        if policy.require_lowercase && !password.chars().any(|c| c.is_lowercase()) {
            return Err(Error::ValidationError(
                "Password must contain lowercase letter".into(),
            ));
        }

        if policy.require_digit && !password.chars().any(|c| c.is_ascii_digit()) {
            return Err(Error::ValidationError("Password must contain digit".into()));
        }

        if policy.require_special
            && !password.chars().any(|c| !c.is_alphanumeric() && !c.is_whitespace())
        {
            return Err(Error::ValidationError(
                "Password must contain special character".into(),
            ));
        }

        Ok(())
    }

    /// Check password strength.
    pub fn strength(password: &str) -> PasswordStrength {
        let len = password.len();
        let has_upper = password.chars().any(|c| c.is_uppercase());
        let has_lower = password.chars().any(|c| c.is_lowercase());
        let has_digit = password.chars().any(|c| c.is_ascii_digit());
        let has_special = password
            .chars()
            .any(|c| !c.is_alphanumeric() && !c.is_whitespace());

        let variety = [has_upper, has_lower, has_digit, has_special]
            .iter()
            .filter(|&&b| b)
            .count();

        match (len, variety) {
            (0..=5, _) => PasswordStrength::VeryWeak,
            (6..=7, 0..=1) => PasswordStrength::VeryWeak,
            (6..=7, _) => PasswordStrength::Weak,
            (8..=11, 0..=2) => PasswordStrength::Weak,
            (8..=11, _) => PasswordStrength::Fair,
            (12..=15, 0..=2) => PasswordStrength::Fair,
            (12..=15, _) => PasswordStrength::Strong,
            (_, 0..=2) => PasswordStrength::Fair,
            (_, 3) => PasswordStrength::Strong,
            _ => PasswordStrength::VeryStrong,
        }
    }

    /// Check for common password patterns.
    pub fn has_common_pattern(password: &str) -> bool {
        let lower = password.to_lowercase();
        let common = [
            "password", "123456", "qwerty", "admin", "letmein", "welcome", "monkey", "dragon",
            "master", "login",
        ];
        common.iter().any(|&p| lower.contains(p))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strength() {
        assert_eq!(SafePassword::strength("abc"), PasswordStrength::VeryWeak);
        assert_eq!(SafePassword::strength("abcdefgh"), PasswordStrength::Weak);
        assert_eq!(
            SafePassword::strength("Abcdefgh1!"),
            PasswordStrength::Fair
        );
        assert_eq!(
            SafePassword::strength("Abcdefghijkl1!"),
            PasswordStrength::Strong
        );
    }

    #[test]
    fn test_validate() {
        let policy = PasswordPolicy::hipaa();
        assert!(SafePassword::validate("Short1!", &policy).is_err());
        assert!(SafePassword::validate("LongEnough1!", &policy).is_ok());
    }
}
