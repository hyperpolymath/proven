// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe email validation.

use crate::core::{Error, Result};

/// Safe email operations.
pub struct SafeEmail;

impl SafeEmail {
    /// Validate an email address (basic RFC 5321 check).
    pub fn is_valid(email: &str) -> bool {
        Self::validate(email).is_ok()
    }

    /// Validate and return parsed email.
    pub fn validate(email: &str) -> Result<(String, String)> {
        let email = email.trim();
        if email.is_empty() {
            return Err(Error::EmptyInput);
        }

        let parts: Vec<&str> = email.rsplitn(2, '@').collect();
        if parts.len() != 2 {
            return Err(Error::InvalidFormat("Missing @ symbol".into()));
        }

        let domain = parts[0];
        let local = parts[1];

        if local.is_empty() {
            return Err(Error::InvalidFormat("Empty local part".into()));
        }
        if domain.is_empty() {
            return Err(Error::InvalidFormat("Empty domain".into()));
        }
        if local.len() > 64 {
            return Err(Error::InvalidFormat("Local part too long".into()));
        }
        if domain.len() > 255 {
            return Err(Error::InvalidFormat("Domain too long".into()));
        }
        if !domain.contains('.') {
            return Err(Error::InvalidFormat("Domain must contain a dot".into()));
        }

        Ok((local.to_string(), domain.to_string()))
    }

    /// Extract local part from email.
    pub fn get_local_part(email: &str) -> Result<String> {
        Self::validate(email).map(|(local, _)| local)
    }

    /// Extract domain from email.
    pub fn get_domain(email: &str) -> Result<String> {
        Self::validate(email).map(|(_, domain)| domain)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_emails() {
        assert!(SafeEmail::is_valid("user@example.com"));
        assert!(SafeEmail::is_valid("user.name@example.co.uk"));
    }

    #[test]
    fn test_invalid_emails() {
        assert!(!SafeEmail::is_valid("not-an-email"));
        assert!(!SafeEmail::is_valid("@example.com"));
        assert!(!SafeEmail::is_valid("user@"));
    }
}
