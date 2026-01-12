// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! # Proven - Safe, Formally Verified Library
//!
//! Rust bindings for the proven library, providing formally verified
//! implementations for safe math, cryptography, parsing, and validation.
//!
//! ## Features
//!
//! - **Safe Math**: Overflow detection, safe division, bounded integers
//! - **Safe Strings**: UTF-8 validation, injection-safe escaping
//! - **Safe JSON**: Exception-free parsing with type-safe access
//! - **Safe URLs**: RFC 3986 compliant parsing
//! - **Safe Email**: RFC 5321/5322 validation
//! - **Safe Paths**: Traversal prevention, glob matching
//! - **Safe Crypto**: Secure hashing, HMAC, random generation (stubs)
//! - **Safe Passwords**: Policy validation, strength analysis
//! - **Safe DateTime**: ISO 8601 parsing, timezone handling
//! - **Safe Network**: IPv4/IPv6 parsing, CIDR, ports
//!
//! ## Example
//!
//! ```rust
//! use proven::{SafeMath, SafeString, Result};
//!
//! // Safe addition with overflow detection
//! let result = SafeMath::add(i64::MAX, 1);
//! assert!(result.is_err());
//!
//! // Safe HTML escaping
//! let escaped = SafeString::escape_html("<script>alert('xss')</script>");
//! assert!(!escaped.contains('<'));
//! ```

#![forbid(unsafe_code)]
#![warn(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]

pub mod core;
pub mod safe_crypto;
pub mod safe_datetime;
pub mod safe_email;
pub mod safe_json;
pub mod safe_math;
pub mod safe_network;
pub mod safe_password;
pub mod safe_path;
pub mod safe_string;
pub mod safe_url;

// Re-exports for convenience
pub use crate::core::{Bounded, Error, NonEmpty, Result};
pub use safe_crypto::SafeCrypto;
pub use safe_datetime::SafeDateTime;
pub use safe_email::SafeEmail;
pub use safe_json::SafeJson;
pub use safe_math::SafeMath;
pub use safe_network::SafeNetwork;
pub use safe_password::SafePassword;
pub use safe_path::SafePath;
pub use safe_string::SafeString;
pub use safe_url::SafeUrl;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_add_overflow() {
        let result = SafeMath::add(i64::MAX, 1);
        assert!(result.is_err());
    }

    #[test]
    fn test_safe_add_normal() {
        let result = SafeMath::add(1, 2);
        assert_eq!(result.unwrap(), 3);
    }

    #[test]
    fn test_safe_div_by_zero() {
        let result = SafeMath::div(10, 0);
        assert!(result.is_err());
    }

    #[test]
    fn test_html_escape() {
        let escaped = SafeString::escape_html("<script>");
        assert_eq!(escaped, "&lt;script&gt;");
    }

    #[test]
    fn test_sql_escape() {
        let escaped = SafeString::escape_sql("O'Brien");
        assert_eq!(escaped, "O''Brien");
    }
}
