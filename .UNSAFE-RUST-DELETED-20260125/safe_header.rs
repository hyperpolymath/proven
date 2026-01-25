// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe HTTP Header operations that prevent CRLF injection attacks.
//!
//! All operations handle injection attacks and size limits without panicking.
//! Operations return `Result` on failure.

use crate::core::{Error, Result};

/// HTTP header name/value pair
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Header {
    /// Header name (validated token)
    pub name: String,
    /// Header value (CRLF-free)
    pub value: String,
}

/// SameSite attribute for cookies
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SameSite {
    /// Strict same-site policy
    Strict,
    /// Lax same-site policy
    Lax,
    /// No same-site restriction (requires Secure)
    None,
}

/// Safe header operations
pub struct SafeHeader;

impl SafeHeader {
    /// Check if a string contains CRLF injection characters
    pub fn has_crlf(s: &str) -> bool {
        s.contains('\r') || s.contains('\n')
    }

    /// Check if header name is a valid token per RFC 7230
    pub fn is_valid_name(name: &str) -> bool {
        if name.is_empty() || name.len() > 256 {
            return false;
        }
        name.chars().all(Self::is_valid_token_char)
    }

    /// Check if character is valid for HTTP token
    fn is_valid_token_char(c: char) -> bool {
        // Token chars per RFC 7230: !#$%&'*+-.^_`|~ plus alphanumeric
        matches!(c,
            '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' |
            '^' | '_' | '`' | '|' | '~' |
            'a'..='z' | 'A'..='Z' | '0'..='9'
        )
    }

    /// List of dangerous headers that should not be set by user code
    const DANGEROUS_HEADERS: &'static [&'static str] = &[
        "proxy-authorization",
        "proxy-authenticate",
        "proxy-connection",
        "transfer-encoding",
        "content-length",
        "host",
        "connection",
        "keep-alive",
        "upgrade",
        "te",
        "trailer",
    ];

    /// Check if header name is in the dangerous headers list
    pub fn is_dangerous(name: &str) -> bool {
        let lower = name.to_lowercase();
        Self::DANGEROUS_HEADERS.contains(&lower.as_str())
    }

    /// Create a validated header
    pub fn make(name: &str, value: &str) -> Result<Header> {
        let trimmed_name = name.trim();
        let trimmed_value = value.trim();

        if !Self::is_valid_name(trimmed_name) {
            return Err(Error::InvalidFormat("Invalid header name".into()));
        }

        if Self::has_crlf(trimmed_value) {
            return Err(Error::InvalidFormat("Header value contains CRLF".into()));
        }

        if trimmed_value.len() > 8192 {
            return Err(Error::TooLong("Header value too long".into()));
        }

        Ok(Header {
            name: trimmed_name.to_string(),
            value: trimmed_value.to_string(),
        })
    }

    /// Create header, blocking dangerous headers
    pub fn make_safe(name: &str, value: &str) -> Result<Header> {
        if Self::is_dangerous(name) {
            return Err(Error::InvalidFormat("Dangerous header not allowed".into()));
        }
        Self::make(name, value)
    }

    /// Render header to "Name: Value" format
    pub fn render(header: &Header) -> String {
        format!("{}: {}", header.name, header.value)
    }

    /// Build Strict-Transport-Security header value
    pub fn build_hsts(max_age: u64, include_subdomains: bool, preload: bool) -> String {
        let mut value = format!("max-age={}", max_age);
        if include_subdomains {
            value.push_str("; includeSubDomains");
        }
        if preload {
            value.push_str("; preload");
        }
        value
    }

    /// Build Content-Security-Policy header value from directives
    pub fn build_csp(directives: &[(String, Vec<String>)]) -> String {
        directives
            .iter()
            .map(|(name, sources)| {
                if sources.is_empty() {
                    name.clone()
                } else {
                    format!("{} {}", name, sources.join(" "))
                }
            })
            .collect::<Vec<_>>()
            .join("; ")
    }

    /// Get common security headers preset
    pub fn security_headers() -> Vec<Header> {
        vec![
            Header {
                name: "X-Frame-Options".to_string(),
                value: "DENY".to_string(),
            },
            Header {
                name: "X-Content-Type-Options".to_string(),
                value: "nosniff".to_string(),
            },
            Header {
                name: "Referrer-Policy".to_string(),
                value: "strict-origin-when-cross-origin".to_string(),
            },
            Header {
                name: "X-XSS-Protection".to_string(),
                value: "1; mode=block".to_string(),
            },
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_has_crlf() {
        assert!(!SafeHeader::has_crlf("application/json"));
        assert!(SafeHeader::has_crlf("text\r\nX-Injected: evil"));
        assert!(SafeHeader::has_crlf("value\ninjected"));
    }

    #[test]
    fn test_is_valid_name() {
        assert!(SafeHeader::is_valid_name("Content-Type"));
        assert!(SafeHeader::is_valid_name("X-Custom-Header"));
        assert!(!SafeHeader::is_valid_name("Content:Type"));
        assert!(!SafeHeader::is_valid_name(""));
    }

    #[test]
    fn test_is_dangerous() {
        assert!(SafeHeader::is_dangerous("Host"));
        assert!(SafeHeader::is_dangerous("Transfer-Encoding"));
        assert!(!SafeHeader::is_dangerous("X-Custom-Header"));
    }

    #[test]
    fn test_build_hsts() {
        let hsts = SafeHeader::build_hsts(31536000, true, true);
        assert_eq!(hsts, "max-age=31536000; includeSubDomains; preload");
    }

    #[test]
    fn test_make_header() {
        let header = SafeHeader::make("Content-Type", "application/json").unwrap();
        assert_eq!(header.name, "Content-Type");
        assert_eq!(header.value, "application/json");
    }

    #[test]
    fn test_reject_crlf() {
        let result = SafeHeader::make("Content-Type", "text\r\nX-Injected: evil");
        assert!(result.is_err());
    }
}
