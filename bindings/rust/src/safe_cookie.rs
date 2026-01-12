// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe HTTP Cookie operations that prevent injection attacks.
//!
//! All operations handle injection attacks (semicolons, newlines) and
//! enforce security requirements without panicking.

use crate::core::{Error, Result};

/// SameSite attribute values
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SameSite {
    /// Strict same-site policy
    Strict,
    /// Lax same-site policy (default)
    #[default]
    Lax,
    /// No same-site restriction (requires Secure)
    None,
}

/// Cookie prefix type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CookiePrefix {
    /// No prefix
    NoPrefix,
    /// __Secure- prefix
    SecurePrefix,
    /// __Host- prefix
    HostPrefix,
}

/// Cookie attributes
#[derive(Debug, Clone)]
pub struct CookieAttributes {
    /// Domain scope
    pub domain: Option<String>,
    /// Path scope
    pub path: Option<String>,
    /// Max-Age in seconds
    pub max_age: Option<i64>,
    /// Secure flag (HTTPS only)
    pub secure: bool,
    /// HttpOnly flag (no JS access)
    pub http_only: bool,
    /// SameSite attribute
    pub same_site: Option<SameSite>,
    /// Partitioned (CHIPS)
    pub partitioned: bool,
}

impl Default for CookieAttributes {
    fn default() -> Self {
        Self {
            domain: None,
            path: Some("/".to_string()),
            max_age: None,
            secure: true,
            http_only: true,
            same_site: Some(SameSite::Lax),
            partitioned: false,
        }
    }
}

/// Complete cookie
#[derive(Debug, Clone)]
pub struct Cookie {
    /// Cookie name
    pub name: String,
    /// Cookie value
    pub value: String,
    /// Cookie attributes
    pub attributes: CookieAttributes,
}

/// Safe cookie operations
pub struct SafeCookie;

impl SafeCookie {
    /// Check for injection characters (semicolon, CR, LF)
    pub fn has_injection(s: &str) -> bool {
        s.contains(';') || s.contains('\r') || s.contains('\n')
    }

    /// Get prefix from cookie name
    pub fn get_prefix(name: &str) -> CookiePrefix {
        if name.starts_with("__Host-") {
            CookiePrefix::HostPrefix
        } else if name.starts_with("__Secure-") {
            CookiePrefix::SecurePrefix
        } else {
            CookiePrefix::NoPrefix
        }
    }

    /// Validate cookie name
    pub fn validate_name(name: &str) -> Result<&str> {
        if name.is_empty() || name.len() > 256 {
            return Err(Error::TooLong("Cookie name length invalid".into()));
        }
        if Self::has_injection(name) {
            return Err(Error::InvalidFormat("Cookie name contains injection chars".into()));
        }
        Ok(name)
    }

    /// Validate cookie value
    pub fn validate_value(value: &str) -> Result<&str> {
        if value.len() > 4096 {
            return Err(Error::TooLong("Cookie value too long".into()));
        }
        if Self::has_injection(value) {
            return Err(Error::InvalidFormat("Cookie value contains injection chars".into()));
        }
        Ok(value)
    }

    /// Validate prefix requirements
    fn validate_prefix(name: &str, attrs: &CookieAttributes) -> Result<()> {
        match Self::get_prefix(name) {
            CookiePrefix::NoPrefix => Ok(()),
            CookiePrefix::SecurePrefix => {
                if attrs.secure {
                    Ok(())
                } else {
                    Err(Error::InvalidFormat("__Secure- requires Secure flag".into()))
                }
            }
            CookiePrefix::HostPrefix => {
                if attrs.secure && attrs.domain.is_none() && attrs.path == Some("/".to_string()) {
                    Ok(())
                } else {
                    Err(Error::InvalidFormat(
                        "__Host- requires Secure, no Domain, Path=/".into(),
                    ))
                }
            }
        }
    }

    /// Validate SameSite=None requires Secure
    fn validate_same_site(attrs: &CookieAttributes) -> Result<()> {
        if attrs.same_site == Some(SameSite::None) && !attrs.secure {
            Err(Error::InvalidFormat("SameSite=None requires Secure".into()))
        } else {
            Ok(())
        }
    }

    /// Create a validated cookie
    pub fn make(name: &str, value: &str, attrs: CookieAttributes) -> Result<Cookie> {
        let validated_name = Self::validate_name(name)?;
        let validated_value = Self::validate_value(value)?;
        Self::validate_prefix(validated_name, &attrs)?;
        Self::validate_same_site(&attrs)?;

        Ok(Cookie {
            name: validated_name.to_string(),
            value: validated_value.to_string(),
            attributes: attrs,
        })
    }

    /// Create cookie with default secure attributes
    pub fn make_default(name: &str, value: &str) -> Result<Cookie> {
        Self::make(name, value, CookieAttributes::default())
    }

    /// Create session cookie (strict security)
    pub fn make_session(name: &str, value: &str) -> Result<Cookie> {
        Self::make(
            name,
            value,
            CookieAttributes {
                same_site: Some(SameSite::Strict),
                ..Default::default()
            },
        )
    }

    /// Build Set-Cookie header value
    pub fn build_set_cookie(cookie: &Cookie) -> String {
        let mut parts = vec![format!("{}={}", cookie.name, cookie.value)];

        if let Some(ref domain) = cookie.attributes.domain {
            parts.push(format!("Domain={}", domain));
        }

        if let Some(ref path) = cookie.attributes.path {
            parts.push(format!("Path={}", path));
        }

        if let Some(max_age) = cookie.attributes.max_age {
            parts.push(format!("Max-Age={}", max_age));
        }

        if cookie.attributes.secure {
            parts.push("Secure".to_string());
        }

        if cookie.attributes.http_only {
            parts.push("HttpOnly".to_string());
        }

        if let Some(same_site) = cookie.attributes.same_site {
            parts.push(format!(
                "SameSite={}",
                match same_site {
                    SameSite::Strict => "Strict",
                    SameSite::Lax => "Lax",
                    SameSite::None => "None",
                }
            ));
        }

        if cookie.attributes.partitioned {
            parts.push("Partitioned".to_string());
        }

        parts.join("; ")
    }

    /// Build delete cookie header value
    pub fn build_delete(name: &str) -> Result<String> {
        Self::validate_name(name)?;
        Ok(format!(
            "{}=; Max-Age=0; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT",
            name
        ))
    }

    /// Parse Cookie header into name-value pairs
    pub fn parse_cookie_header(header: &str) -> Vec<(String, String)> {
        header
            .split(';')
            .filter_map(|pair| {
                let trimmed = pair.trim();
                if trimmed.is_empty() {
                    return None;
                }
                match trimmed.find('=') {
                    Some(idx) => Some((
                        trimmed[..idx].trim().to_string(),
                        trimmed[idx + 1..].trim().to_string(),
                    )),
                    None => Some((trimmed.to_string(), String::new())),
                }
            })
            .collect()
    }

    /// Common expiration durations in seconds
    pub const ONE_HOUR: i64 = 3600;
    pub const ONE_DAY: i64 = 86400;
    pub const ONE_WEEK: i64 = 604800;
    pub const THIRTY_DAYS: i64 = 2592000;
    pub const ONE_YEAR: i64 = 31536000;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_has_injection() {
        assert!(!SafeCookie::has_injection("session_token"));
        assert!(SafeCookie::has_injection("value;injected=true"));
        assert!(SafeCookie::has_injection("value\ninjected"));
    }

    #[test]
    fn test_get_prefix() {
        assert_eq!(SafeCookie::get_prefix("__Host-session"), CookiePrefix::HostPrefix);
        assert_eq!(SafeCookie::get_prefix("__Secure-token"), CookiePrefix::SecurePrefix);
        assert_eq!(SafeCookie::get_prefix("regular"), CookiePrefix::NoPrefix);
    }

    #[test]
    fn test_make_cookie() {
        let cookie = SafeCookie::make_default("session", "abc123").unwrap();
        assert_eq!(cookie.name, "session");
        assert_eq!(cookie.value, "abc123");
        assert!(cookie.attributes.secure);
        assert!(cookie.attributes.http_only);
    }

    #[test]
    fn test_reject_injection() {
        let result = SafeCookie::make_default("session", "value;injected=true");
        assert!(result.is_err());
    }

    #[test]
    fn test_build_set_cookie() {
        let cookie = SafeCookie::make_default("session", "abc123").unwrap();
        let header = SafeCookie::build_set_cookie(&cookie);
        assert!(header.contains("session=abc123"));
        assert!(header.contains("Secure"));
        assert!(header.contains("HttpOnly"));
        assert!(header.contains("SameSite=Lax"));
    }

    #[test]
    fn test_host_prefix_requirements() {
        // __Host- requires Secure, no Domain, Path=/
        let result = SafeCookie::make(
            "__Host-session",
            "abc123",
            CookieAttributes {
                domain: Some("example.com".to_string()), // This should fail
                ..Default::default()
            },
        );
        assert!(result.is_err());
    }
}
