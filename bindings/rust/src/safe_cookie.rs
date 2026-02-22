// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe HTTP cookie operations via libproven FFI.
//!
//! Validates cookie names and values, prevents injection attacks, and builds
//! Set-Cookie headers. All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Re-export SameSite from FFI for public use.
pub use crate::ffi::SameSite;

/// Cookie prefix type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CookiePrefix {
    /// No prefix.
    None,
    /// `__Secure-` prefix.
    Secure,
    /// `__Host-` prefix.
    Host,
}

/// Cookie attributes for building Set-Cookie headers.
#[derive(Debug, Clone)]
pub struct CookieAttributes {
    /// Domain attribute.
    pub domain: Option<String>,
    /// Path attribute.
    pub path: Option<String>,
    /// Max-Age in seconds (-1 for session cookie).
    pub max_age: i64,
    /// Secure flag.
    pub secure: bool,
    /// HttpOnly flag.
    pub http_only: bool,
    /// SameSite attribute.
    pub same_site: SameSite,
    /// Partitioned attribute.
    pub partitioned: bool,
}

impl Default for CookieAttributes {
    fn default() -> Self {
        CookieAttributes {
            domain: None,
            path: None,
            max_age: -1,
            secure: false,
            http_only: false,
            same_site: SameSite::Lax,
            partitioned: false,
        }
    }
}

/// Validated cookie.
#[derive(Debug, Clone)]
pub struct Cookie {
    /// Cookie name.
    pub name: String,
    /// Cookie value.
    pub value: String,
    /// Cookie attributes.
    pub attributes: CookieAttributes,
}

/// Safe HTTP cookie operations that prevent injection attacks.
pub struct SafeCookie;

impl SafeCookie {
    /// Check for cookie injection characters (semicolon, CR, LF).
    ///
    /// Returns `true` if injection characters are detected.
    pub fn has_injection(value: &str) -> Result<bool> {
        let bytes = value.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_cookie_has_injection(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }

    /// Validate a cookie name.
    ///
    /// Returns `true` if the name is valid per the cookie specification.
    pub fn validate_name(name: &str) -> Result<bool> {
        let bytes = name.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_cookie_validate_name(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }

    /// Validate a cookie value.
    ///
    /// Returns `true` if the value is valid per the cookie specification.
    pub fn validate_value(value: &str) -> Result<bool> {
        let bytes = value.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_cookie_validate_value(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }

    /// Get the cookie prefix type from a cookie name.
    ///
    /// Returns which prefix the cookie name has (if any).
    pub fn get_prefix(name: &str) -> Result<CookiePrefix> {
        let bytes = name.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_cookie_get_prefix(bytes.as_ptr(), bytes.len())
        };
        let value = core::int_result_to_result(result)?;
        match value {
            0 => Ok(CookiePrefix::None),
            1 => Ok(CookiePrefix::Secure),
            2 => Ok(CookiePrefix::Host),
            _ => Ok(CookiePrefix::None),
        }
    }

    /// Build a Set-Cookie header value.
    ///
    /// Validates the name and value, then produces a properly formatted
    /// Set-Cookie header string including all attributes.
    pub fn build_set_cookie(
        name: &str,
        value: &str,
        attrs: &CookieAttributes,
    ) -> Result<String> {
        let name_bytes = name.as_bytes();
        let value_bytes = value.as_bytes();

        // Build the FFI cookie attributes struct
        let domain_bytes = attrs.domain.as_deref().unwrap_or("");
        let path_bytes = attrs.path.as_deref().unwrap_or("");

        let ffi_attrs = ffi::CookieAttributes {
            domain: domain_bytes.as_ptr(),
            domain_len: domain_bytes.len(),
            path: path_bytes.as_ptr(),
            path_len: path_bytes.len(),
            max_age: attrs.max_age,
            secure: attrs.secure,
            http_only: attrs.http_only,
            same_site: attrs.same_site,
            partitioned: attrs.partitioned,
        };

        // SAFETY: We pass valid pointers and lengths from string bytes.
        // The CookieAttributes struct contains valid pointers for the
        // duration of this call.
        let result = unsafe {
            ffi::proven_cookie_build_set_cookie(
                name_bytes.as_ptr(),
                name_bytes.len(),
                value_bytes.as_ptr(),
                value_bytes.len(),
                ffi_attrs,
            )
        };
        core::string_result_to_result(result)
    }

    /// Build a delete cookie header value.
    ///
    /// Creates a Set-Cookie header that expires the named cookie immediately.
    pub fn build_delete(name: &str) -> Result<String> {
        let bytes = name.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_cookie_build_delete(bytes.as_ptr(), bytes.len())
        };
        core::string_result_to_result(result)
    }
}
