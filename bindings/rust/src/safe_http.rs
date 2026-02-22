// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe HTTP utilities via libproven FFI.
//!
//! Provides URL encoding/decoding and WWW-Authenticate header parsing.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// Parsed WWW-Authenticate challenge.
#[derive(Debug, Clone)]
pub struct AuthChallenge {
    /// Authentication scheme (e.g., "Bearer").
    pub scheme: String,
    /// Realm parameter.
    pub realm: Option<String>,
    /// Service parameter (Docker Registry v2).
    pub service: Option<String>,
    /// Scope parameter (Docker Registry v2).
    pub scope: Option<String>,
}

/// Safe HTTP operations.
pub struct SafeHttp;

impl SafeHttp {
    /// URL-encode a string (RFC 3986 percent encoding).
    ///
    /// Unreserved characters (A-Za-z0-9-._~) pass through unchanged.
    /// All other characters are percent-encoded (%XX).
    pub fn url_encode(input: &str) -> Result<String> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_http_url_encode(bytes.as_ptr(), bytes.len())
        };
        core::string_result_to_result(result)
    }

    /// URL-decode a percent-encoded string.
    pub fn url_decode(input: &str) -> Result<String> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_http_url_decode(bytes.as_ptr(), bytes.len())
        };
        core::string_result_to_result(result)
    }

    /// Parse a WWW-Authenticate header (Docker Registry v2 OAuth2).
    ///
    /// Parses format: `Bearer realm="...",service="...",scope="..."`
    pub fn parse_www_authenticate(input: &str) -> Result<AuthChallenge> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_http_parse_www_authenticate(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;

        // SAFETY: The FFI call succeeded. All string pointers are valid.
        let challenge = unsafe {
            let scheme = if result.challenge.scheme.is_null() {
                String::new()
            } else {
                let slice = std::slice::from_raw_parts(
                    result.challenge.scheme,
                    result.challenge.scheme_len,
                );
                let s = String::from_utf8_lossy(slice).into_owned();
                ffi::proven_free_string(result.challenge.scheme);
                s
            };

            let realm = if result.challenge.realm.is_null()
                || result.challenge.realm_len == 0
            {
                None
            } else {
                let slice = std::slice::from_raw_parts(
                    result.challenge.realm,
                    result.challenge.realm_len,
                );
                let s = String::from_utf8_lossy(slice).into_owned();
                ffi::proven_free_string(result.challenge.realm);
                Some(s)
            };

            let service = if result.challenge.service.is_null()
                || result.challenge.service_len == 0
            {
                None
            } else {
                let slice = std::slice::from_raw_parts(
                    result.challenge.service,
                    result.challenge.service_len,
                );
                let s = String::from_utf8_lossy(slice).into_owned();
                ffi::proven_free_string(result.challenge.service);
                Some(s)
            };

            let scope = if result.challenge.scope.is_null()
                || result.challenge.scope_len == 0
            {
                None
            } else {
                let slice = std::slice::from_raw_parts(
                    result.challenge.scope,
                    result.challenge.scope_len,
                );
                let s = String::from_utf8_lossy(slice).into_owned();
                ffi::proven_free_string(result.challenge.scope);
                Some(s)
            };

            AuthChallenge {
                scheme,
                realm,
                service,
                scope,
            }
        };

        Ok(challenge)
    }
}
