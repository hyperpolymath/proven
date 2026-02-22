// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe OCI image reference parsing via libproven FFI.
//!
//! Parses and validates OCI/Docker image references.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// Parsed OCI image reference.
#[derive(Debug, Clone)]
pub struct ImageReference {
    /// Registry hostname (e.g., "ghcr.io").
    pub registry: Option<String>,
    /// Repository path (e.g., "user/repo").
    pub repository: String,
    /// Tag (e.g., "v1.0").
    pub tag: Option<String>,
    /// Digest (e.g., "sha256:abc123...").
    pub digest: Option<String>,
}

impl std::fmt::Display for ImageReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref registry) = self.registry {
            write!(f, "{}/", registry)?;
        }
        write!(f, "{}", self.repository)?;
        if let Some(ref tag) = self.tag {
            write!(f, ":{}", tag)?;
        }
        if let Some(ref digest) = self.digest {
            write!(f, "@{}", digest)?;
        }
        Ok(())
    }
}

/// Safe OCI registry operations.
pub struct SafeRegistry;

impl SafeRegistry {
    /// Parse an OCI image reference string.
    ///
    /// Supports formats like:
    /// - `nginx` (Docker Hub library image)
    /// - `user/repo:tag`
    /// - `ghcr.io/user/repo:v1.0`
    /// - `ghcr.io/user/repo@sha256:abc123...`
    pub fn parse(input: &str) -> Result<ImageReference> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function allocates strings for each component.
        let result = unsafe {
            ffi::proven_registry_parse(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;

        // SAFETY: The FFI call succeeded. All string pointers in the result
        // are either null or valid allocations from libproven.
        let image_ref = unsafe {
            let registry = if result.reference.registry.is_null()
                || result.reference.registry_len == 0
            {
                None
            } else {
                let slice = std::slice::from_raw_parts(
                    result.reference.registry,
                    result.reference.registry_len,
                );
                Some(String::from_utf8_lossy(slice).into_owned())
            };

            let repository = if result.reference.repository.is_null() {
                String::new()
            } else {
                let slice = std::slice::from_raw_parts(
                    result.reference.repository,
                    result.reference.repository_len,
                );
                String::from_utf8_lossy(slice).into_owned()
            };

            let tag = if result.reference.tag.is_null()
                || result.reference.tag_len == 0
            {
                None
            } else {
                let slice = std::slice::from_raw_parts(
                    result.reference.tag,
                    result.reference.tag_len,
                );
                Some(String::from_utf8_lossy(slice).into_owned())
            };

            let digest = if result.reference.digest.is_null()
                || result.reference.digest_len == 0
            {
                None
            } else {
                let slice = std::slice::from_raw_parts(
                    result.reference.digest,
                    result.reference.digest_len,
                );
                Some(String::from_utf8_lossy(slice).into_owned())
            };

            // Note: The FFI does not provide a separate free function for
            // ImageRefResult. Strings are freed individually via proven_free_string.
            if !result.reference.registry.is_null() {
                ffi::proven_free_string(result.reference.registry);
            }
            if !result.reference.repository.is_null() {
                ffi::proven_free_string(result.reference.repository);
            }
            if !result.reference.tag.is_null() {
                ffi::proven_free_string(result.reference.tag);
            }
            if !result.reference.digest.is_null() {
                ffi::proven_free_string(result.reference.digest);
            }

            ImageReference {
                registry,
                repository,
                tag,
                digest,
            }
        };

        Ok(image_ref)
    }
}
