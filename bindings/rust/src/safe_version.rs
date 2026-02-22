// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe semantic versioning via libproven FFI.
//!
//! Parses and compares semantic version strings.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;
use std::cmp::Ordering;

/// Parsed semantic version.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    /// Major version number.
    pub major: u32,
    /// Minor version number.
    pub minor: u32,
    /// Patch version number.
    pub patch: u32,
    /// Pre-release label (e.g., "alpha.1").
    pub prerelease: Option<String>,
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;
        if let Some(ref pre) = self.prerelease {
            write!(f, "-{}", pre)?;
        }
        Ok(())
    }
}

/// Safe version operations.
pub struct SafeVersion;

impl SafeVersion {
    /// Parse a semantic version string (e.g., "1.2.3" or "1.0.0-alpha.1").
    pub fn parse(input: &str) -> Result<Version> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function allocates a prerelease string if present.
        let mut result = unsafe {
            ffi::proven_version_parse(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;

        // SAFETY: The FFI call succeeded. If prerelease is non-null, it points
        // to a valid string allocated by libproven.
        let prerelease = unsafe {
            if result.version.prerelease.is_null() || result.version.prerelease_len == 0 {
                None
            } else {
                let slice = std::slice::from_raw_parts(
                    result.version.prerelease,
                    result.version.prerelease_len,
                );
                Some(String::from_utf8_lossy(slice).into_owned())
            }
        };

        let version = Version {
            major: result.version.major,
            minor: result.version.minor,
            patch: result.version.patch,
            prerelease,
        };

        // SAFETY: We have extracted all data and can now free the FFI allocation.
        unsafe {
            ffi::proven_version_free(&mut result.version);
        }

        Ok(version)
    }

    /// Compare two semantic versions.
    ///
    /// Returns `Ordering::Less`, `Ordering::Equal`, or `Ordering::Greater`.
    pub fn compare(a: &Version, b: &Version) -> Ordering {
        let pre_a = a.prerelease.as_deref().unwrap_or("");
        let pre_b = b.prerelease.as_deref().unwrap_or("");

        let ffi_a = ffi::SemanticVersion {
            major: a.major,
            minor: a.minor,
            patch: a.patch,
            prerelease_len: pre_a.len(),
            prerelease: if pre_a.is_empty() {
                std::ptr::null_mut()
            } else {
                pre_a.as_ptr() as *mut u8
            },
        };

        let ffi_b = ffi::SemanticVersion {
            major: b.major,
            minor: b.minor,
            patch: b.patch,
            prerelease_len: pre_b.len(),
            prerelease: if pre_b.is_empty() {
                std::ptr::null_mut()
            } else {
                pre_b.as_ptr() as *mut u8
            },
        };

        // SAFETY: We pass value-type SemanticVersion structs. The prerelease
        // pointers are valid for the duration of this call (they point into
        // Rust string data that outlives the call).
        let cmp = unsafe { ffi::proven_version_compare(ffi_a, ffi_b) };

        match cmp {
            n if n < 0 => Ordering::Less,
            0 => Ordering::Equal,
            _ => Ordering::Greater,
        }
    }
}
