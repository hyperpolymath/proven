// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeVersion - Semantic versioning that cannot crash.
////
//// Thin FFI wrapper over libproven proven_version_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

import gleam/option.{type Option}

/// A parsed semantic version.
pub type SemanticVersion {
  SemanticVersion(
    major: Int,
    minor: Int,
    patch: Int,
    prerelease: Option(String),
  )
}

/// Parse a semantic version string (e.g., "1.2.3-alpha").
/// Supports optional 'v' or 'V' prefix.
@external(erlang, "proven_nif", "version_parse")
pub fn parse(input: String) -> Result(SemanticVersion, String)

/// Compare two semantic versions.
/// Returns negative if a < b, 0 if equal, positive if a > b.
@external(erlang, "proven_nif", "version_compare")
pub fn compare(
  major_a: Int,
  minor_a: Int,
  patch_a: Int,
  major_b: Int,
  minor_b: Int,
  patch_b: Int,
) -> Int
