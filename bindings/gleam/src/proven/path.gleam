// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafePath - Filesystem path operations that cannot crash.
////
//// Thin FFI wrapper over libproven proven_path_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Check if a path contains directory traversal sequences (..).
@external(erlang, "proven_nif", "path_has_traversal")
pub fn has_traversal(path: String) -> Result(Bool, String)

/// Sanitize a filename by removing dangerous characters.
/// Keeps only alphanumeric characters, dots, dashes, and underscores.
@external(erlang, "proven_nif", "path_sanitize_filename")
pub fn sanitize_filename(filename: String) -> Result(String, String)
