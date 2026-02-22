// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// SafeString -- Resource-typed wrappers around libproven string FFI.
///
/// All operations delegate to the formally verified Idris 2 implementation
/// via the Zig FFI layer. No string logic is reimplemented here.
/// FFI-allocated strings are freed after being copied into Eclexia strings.

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Convert an FFI StringResult into a Result[String, ProvenError].
/// Frees the FFI-allocated string pointer after copying.
fn string_result_to_result(result: StringResult) -> Result[String, ProvenError] {
    if result.status == STATUS_OK {
        let s = string_from_raw(result.ptr, result.len)
        proven_free_string(result.ptr)
        Ok(s)
    } else {
        Err(status_to_error(result.status))
    }
}

/// Convert an FFI BoolResult into a Result[Bool, ProvenError].
fn bool_result_to_result(result: BoolResult) -> Result[Bool, ProvenError] {
    if result.status == STATUS_OK {
        Ok(result.value != 0)
    } else {
        Err(status_to_error(result.status))
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Check if a byte sequence is valid UTF-8.
/// Delegates to proven_string_is_valid_utf8 via FFI.
fn is_valid_utf8(input: String) -> Result[Bool, ProvenError] {
    let (ptr, len) = string_to_raw(input)
    bool_result_to_result(proven_string_is_valid_utf8(ptr, len))
}

/// Escape a string for safe SQL interpolation.
/// Delegates to proven_string_escape_sql via FFI.
fn escape_sql(value: String) -> Result[String, ProvenError] {
    let (ptr, len) = string_to_raw(value)
    string_result_to_result(proven_string_escape_sql(ptr, len))
}

/// Escape a string for safe HTML insertion (prevents XSS).
/// Delegates to proven_string_escape_html via FFI.
fn escape_html(value: String) -> Result[String, ProvenError] {
    let (ptr, len) = string_to_raw(value)
    string_result_to_result(proven_string_escape_html(ptr, len))
}

/// Escape a string for safe JavaScript string literal insertion.
/// Delegates to proven_string_escape_js via FFI.
fn escape_js(value: String) -> Result[String, ProvenError] {
    let (ptr, len) = string_to_raw(value)
    string_result_to_result(proven_string_escape_js(ptr, len))
}
