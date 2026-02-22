// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// SafeJson -- Resource-typed wrappers around libproven JSON validation FFI.
///
/// All operations delegate to the formally verified Idris 2 implementation
/// via the Zig FFI layer. No JSON logic is reimplemented here.
/// Provides safe JSON validation without parser panics.

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// JSON value type enumeration.
type JsonType = enum {
    Null,
    Bool,
    Number,
    JsonString,
    Array,
    Object,
    Invalid,
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Convert an FFI BoolResult into a Result[Bool, ProvenError].
fn bool_result_to_result(result: BoolResult) -> Result[Bool, ProvenError] {
    if result.status == STATUS_OK {
        Ok(result.value != 0)
    } else {
        Err(status_to_error(result.status))
    }
}

/// Convert an integer type code to a JsonType enum.
fn int_to_json_type(code: Int) -> JsonType {
    match code {
        0 => Null,
        1 => Bool,
        2 => Number,
        3 => JsonString,
        4 => Array,
        5 => Object,
        _ => Invalid,
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Check if a string is valid JSON.
/// Delegates to proven_json_is_valid via FFI.
fn is_valid(input: String) -> Result[Bool, ProvenError] {
    let (ptr, len) = string_to_raw(input)
    bool_result_to_result(proven_json_is_valid(ptr, len))
}

/// Get the JSON value type of the root element.
/// Delegates to proven_json_get_type via FFI.
fn get_type(input: String) -> JsonType {
    let (ptr, len) = string_to_raw(input)
    int_to_json_type(proven_json_get_type(ptr, len))
}

/// Validate JSON and return the input if valid.
fn validate(input: String) -> Result[String, ProvenError] {
    match is_valid(input) {
        Err(e) => Err(e),
        Ok(false) => Err(ProvenError {
            code: -7,
            message: "invalid JSON"
        }),
        Ok(true) => Ok(input)
    }
}

/// Check if a JSON string represents a JSON object at the root level.
fn is_object(input: String) -> Bool {
    match get_type(input) {
        Object => true,
        _ => false
    }
}

/// Check if a JSON string represents a JSON array at the root level.
fn is_array(input: String) -> Bool {
    match get_type(input) {
        Array => true,
        _ => false
    }
}
