// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeJson - JSON validation that cannot crash.
////
//// Thin FFI wrapper over libproven proven_json_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// JSON value type (matches ProvenJsonType from C header).
pub type JsonType {
  JsonNull
  JsonBool
  JsonNumber
  JsonString
  JsonArray
  JsonObject
  JsonInvalid
}

/// Check if a string is valid JSON.
@external(erlang, "proven_nif", "json_is_valid")
pub fn is_valid(input: String) -> Result(Bool, String)

/// Get the JSON value type at root level.
@external(erlang, "proven_nif", "json_get_type")
pub fn get_type(input: String) -> Result(JsonType, String)
