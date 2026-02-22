// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeJson.pike - JSON validation for Pike.
//
// All operations delegate to libproven via LibProven. Returns UNDEFINED
// on error.
//
// Usage:
//   import Proven;
//   LibProven.init();
//   if (SafeJson.is_valid("{\"key\": 42}"))
//       write("Valid JSON\n");
//   write("Type: %s\n", SafeJson.get_type("[1,2,3]"));  // "array"
//   LibProven.deinit();

//! @class SafeJson
//! JSON validation and type detection.
//!
//! Validates JSON strings and detects their root-level type using
//! the formally verified Idris 2 implementation.

protected LibProven lib = LibProven();

//! @decl int(0..1)|zero is_valid(string json_str)
//! Check if a string is valid JSON.
//! @returns
//!   @expr{1@} if valid JSON, @expr{0@} if invalid,
//!   @expr{UNDEFINED@} on error.
int(0..1)|zero is_valid(string json_str)
{
    return lib->call_bool("json_is_valid", ({json_str}));
}

//! @decl string|zero get_type(string json_str)
//! Get the root-level JSON value type.
//! @returns
//!   One of: @expr{"null"@}, @expr{"bool"@}, @expr{"number"@},
//!   @expr{"string"@}, @expr{"array"@}, @expr{"object"@},
//!   @expr{"invalid"@}. Returns @expr{UNDEFINED@} on error.
string|zero get_type(string json_str)
{
    return lib->call_string("json_get_type", ({json_str}));
}
