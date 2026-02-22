// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_json.carbon - Safe JSON validation via libproven FFI.
//
// Validates JSON strings and determines root value types without full parsing.
// All operations delegate to Idris 2 verified code.
// No JSON parsing logic is implemented in Carbon.

package Proven api;

import Proven library "lib_proven";

// Safe JSON operations.
//
// All methods call the formally verified Idris 2 implementation via FFI.
class SafeJson {

  // Check if a string is valid JSON.
  //
  // Returns Optional(bool): true if the input is syntactically valid JSON.
  // None indicates the FFI call itself failed.
  fn IsValid(data: Cpp.Ptr(u8), len: i64) -> Optional(bool) {
    var result: BoolResult = proven_json_is_valid(data, len);
    if (Succeeded(result.status)) {
      return Optional(bool).Create(result.value != 0);
    }
    return Optional(bool).None();
  }

  // Get the JSON value type at the root level.
  //
  // Returns one of the JSON_* constants:
  //   JSON_NULL (0), JSON_BOOL (1), JSON_NUMBER (2), JSON_STRING (3),
  //   JSON_ARRAY (4), JSON_OBJECT (5), JSON_INVALID (-1).
  fn GetType(data: Cpp.Ptr(u8), len: i64) -> i32 {
    return proven_json_get_type(data, len);
  }
}
