// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeJson - JSON validation and type detection via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.

module SafeJson {

  public use LibProven;

  /**
   * Check if string is valid JSON.
   *
   * :arg s: Input string to validate.
   * :returns: ``none`` on error, true if valid JSON, false otherwise.
   */
  proc isValid(s: string): bool? {
    var (ptr, len) = toCBytes(s);
    var r = provenJsonIsValid(ptr, len);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Get JSON value type at root level.
   *
   * :arg s: Input JSON string.
   * :returns: JSON type code (see JSON_* constants in LibProven).
   *           Returns JSON_INVALID (-1) for invalid JSON.
   */
  proc getType(s: string): int(32) {
    var (ptr, len) = toCBytes(s);
    return provenJsonGetType(ptr, len);
  }

  /**
   * Convert JSON type code to a human-readable name.
   *
   * This is a local convenience function; no FFI call is made.
   *
   * :arg typeCode: Integer type code from ``getType()``.
   * :returns: Type name string.
   */
  proc typeName(typeCode: int(32)): string {
    select typeCode {
      when JSON_NULL    do return "null";
      when JSON_BOOL    do return "boolean";
      when JSON_NUMBER  do return "number";
      when JSON_STRING  do return "string";
      when JSON_ARRAY   do return "array";
      when JSON_OBJECT  do return "object";
      otherwise         do return "invalid";
    }
  }

}
