// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeJson - JSON validation and type detection via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This class is a thin wrapper; it does NOT reimplement any logic.

// JSON type constants matching ProvenJsonType enum.
primitive JsonNull    fun apply(): I32 =>  0
primitive JsonBool    fun apply(): I32 =>  1
primitive JsonNumber  fun apply(): I32 =>  2
primitive JsonString  fun apply(): I32 =>  3
primitive JsonArray   fun apply(): I32 =>  4
primitive JsonObject  fun apply(): I32 =>  5
primitive JsonInvalid fun apply(): I32 => -1

primitive SafeJson
  """
  Formally verified JSON operations.

  Example:
  ```pony
  match SafeJson.is_valid("{\"key\": \"value\"}")
  | let v: Bool => env.out.print("Valid: " + v.string())
  | None => env.out.print("Error")
  end
  ```
  """

  fun is_valid(s: String): (Bool | None) =>
    """Check if string is valid JSON."""
    let r = _LibProven.json_is_valid(s.cpointer(), s.size())
    if r.status == ProvenOk() then r.value else None end

  fun get_type(s: String): I32 =>
    """
    Get JSON value type at root level.
    Returns type code (use JsonNull(), JsonObject(), etc. to compare).
    Returns JsonInvalid() (-1) for invalid JSON.
    """
    _LibProven.json_get_type(s.cpointer(), s.size())

  fun type_name(type_code: I32): String =>
    """
    Convert JSON type code to a human-readable name.
    This is a local convenience function; no FFI call is made.
    """
    if type_code == JsonNull()   then "null"
    elseif type_code == JsonBool()   then "boolean"
    elseif type_code == JsonNumber() then "number"
    elseif type_code == JsonString() then "string"
    elseif type_code == JsonArray()  then "array"
    elseif type_code == JsonObject() then "object"
    else "invalid"
    end
