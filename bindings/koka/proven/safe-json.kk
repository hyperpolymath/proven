// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe-json.kk -- Safe JSON validation via libproven FFI.
//
// Validates JSON strings and determines root value types without full parsing.
// All computation is performed by the Idris 2 verified implementation.
// No JSON parsing logic is reimplemented in Koka.
module proven/safe-json

import std/num/int64
import proven/lib-proven

// ============================================================================
// JSON type enum
// ============================================================================

// JSON value types returned by get-type.
pub type json-type
  JsonNull
  JsonBool
  JsonNumber
  JsonString
  JsonArray
  JsonObject
  JsonInvalid

// Convert a raw integer to a json-type.
fun json-type-from-int32(code: int32): json-type
  match code
    0i32  -> JsonNull
    1i32  -> JsonBool
    2i32  -> JsonNumber
    3i32  -> JsonString
    4i32  -> JsonArray
    5i32  -> JsonObject
    _     -> JsonInvalid

// Show a json-type as a human-readable string.
pub fun show(jt: json-type): string
  match jt
    JsonNull    -> "null"
    JsonBool    -> "boolean"
    JsonNumber  -> "number"
    JsonString  -> "string"
    JsonArray   -> "array"
    JsonObject  -> "object"
    JsonInvalid -> "invalid"

// ============================================================================
// Raw FFI declarations
// ============================================================================

// Check if string is valid JSON.
extern raw-is-valid(ptr: intptr_t, len: int64): bool-result
  c inline "{ ProvenBoolResult r = proven_json_is_valid((const uint8_t*)#1, (size_t)#2); return kk_proven_bool_result(r.status, r.value ? 1 : 0, kk_context()); }"

// Get JSON value type at root level.
extern raw-get-type(ptr: intptr_t, len: int64): int32
  c inline "(int32_t)proven_json_get_type((const uint8_t*)#1, (size_t)#2)"

// ============================================================================
// Safe public API
// ============================================================================

// Check if a string is valid JSON.
// Returns Just(true) if valid, Just(false) if not, Nothing on error.
pub fun is-valid-json(input: string): <ndet> maybe<bool>
  val r = raw-is-valid(input.cptr, input.count.int64)
  bool-result-to-maybe(r)

// Get the JSON value type at the root level.
// Returns JsonInvalid if the input is not valid JSON.
pub fun get-json-type(input: string): <ndet> json-type
  val code = raw-get-type(input.cptr, input.count.int64)
  json-type-from-int32(code)

// Convenience: check if the input is a JSON object.
pub fun is-json-object(input: string): <ndet> bool
  match get-json-type(input)
    JsonObject -> True
    _          -> False

// Convenience: check if the input is a JSON array.
pub fun is-json-array(input: string): <ndet> bool
  match get-json-type(input)
    JsonArray -> True
    _         -> False
