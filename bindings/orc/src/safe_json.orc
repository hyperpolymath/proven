{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-
  safe_json.orc - JSON validation for Orc via libproven

  All JSON validation is performed by libproven's formally verified
  Idris 2 core. This module provides Orc-idiomatic wrappers for
  JSON validation and type detection.
-}

include "src/ffi.orc"
include "src/proven.orc"
include "src/safe_string.orc"

-- ============================================================================
-- JSON Type Constants (matching libproven C ABI)
-- ============================================================================

val JSON_TYPE_NULL = 0
val JSON_TYPE_BOOL = 1
val JSON_TYPE_NUMBER = 2
val JSON_TYPE_STRING = 3
val JSON_TYPE_ARRAY = 4
val JSON_TYPE_OBJECT = 5
val JSON_TYPE_INVALID = -1

-- ============================================================================
-- JSON Validation
-- ============================================================================

-- Validate a JSON document string.
-- Publishes true if valid JSON, false otherwise, or halts on FFI error.
-- Delegates to proven_json_is_valid via FFI.
def is_valid_json(json_str) =
  with_string_ptr(json_str, lambda(ptr, len) =
    val result = ffi_json_is_valid(ptr, len)
    extract_bool(result)
  )

-- ============================================================================
-- JSON Type Detection
-- ============================================================================

-- Get the top-level JSON type of a document.
-- Publishes one of the JSON_TYPE_* constants.
-- Delegates to proven_json_get_type via FFI.
def json_type(json_str) =
  with_string_ptr(json_str, lambda(ptr, len) =
    ffi_json_get_type(ptr, len)
  )

-- Get the top-level JSON type as a human-readable string.
-- Publishes "null", "bool", "number", "string", "array", "object", or "invalid".
def json_type_name(json_str) =
  val t = json_type(json_str)
  if t = JSON_TYPE_NULL then "null"
  else if t = JSON_TYPE_BOOL then "bool"
  else if t = JSON_TYPE_NUMBER then "number"
  else if t = JSON_TYPE_STRING then "string"
  else if t = JSON_TYPE_ARRAY then "array"
  else if t = JSON_TYPE_OBJECT then "object"
  else "invalid"

-- ============================================================================
-- Concurrent JSON Validation
-- ============================================================================

-- Validate a list of JSON documents concurrently.
-- Publishes (index, json, valid) tuples for each document.
def validate_json_batch(docs) =
  each(docs) >doc>
  val valid = is_valid_json(doc)
  (doc, valid)

-- Filter a list to only valid JSON documents.
-- Publishes only documents that pass validation.
def filter_valid_json(docs) =
  each(docs) >doc>
  val valid = is_valid_json(doc)
  if valid then doc
  else stop

-- Check if a JSON document is a specific type.
-- Publishes true if the document's top-level type matches.
def is_json_object(json_str) = json_type(json_str) = JSON_TYPE_OBJECT
def is_json_array(json_str) = json_type(json_str) = JSON_TYPE_ARRAY
def is_json_string(json_str) = json_type(json_str) = JSON_TYPE_STRING
def is_json_number(json_str) = json_type(json_str) = JSON_TYPE_NUMBER
