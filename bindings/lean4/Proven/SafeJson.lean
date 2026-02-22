/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafeJson - JSON validation and type detection

Provides safe JSON validation and root-level type detection. Every operation
delegates to the formally verified Idris 2 core via the Zig FFI bridge
(`libproven`). No JSON parsing logic is reimplemented in Lean.
-/

import Proven.FFI

namespace Proven.SafeJson

open Proven.FFI

/-- Error type for JSON operations. -/
structure JsonError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString JsonError := ⟨fun e => s!"JsonError: {e.status}"⟩

/-- Alias for JSON operation results. -/
abbrev JsonResult (a : Type) := Except JsonError a

-- Internal helper
private def unwrapBoolResult (r : BoolResult) : JsonResult Bool :=
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => .ok r.value
  | _   => .error { status := s }

-- ============================================================================
-- JSON validation
-- ============================================================================

/-- Check if a string is valid JSON.
    Delegates to `proven_json_is_valid`. -/
def isValid (json : String) : IO (JsonResult Bool) := do
  let r <- provenJsonIsValid json.toUTF8
  return unwrapBoolResult r

/-- Check if a string is valid JSON, returning `Option Bool`. -/
def isValid? (json : String) : IO (Option Bool) := do
  let r <- isValid json
  return r.toOption

/-- Check if a string is valid JSON, returning a simple `Bool`.
    Returns `false` on internal error. -/
def validate (json : String) : IO Bool := do
  let r <- isValid? json
  return r.getD false

-- ============================================================================
-- JSON type detection
-- ============================================================================

/-- Get the root-level JSON value type.
    Returns `JsonType.invalid` if the input is not valid JSON.
    Delegates to `proven_json_get_type`. -/
def getType (json : String) : IO JsonType := do
  let raw <- provenJsonGetType json.toUTF8
  return JsonType.ofInt32 raw

/-- Check if the root JSON value is a null. -/
def isNull (json : String) : IO Bool := do
  let t <- getType json
  return t == .null

/-- Check if the root JSON value is a boolean. -/
def isBool (json : String) : IO Bool := do
  let t <- getType json
  return t == .bool

/-- Check if the root JSON value is a number. -/
def isNumber (json : String) : IO Bool := do
  let t <- getType json
  return t == .number

/-- Check if the root JSON value is a string. -/
def isString (json : String) : IO Bool := do
  let t <- getType json
  return t == .string

/-- Check if the root JSON value is an array. -/
def isArray (json : String) : IO Bool := do
  let t <- getType json
  return t == .array

/-- Check if the root JSON value is an object. -/
def isObject (json : String) : IO Bool := do
  let t <- getType json
  return t == .object

-- ============================================================================
-- JSON validation from ByteArray
-- ============================================================================

/-- Check if raw bytes are valid JSON.
    Delegates to `proven_json_is_valid`. -/
def isValidBytes (data : ByteArray) : IO (JsonResult Bool) := do
  let r <- provenJsonIsValid data
  return unwrapBoolResult r

end Proven.SafeJson
