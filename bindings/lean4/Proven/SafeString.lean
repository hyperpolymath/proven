/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafeString - Safe text operations

Provides encoding-safe string operations: UTF-8 validation, SQL/HTML/JS
escaping. Every operation delegates to the formally verified Idris 2 core
via the Zig FFI bridge (`libproven`). No string logic is reimplemented
in Lean.

## Memory management

String results from libproven are C-allocated. The wrappers in this module
automatically marshal them into Lean `String` values and free the C memory,
so callers need not manage memory manually.
-/

import Proven.FFI

namespace Proven.SafeString

open Proven.FFI

/-- Error type for string operations. -/
structure StringError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString StringError := ⟨fun e => s!"StringError: {e.status}"⟩

/-- Alias for string operation results. -/
abbrev StringResult (a : Type) := Except StringError a

-- Internal helper: convert a StringResultRaw to Except StringError String
private def unwrapStringResult (raw : StringResultRaw) : IO (StringResult String) := do
  let s := ProvenStatus.ofInt32 raw.status
  match s with
  | .ok => do
    let maybeStr <- marshalStringResult raw
    match maybeStr with
    | some str => return .ok str
    | none     => return .error { status := .errAllocationFailed }
  | _ => return .error { status := s }

-- Internal helper: convert a BoolResult to Except StringError Bool
private def unwrapBoolResult (r : BoolResult) : StringResult Bool :=
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => .ok r.value
  | _   => .error { status := s }

-- ============================================================================
-- UTF-8 validation
-- ============================================================================

/-- Check if a `ByteArray` contains valid UTF-8 data.
    Delegates to `proven_string_is_valid_utf8`. -/
def isValidUtf8 (data : ByteArray) : IO (StringResult Bool) := do
  let r <- provenStringIsValidUtf8 data
  return unwrapBoolResult r

-- ============================================================================
-- String escaping
-- ============================================================================

/-- Escape a string for safe inclusion in SQL queries (single-quote escaping).
    Prefer parameterized queries over string escaping when possible.
    Delegates to `proven_string_escape_sql`. -/
def escapeSql (input : String) : IO (StringResult String) := do
  let raw <- provenStringEscapeSql input.toUTF8
  unwrapStringResult raw

/-- Escape a string for safe inclusion in HTML (prevents XSS).
    Delegates to `proven_string_escape_html`. -/
def escapeHtml (input : String) : IO (StringResult String) := do
  let raw <- provenStringEscapeHtml input.toUTF8
  unwrapStringResult raw

/-- Escape a string for safe inclusion in JavaScript string literals.
    Delegates to `proven_string_escape_js`. -/
def escapeJs (input : String) : IO (StringResult String) := do
  let raw <- provenStringEscapeJs input.toUTF8
  unwrapStringResult raw

-- ============================================================================
-- Convenience: Option-returning variants
-- ============================================================================

/-- UTF-8 validation returning `Option`. -/
def isValidUtf8? (data : ByteArray) : IO (Option Bool) := do
  let r <- isValidUtf8 data
  return r.toOption

/-- SQL escaping returning `Option`. -/
def escapeSql? (input : String) : IO (Option String) := do
  let r <- escapeSql input
  return r.toOption

/-- HTML escaping returning `Option`. -/
def escapeHtml? (input : String) : IO (Option String) := do
  let r <- escapeHtml input
  return r.toOption

/-- JS escaping returning `Option`. -/
def escapeJs? (input : String) : IO (Option String) := do
  let r <- escapeJs input
  return r.toOption

end Proven.SafeString
