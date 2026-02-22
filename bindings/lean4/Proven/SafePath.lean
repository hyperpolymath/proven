/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafePath - Filesystem traversal prevention

Provides safe filesystem path operations: directory traversal detection and
filename sanitization. Every operation delegates to the formally verified
Idris 2 core via the Zig FFI bridge (`libproven`). No path logic is
reimplemented in Lean.
-/

import Proven.FFI

namespace Proven.SafePath

open Proven.FFI

/-- Error type for path operations. -/
structure PathError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString PathError := ⟨fun e => s!"PathError: {e.status}"⟩

/-- Alias for path operation results. -/
abbrev PathResult (a : Type) := Except PathError a

-- Internal helpers
private def unwrapBoolResult (r : BoolResult) : PathResult Bool :=
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => .ok r.value
  | _   => .error { status := s }

private def unwrapStringResult (raw : StringResultRaw) : IO (PathResult String) := do
  let s := ProvenStatus.ofInt32 raw.status
  match s with
  | .ok => do
    let maybeStr <- marshalStringResult raw
    match maybeStr with
    | some str => return .ok str
    | none     => return .error { status := .errAllocationFailed }
  | _ => return .error { status := s }

-- ============================================================================
-- Traversal detection
-- ============================================================================

/-- Check if a path contains directory traversal sequences (`..`).
    Returns `true` if traversal is detected (i.e., the path is dangerous).
    Delegates to `proven_path_has_traversal`. -/
def hasTraversal (path : String) : IO (PathResult Bool) := do
  let r <- provenPathHasTraversal path.toUTF8
  return unwrapBoolResult r

/-- Check if a path is safe (does not contain traversal sequences).
    Convenience wrapper that negates `hasTraversal`. -/
def isSafe (path : String) : IO (PathResult Bool) := do
  let r <- hasTraversal path
  return r.map (! ·)

-- ============================================================================
-- Filename sanitization
-- ============================================================================

/-- Sanitize a filename by removing dangerous characters.
    Strips path separators, null bytes, and other dangerous characters.
    Delegates to `proven_path_sanitize_filename`. -/
def sanitizeFilename (filename : String) : IO (PathResult String) := do
  let raw <- provenPathSanitizeFilename filename.toUTF8
  unwrapStringResult raw

-- ============================================================================
-- Convenience: Option-returning variants
-- ============================================================================

/-- Traversal detection returning `Option`. -/
def hasTraversal? (path : String) : IO (Option Bool) := do
  let r <- hasTraversal path
  return r.toOption

/-- Path safety check returning `Option`. -/
def isSafe? (path : String) : IO (Option Bool) := do
  let r <- isSafe path
  return r.toOption

/-- Filename sanitization returning `Option`. -/
def sanitizeFilename? (filename : String) : IO (Option String) := do
  let r <- sanitizeFilename filename
  return r.toOption

end Proven.SafePath
