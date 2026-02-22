/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafeUrl - URL parsing and validation

Provides safe URL parsing into components (scheme, host, port, path, query,
fragment). The parsing logic is implemented in formally verified Idris 2 code
and accessed via the Zig FFI bridge (`libproven`). No URL parsing logic is
reimplemented in Lean.

## Memory management

The C library allocates strings for each URL component. This module
automatically copies them into Lean `String` values and frees the C memory,
so callers need not manage memory manually.
-/

import Proven.FFI

namespace Proven.SafeUrl

open Proven.FFI

/-- Parsed URL components with all fields as Lean `String` values. -/
structure UrlComponents where
  scheme   : String
  host     : String
  port     : Option UInt16
  path     : String
  query    : String
  fragment : String
  deriving Repr, BEq, Inhabited

/-- Error type for URL operations. -/
structure UrlError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString UrlError := ⟨fun e => s!"UrlError: {e.status}"⟩

/-- Alias for URL operation results. -/
abbrev UrlResult (a : Type) := Except UrlError a

-- ============================================================================
-- URL parsing
-- ============================================================================

/-- Parse a URL string into its components.
    Returns `UrlComponents` with scheme, host, port, path, query, and fragment.
    Delegates to `proven_url_parse`. The C-allocated component strings are
    automatically copied into Lean strings and freed. -/
def parse (url : String) : IO (UrlResult UrlComponents) := do
  let raw <- provenUrlParse url.toUTF8
  let s := ProvenStatus.ofInt32 raw.status
  match s with
  | .ok => do
    -- Marshal each component string from C memory into Lean.
    -- The marshalStringResult helper copies and frees individual fields.
    -- For URL components we use the raw structure and free as a group.
    let comps := raw.components
    -- Build a temporary StringResultRaw for each field to marshal it.
    let schemeRaw : StringResultRaw := { status := 0, ptr := comps.schemePtr, len := comps.schemeLen }
    let hostRaw   : StringResultRaw := { status := 0, ptr := comps.hostPtr,   len := comps.hostLen }
    let pathRaw   : StringResultRaw := { status := 0, ptr := comps.pathPtr,   len := comps.pathLen }
    let queryRaw  : StringResultRaw := { status := 0, ptr := comps.queryPtr,  len := comps.queryLen }
    let fragRaw   : StringResultRaw := { status := 0, ptr := comps.fragmentPtr, len := comps.fragmentLen }
    -- Note: marshalStringResult copies bytes and does NOT free (we free via provenUrlFree)
    -- We use a separate read-only marshal here. For safety, read bytes then free group.
    let scheme <- marshalStringResult schemeRaw
    let host   <- marshalStringResult hostRaw
    let path   <- marshalStringResult pathRaw
    let query  <- marshalStringResult queryRaw
    let frag   <- marshalStringResult fragRaw
    -- Free the entire URL component block at once
    provenUrlFree comps
    let port := if comps.hasPort then some comps.port else none
    return .ok {
      scheme   := scheme.getD ""
      host     := host.getD ""
      port     := port
      path     := path.getD ""
      query    := query.getD ""
      fragment := frag.getD ""
    }
  | _ => return .error { status := s }

/-- Parse a URL, returning `Option UrlComponents`. -/
def parse? (url : String) : IO (Option UrlComponents) := do
  let r <- parse url
  return r.toOption

-- ============================================================================
-- Component accessors (convenience)
-- ============================================================================

/-- Extract just the scheme from a URL (e.g., "https"). -/
def getScheme (url : String) : IO (Option String) := do
  let r <- parse? url
  return r.map (·.scheme)

/-- Extract just the host from a URL. -/
def getHost (url : String) : IO (Option String) := do
  let r <- parse? url
  return r.map (·.host)

/-- Extract just the port from a URL. -/
def getPort (url : String) : IO (Option UInt16) := do
  let r <- parse? url
  return r.bind (·.port)

/-- Extract just the path from a URL. -/
def getPath (url : String) : IO (Option String) := do
  let r <- parse? url
  return r.map (·.path)

end Proven.SafeUrl
