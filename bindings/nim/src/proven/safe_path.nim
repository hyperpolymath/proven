# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe filesystem path operations with traversal attack prevention.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

proc hasTraversal*(path: string): bool =
  ## Check if a path contains directory traversal sequences.
  if path.len == 0:
    return false
  let res = provenPathHasTraversal(unsafeAddr path[0], csize_t(path.len))
  if res.status == PROVEN_OK:
    return res.value
  false

proc isSafe*(path: string): bool =
  ## Check if a path is safe (no traversal attacks).
  not hasTraversal(path)

proc sanitizeFilename*(filename: string): Option[string] =
  ## Sanitize a filename by removing dangerous characters.
  if filename.len == 0:
    return some("")
  let res = provenPathSanitizeFilename(unsafeAddr filename[0], csize_t(filename.len))
  if res.status == PROVEN_OK and res.value != nil:
    let sanitized = $res.value
    provenFreeString(res.value)
    return some(sanitized)
  none(string)

proc safeJoin*(base: string, parts: varargs[string]): Option[string] =
  ## Safely join path components, rejecting traversal attempts.
  ## Delegates traversal checking to libproven, then joins paths.
  for part in parts:
    if hasTraversal(part):
      return none(string)

  var joined = base
  for part in parts:
    let sanitized = sanitizeFilename(part)
    if sanitized.isNone:
      return none(string)
    if joined.len > 0 and joined[^1] != '/':
      joined.add('/')
    joined.add(sanitized.get)

  some(joined)
