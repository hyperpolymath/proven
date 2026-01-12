# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe filesystem path operations with traversal attack prevention.

import std/[strutils, os, options]

proc hasTraversal*(path: string): bool =
  ## Check if a path contains directory traversal sequences.
  result = ".." in path or "~" in path

proc isSafe*(path: string): bool =
  ## Check if a path is safe (no traversal attacks).
  result = not hasTraversal(path)

proc sanitizeFilename*(filename: string): string =
  ## Sanitize a filename by removing dangerous characters.
  result = filename
    .replace("..", "_")
    .multiReplace([
      ("/", "_"),
      ("\\", "_"),
      ("<", "_"),
      (">", "_"),
      (":", "_"),
      ("\"", "_"),
      ("|", "_"),
      ("?", "_"),
      ("*", "_"),
      ("\0", "_")
    ])

proc safeJoin*(base: string, parts: varargs[string]): Option[string] =
  ## Safely join path components, rejecting traversal attempts.
  for part in parts:
    if hasTraversal(part):
      return none(string)

  var result = base
  for part in parts:
    let sanitized = sanitizeFilename(part)
    result = result / sanitized

  return some(result)
