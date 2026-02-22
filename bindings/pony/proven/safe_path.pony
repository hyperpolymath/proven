// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafePath - Filesystem traversal prevention via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This class is a thin wrapper; it does NOT reimplement any logic.

primitive SafePath
  """
  Formally verified path safety operations.

  Example:
  ```pony
  match SafePath.has_traversal("../../etc/passwd")
  | let v: Bool => if v then env.out.print("Traversal detected!") end
  | None => env.out.print("Error")
  end
  ```
  """

  fun has_traversal(path: String): (Bool | None) =>
    """
    Check if path contains directory traversal sequences ("..").
    Returns true if traversal detected, false if safe, None on error.
    """
    let r = _LibProven.path_has_traversal(path.cpointer(), path.size())
    if r.status == ProvenOk() then r.value else None end

  fun sanitize_filename(filename: String): (String | None) =>
    """Sanitize a filename by removing dangerous characters."""
    let r = _LibProven.path_sanitize_filename(
      filename.cpointer(), filename.size())
    _LibProven._extract_string(r)
