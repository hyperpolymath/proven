// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUrl - URL encoding/decoding via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This class is a thin wrapper; it does NOT reimplement any logic.

primitive SafeUrl
  """
  Formally verified URL operations.

  Example:
  ```pony
  match SafeUrl.encode("hello world")
  | let s: String => env.out.print(s)  // "hello%20world"
  | None => env.out.print("Error")
  end
  ```
  """

  fun encode(s: String): (String | None) =>
    """
    URL-encode a string (RFC 3986 percent encoding).
    Unreserved characters (A-Za-z0-9-._~) pass through.
    """
    let r = _LibProven.http_url_encode(s.cpointer(), s.size())
    _LibProven._extract_string(r)

  fun decode(s: String): (String | None) =>
    """URL-decode a percent-encoded string."""
    let r = _LibProven.http_url_decode(s.cpointer(), s.size())
    _LibProven._extract_string(r)
