// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_url.carbon - Safe URL parsing and validation via libproven FFI.
//
// Parses URLs into components following RFC 3986.
// All operations delegate to Idris 2 verified code.
// No URL parsing logic is implemented in Carbon.

package Proven api;

import Proven library "lib_proven";

// Safe URL operations.
//
// All methods call the formally verified Idris 2 implementation via FFI.
class SafeUrl {

  // Parse a URL string into its components.
  //
  // Returns an opaque pointer to a ProvenUrlResult structure allocated by
  // libproven. The caller MUST free it using FreeUrlResult() when done.
  // Returns a null pointer on failure.
  //
  // To access individual components, use the C-level struct fields:
  //   - scheme, scheme_len
  //   - host, host_len
  //   - port, has_port
  //   - path, path_len
  //   - query, query_len
  //   - fragment, fragment_len
  fn Parse(data: Cpp.Ptr(u8), len: i64) -> Cpp.Ptr(()) {
    return proven_url_parse(data, len);
  }

  // Free a URL result previously allocated by Parse().
  //
  // Safe to call with a null pointer.
  fn FreeUrlResult(components: Cpp.Ptr(())) -> () {
    proven_url_free(components);
  }
}
