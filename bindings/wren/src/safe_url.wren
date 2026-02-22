// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUrl - URL parsing and validation via libproven FFI.
// All computation is performed in verified Idris 2 code via the Zig FFI
// layer. This Wren class is a thin wrapper; it does NOT reimplement any logic.

// Safe URL parsing. Returns parsed components as a list or null on error.
class SafeUrl {
    // Parse a URL into its components.
    // Returns [scheme, host, port, path, query, fragment] where port is
    // null if not specified. Returns null on parse failure.
    foreign static parse(s)
}
