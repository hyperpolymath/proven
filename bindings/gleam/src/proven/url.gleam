// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeUrl - URL parsing and validation that cannot crash.
////
//// Thin FFI wrapper over libproven proven_url_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

import gleam/option.{type Option}

/// URL components after parsing.
pub type UrlComponents {
  UrlComponents(
    scheme: Option(String),
    host: Option(String),
    port: Option(Int),
    path: String,
    query: Option(String),
    fragment: Option(String),
  )
}

/// Parse a URL string into components.
@external(erlang, "proven_nif", "url_parse")
pub fn parse(url: String) -> Result(UrlComponents, String)
