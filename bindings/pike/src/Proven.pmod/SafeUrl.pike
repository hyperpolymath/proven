// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUrl.pike - URL parsing and encoding for Pike.
//
// All operations delegate to libproven via LibProven. Returns UNDEFINED
// on error.
//
// Usage:
//   import Proven;
//   LibProven.init();
//   mapping parts = SafeUrl.parse("https://example.com:8080/path?q=1");
//   if (parts)
//       write("Host: %s, Port: %d\n", parts->host, parts->port);
//   LibProven.deinit();

//! @class SafeUrl
//! URL parsing, validation, and percent-encoding.
//!
//! Parses URLs into component parts, and provides RFC 3986
//! percent-encoding/decoding.

protected LibProven lib = LibProven();

//! @decl mapping|zero parse(string url_string)
//! Parse a URL into its components.
//! @param url_string
//!   URL to parse.
//! @returns
//!   A mapping with keys: @expr{"scheme"@}, @expr{"host"@},
//!   @expr{"port"@}, @expr{"path"@}, @expr{"query"@},
//!   @expr{"fragment"@}. Returns @expr{UNDEFINED@} on error.
mapping|zero parse(string url_string)
{
    mapping|zero result = lib->call("url_parse", ({url_string}));
    if (!result || result->status != 0) {
        return UNDEFINED;
    }
    return result->value;
}

//! @decl string|zero encode(string str)
//! URL-encode a string (RFC 3986 percent-encoding).
//! @returns
//!   Encoded string, or @expr{UNDEFINED@} on error.
string|zero encode(string str)
{
    return lib->call_string("http_url_encode", ({str}));
}

//! @decl string|zero decode(string str)
//! URL-decode a percent-encoded string.
//! @returns
//!   Decoded string, or @expr{UNDEFINED@} on error.
string|zero decode(string str)
{
    return lib->call_string("http_url_decode", ({str}));
}
