// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeString.pike - Encoding-safe text operations for Pike.
//
// All operations delegate to libproven via LibProven. Returns UNDEFINED
// on error.
//
// Usage:
//   import Proven;
//   LibProven.init();
//   string safe = SafeString.escape_html("<script>alert(1)</script>");
//   write("Safe: %s\n", safe);
//   LibProven.deinit();

//! @class SafeString
//! Safe text encoding and escaping operations.
//!
//! Provides XSS-safe HTML escaping, SQL injection prevention, and
//! JavaScript string literal escaping. All backed by verified Idris 2 code.

protected LibProven lib = LibProven();

//! @decl int(0..1)|zero is_valid_utf8(string str)
//! Check if a string contains valid UTF-8 bytes.
//! @returns
//!   @expr{1@} if valid, @expr{0@} if invalid, @expr{UNDEFINED@} on error.
int(0..1)|zero is_valid_utf8(string str)
{
    return lib->call_bool("string_is_valid_utf8", ({str}));
}

//! @decl string|zero escape_sql(string str)
//! Escape a string for safe inclusion in SQL queries.
//! Prefer parameterized queries over string escaping.
//! @returns
//!   Escaped string, or @expr{UNDEFINED@} on error.
string|zero escape_sql(string str)
{
    return lib->call_string("string_escape_sql", ({str}));
}

//! @decl string|zero escape_html(string str)
//! Escape a string for safe inclusion in HTML content.
//! Prevents XSS attacks by escaping @expr{<@}, @expr{>@}, @expr{&@},
//! @expr{"@}, @expr{'@}.
//! @returns
//!   HTML-safe string, or @expr{UNDEFINED@} on error.
string|zero escape_html(string str)
{
    return lib->call_string("string_escape_html", ({str}));
}

//! @decl string|zero escape_js(string str)
//! Escape a string for safe inclusion in JavaScript string literals.
//! @returns
//!   JS-safe string, or @expr{UNDEFINED@} on error.
string|zero escape_js(string str)
{
    return lib->call_string("string_escape_js", ({str}));
}
