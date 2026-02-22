// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe string operations via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeString =

    /// Check if a byte sequence is valid UTF-8.
    let isValidUtf8 (data: byte array) : bool option =
        FFI.proven_string_is_valid_utf8(data, unativeint data.Length) |> boolResultToOption

    /// Escape string for SQL (single quotes). Prefer parameterized queries.
    let escapeSql (s: string) : string option =
        let bytes = toUtf8 s
        FFI.proven_string_escape_sql(bytes, unativeint bytes.Length) |> stringResultToOption

    /// Escape string for HTML (prevents XSS).
    let escapeHtml (s: string) : string option =
        let bytes = toUtf8 s
        FFI.proven_string_escape_html(bytes, unativeint bytes.Length) |> stringResultToOption

    /// Escape string for JavaScript string literals.
    let escapeJs (s: string) : string option =
        let bytes = toUtf8 s
        FFI.proven_string_escape_js(bytes, unativeint bytes.Length) |> stringResultToOption
