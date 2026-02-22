// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe URL parsing and encoding via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeUrl =

    /// Parse a URL string into an opaque handle.
    /// The caller MUST call free() on the returned handle when done.
    /// Returns nativeint.Zero on failure.
    let parse (url: string) : nativeint =
        let bytes = toUtf8 url
        FFI.proven_url_parse(bytes, unativeint bytes.Length)

    /// Free a URL components handle returned by parse().
    let free (components: nativeint) : unit =
        if components <> nativeint 0 then
            FFI.proven_url_free(components)

    /// URL-encode a string (percent-encoding).
    let urlEncode (s: string) : string option =
        let bytes = toUtf8 s
        FFI.proven_http_url_encode(bytes, unativeint bytes.Length) |> stringResultToOption

    /// URL-decode a string (percent-decoding).
    let urlDecode (s: string) : string option =
        let bytes = toUtf8 s
        FFI.proven_http_url_decode(bytes, unativeint bytes.Length) |> stringResultToOption
