// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe JSON validation and type detection via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeJson =

    /// Check if string is valid JSON.
    let isValid (json: string) : bool option =
        let bytes = toUtf8 json
        FFI.proven_json_is_valid(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Get JSON value type at root level.
    /// Returns a JsonType enum value (Null=0, Bool=1, Number=2, String=3,
    /// Array=4, Object=5, Invalid=-1).
    let getType (json: string) : JsonType =
        let bytes = toUtf8 json
        enum<JsonType>(FFI.proven_json_get_type(bytes, unativeint bytes.Length))
