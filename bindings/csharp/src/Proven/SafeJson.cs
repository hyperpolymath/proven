// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeJson.cs - JSON validation and type detection.
//
// Thin P/Invoke wrapper over libproven. ALL computation is performed in
// verified Idris 2 code via the Zig FFI bridge. No logic is reimplemented here.

namespace Proven
{
    /// <summary>
    /// Safe JSON operations backed by formally verified Idris 2 code.
    /// Provides JSON string validation and root-level type detection.
    /// All methods delegate to the libproven FFI.
    /// </summary>
    public static class SafeJson
    {
        /// <summary>
        /// Check if a string is valid JSON.
        /// Delegates to proven_json_is_valid via FFI.
        /// </summary>
        /// <param name="json">The JSON string to validate.</param>
        /// <returns>true if valid JSON, false if invalid, null on FFI error.</returns>
        public static bool? IsValid(string json)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(json);
            return MarshalHelpers.BoolResultToNullable(
                LibProven.proven_json_is_valid(bytes, (nuint)bytes.Length));
        }

        /// <summary>
        /// Get the JSON value type at the root level.
        /// Delegates to proven_json_get_type via FFI.
        /// </summary>
        /// <param name="json">The JSON string to inspect.</param>
        /// <returns>The root-level JSON type, or JsonType.Invalid for non-JSON input.</returns>
        public static JsonType GetType(string json)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(json);
            int rawType = LibProven.proven_json_get_type(bytes, (nuint)bytes.Length);
            return (JsonType)rawType;
        }
    }
}
