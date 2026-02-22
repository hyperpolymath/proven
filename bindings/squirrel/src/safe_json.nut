// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeJson - JSON validation for Squirrel.
//
// All operations delegate to libproven via the native "proven" table.
// Returns null on error.

/**
 * SafeJson - JSON validation and type detection.
 *
 * @example
 *   local json = SafeJson();
 *   print(json.is_valid(@"{""key"": 42}"));   // true
 *   print(json.is_valid("not json"));          // false
 *   print(json.get_type(@"{""key"": 42}"));   // "object"
 *   print(json.get_type("42"));                // "number"
 */
class SafeJson {
    /**
     * Check if a string is valid JSON.
     * @param {string} json_str - JSON string to validate.
     * @return {bool|null} true if valid JSON, null on error.
     */
    function is_valid(json_str) {
        return proven.json_is_valid(json_str);
    }

    /**
     * Get the root-level JSON value type.
     * @param {string} json_str - JSON string.
     * @return {string} One of: "null", "bool", "number", "string",
     *         "array", "object", "invalid".
     */
    function get_type(json_str) {
        return proven.json_get_type(json_str);
    }
}
