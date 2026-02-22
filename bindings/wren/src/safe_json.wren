// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeJson - JSON validation and type detection via libproven FFI.
// All computation is performed in verified Idris 2 code via the Zig FFI
// layer. This Wren class is a thin wrapper; it does NOT reimplement any logic.

// Safe JSON validation and type detection.
// Returns null on error instead of crashing.
class SafeJson {
    // Check if string is valid JSON.
    // Returns true if valid, false if invalid, null on error.
    foreign static isValid(s)

    // Get the JSON value type at the root level.
    // Returns one of: "null", "boolean", "number", "string",
    // "array", "object", or "invalid".
    foreign static getType(s)
}
