// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe JSON validation delegated to libproven FFI.
///
/// Validates JSON structure without full parsing, using the
/// formally verified Idris 2 core via the Zig FFI bridge.

import CProven

/// JSON value type as detected by libproven.
public enum JsonValueType: Int32, Sendable {
    case null = 0
    case bool = 1
    case number = 2
    case string = 3
    case array = 4
    case object = 5
    case invalid = -1
}

public enum SafeJson {
    /// Check if a string is valid JSON.
    public static func isValid(_ json: String) -> Result<Bool, ProvenError> {
        withStringBytes(json) { ptr, len in
            let result = proven_json_is_valid(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Get the JSON value type at root level.
    public static func getType(_ json: String) -> JsonValueType {
        withStringBytes(json) { ptr, len in
            let cType = proven_json_get_type(ptr, len)
            return JsonValueType(rawValue: cType.rawValue) ?? .invalid
        }
    }
}
