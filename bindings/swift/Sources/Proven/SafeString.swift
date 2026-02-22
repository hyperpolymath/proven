// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe string operations delegated to libproven FFI.
///
/// All escaping and validation is performed by the formally verified
/// Idris 2 core via the Zig FFI bridge.

import CProven

public enum SafeString {
    /// Check if bytes are valid UTF-8.
    public static func isValidUtf8(_ data: [UInt8]) -> Result<Bool, ProvenError> {
        let result = data.withUnsafeBufferPointer { buffer in
            proven_string_is_valid_utf8(buffer.baseAddress, buffer.count)
        }
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Escape a string for safe SQL interpolation.
    /// Note: Prefer parameterized queries over string escaping.
    public static func escapeSql(_ value: String) -> Result<String, ProvenError> {
        withStringBytes(value) { ptr, len in
            consumeStringResult(proven_string_escape_sql(ptr, len))
        }
    }

    /// Escape a string for safe HTML insertion.
    public static func escapeHtml(_ value: String) -> Result<String, ProvenError> {
        withStringBytes(value) { ptr, len in
            consumeStringResult(proven_string_escape_html(ptr, len))
        }
    }

    /// Escape a string for safe JavaScript string literal insertion.
    public static func escapeJs(_ value: String) -> Result<String, ProvenError> {
        withStringBytes(value) { ptr, len in
            consumeStringResult(proven_string_escape_js(ptr, len))
        }
    }
}
