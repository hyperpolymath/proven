// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe filesystem path operations delegated to libproven FFI.
///
/// Prevents directory traversal attacks and sanitizes filenames
/// via the formally verified Idris 2 core.

import CProven

public enum SafePath {
    /// Check if a path contains directory traversal sequences.
    public static func hasTraversal(_ path: String) -> Result<Bool, ProvenError> {
        withStringBytes(path) { ptr, len in
            let result = proven_path_has_traversal(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Sanitize a filename by removing dangerous characters.
    public static func sanitizeFilename(_ filename: String) -> Result<String, ProvenError> {
        withStringBytes(filename) { ptr, len in
            consumeStringResult(proven_path_sanitize_filename(ptr, len))
        }
    }
}
