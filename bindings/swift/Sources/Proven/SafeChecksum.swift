// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Checksum operations delegated to libproven FFI.
///
/// CRC-32 computation and verification via the formally verified
/// Idris 2 core.

import CProven

public enum SafeChecksum {
    /// Calculate CRC-32 checksum.
    public static func crc32(_ data: [UInt8]) -> Result<Int64, ProvenError> {
        data.withUnsafeBufferPointer { buffer in
            let result = proven_checksum_crc32(buffer.baseAddress, buffer.count)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Calculate CRC-32 checksum for a string.
    public static func crc32(_ string: String) -> Result<Int64, ProvenError> {
        crc32(Array(string.utf8))
    }

    /// Verify CRC-32 checksum.
    public static func verifyCrc32(_ data: [UInt8], expected: UInt32) -> Result<Bool, ProvenError> {
        data.withUnsafeBufferPointer { buffer in
            let result = proven_checksum_verify_crc32(buffer.baseAddress, buffer.count, expected)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }
}
