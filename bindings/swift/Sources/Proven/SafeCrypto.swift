// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Cryptographic safety operations delegated to libproven FFI.
///
/// Provides timing-safe comparison and secure random byte generation
/// via the formally verified Idris 2 core.

import CProven

public enum SafeCrypto {
    /// Constant-time byte comparison to prevent timing attacks.
    public static func constantTimeCompare(_ a: [UInt8], _ b: [UInt8]) -> Result<Bool, ProvenError> {
        a.withUnsafeBufferPointer { aBuf in
            b.withUnsafeBufferPointer { bBuf in
                let result = proven_crypto_constant_time_eq(
                    aBuf.baseAddress, aBuf.count,
                    bBuf.baseAddress, bBuf.count
                )
                if let error = ProvenError.fromStatus(result.status) {
                    return .failure(error)
                }
                return .success(result.value)
            }
        }
    }

    /// Constant-time string comparison to prevent timing attacks.
    public static func constantTimeCompare(_ a: String, _ b: String) -> Result<Bool, ProvenError> {
        constantTimeCompare(Array(a.utf8), Array(b.utf8))
    }

    /// Fill a buffer with cryptographically secure random bytes.
    public static func randomBytes(count: Int) -> Result<[UInt8], ProvenError> {
        var buffer = [UInt8](repeating: 0, count: count)
        let status = buffer.withUnsafeMutableBufferPointer { buf in
            proven_crypto_random_bytes(buf.baseAddress, buf.count)
        }
        if let error = ProvenError.fromStatus(status) {
            return .failure(error)
        }
        return .success(buffer)
    }
}
