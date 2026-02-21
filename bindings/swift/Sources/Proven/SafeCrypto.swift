// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Cryptographic safety operations with constant-time guarantees.
public enum SafeCrypto {
    /// Compare two byte arrays in constant time to prevent timing attacks.
    public static func constantTimeCompare(_ a: [UInt8], _ b: [UInt8]) -> Bool {
        guard a.count == b.count else { return false }
        guard !a.isEmpty else { return true }

        var result: UInt8 = 0
        for i in 0..<a.count {
            result |= a[i] ^ b[i]
        }
        return result == 0
    }

    /// Compare two Data objects in constant time to prevent timing attacks.
    public static func constantTimeCompare(_ a: Data, _ b: Data) -> Bool {
        constantTimeCompare([UInt8](a), [UInt8](b))
    }

    /// Compare two strings in constant time to prevent timing attacks.
    public static func constantTimeCompare(_ a: String, _ b: String) -> Bool {
        constantTimeCompare([UInt8](a.utf8), [UInt8](b.utf8))
    }

    /// Securely zero out a byte array to prevent data leakage.
    public static func secureZero(_ data: inout [UInt8]) {
        for i in 0..<data.count {
            data[i] = 0
        }
        // Prevent optimizer from removing the zeroing
        withUnsafePointer(to: &data) { _ in }
    }

    /// Securely zero out a Data object to prevent data leakage.
    public static func secureZero(_ data: inout Data) {
        data.withUnsafeMutableBytes { buffer in
            guard let ptr = buffer.baseAddress else { return }
            memset(ptr, 0, buffer.count)
        }
    }
}
