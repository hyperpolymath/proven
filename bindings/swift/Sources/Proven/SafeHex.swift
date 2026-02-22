// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Hexadecimal encoding/decoding delegated to libproven FFI.
///
/// All encoding and decoding via the formally verified Idris 2 core.

import CProven

public enum SafeHex {
    /// Encode bytes to hex string.
    public static func encode(_ data: [UInt8], uppercase: Bool = false) -> Result<String, ProvenError> {
        data.withUnsafeBufferPointer { buffer in
            consumeStringResult(proven_hex_encode(buffer.baseAddress, buffer.count, uppercase))
        }
    }

    /// Decode hex string to bytes.
    public static func decode(_ hexString: String) -> Result<[UInt8], ProvenError> {
        withStringBytes(hexString) { ptr, len in
            var result = proven_hex_decode(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            guard let dataPtr = result.data else {
                return .failure(.nullPointer)
            }
            let bytes = Array(UnsafeBufferPointer(start: dataPtr, count: result.length))
            proven_hex_free(&result)
            return .success(bytes)
        }
    }
}
