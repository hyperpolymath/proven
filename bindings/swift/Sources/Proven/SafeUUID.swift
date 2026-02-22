// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// UUID operations delegated to libproven FFI.
///
/// RFC 4122 UUID generation, parsing, and formatting via the
/// formally verified Idris 2 core.

import CProven

/// A UUID value (128-bit identifier).
public struct ProvenUUIDValue: Equatable, Sendable {
    /// The 16 bytes of the UUID.
    public let bytes: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8,
                       UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
}

public enum SafeUUID {
    /// Generate a random v4 UUID.
    public static func v4() -> Result<ProvenUUIDValue, ProvenError> {
        let result = proven_uuid_v4()
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        let b = result.uuid.bytes
        return .success(ProvenUUIDValue(bytes: (
            b.0, b.1, b.2, b.3, b.4, b.5, b.6, b.7,
            b.8, b.9, b.10, b.11, b.12, b.13, b.14, b.15
        )))
    }

    /// Format a UUID as canonical string (8-4-4-4-12).
    public static func toString(_ uuid: ProvenUUIDValue) -> Result<String, ProvenError> {
        var cUuid = ProvenUUID()
        let b = uuid.bytes
        cUuid.bytes.0 = b.0; cUuid.bytes.1 = b.1; cUuid.bytes.2 = b.2; cUuid.bytes.3 = b.3
        cUuid.bytes.4 = b.4; cUuid.bytes.5 = b.5; cUuid.bytes.6 = b.6; cUuid.bytes.7 = b.7
        cUuid.bytes.8 = b.8; cUuid.bytes.9 = b.9; cUuid.bytes.10 = b.10; cUuid.bytes.11 = b.11
        cUuid.bytes.12 = b.12; cUuid.bytes.13 = b.13; cUuid.bytes.14 = b.14; cUuid.bytes.15 = b.15
        return consumeStringResult(proven_uuid_to_string(cUuid))
    }

    /// Parse a UUID from canonical string format.
    public static func parse(_ string: String) -> Result<ProvenUUIDValue, ProvenError> {
        withStringBytes(string) { ptr, len in
            let result = proven_uuid_parse(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            let b = result.uuid.bytes
            return .success(ProvenUUIDValue(bytes: (
                b.0, b.1, b.2, b.3, b.4, b.5, b.6, b.7,
                b.8, b.9, b.10, b.11, b.12, b.13, b.14, b.15
            )))
        }
    }

    /// Check if a UUID is the nil UUID (all zeros).
    public static func isNil(_ uuid: ProvenUUIDValue) -> Bool {
        var cUuid = ProvenUUID()
        let b = uuid.bytes
        cUuid.bytes.0 = b.0; cUuid.bytes.1 = b.1; cUuid.bytes.2 = b.2; cUuid.bytes.3 = b.3
        cUuid.bytes.4 = b.4; cUuid.bytes.5 = b.5; cUuid.bytes.6 = b.6; cUuid.bytes.7 = b.7
        cUuid.bytes.8 = b.8; cUuid.bytes.9 = b.9; cUuid.bytes.10 = b.10; cUuid.bytes.11 = b.11
        cUuid.bytes.12 = b.12; cUuid.bytes.13 = b.13; cUuid.bytes.14 = b.14; cUuid.bytes.15 = b.15
        return proven_uuid_is_nil(cUuid)
    }

    /// Get the UUID version number.
    public static func version(_ uuid: ProvenUUIDValue) -> UInt8 {
        var cUuid = ProvenUUID()
        let b = uuid.bytes
        cUuid.bytes.0 = b.0; cUuid.bytes.1 = b.1; cUuid.bytes.2 = b.2; cUuid.bytes.3 = b.3
        cUuid.bytes.4 = b.4; cUuid.bytes.5 = b.5; cUuid.bytes.6 = b.6; cUuid.bytes.7 = b.7
        cUuid.bytes.8 = b.8; cUuid.bytes.9 = b.9; cUuid.bytes.10 = b.10; cUuid.bytes.11 = b.11
        cUuid.bytes.12 = b.12; cUuid.bytes.13 = b.13; cUuid.bytes.14 = b.14; cUuid.bytes.15 = b.15
        return proven_uuid_version(cUuid)
    }
}
