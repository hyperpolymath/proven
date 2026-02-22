// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe HTTP header operations delegated to libproven FFI.
///
/// All validation and rendering is performed by the formally verified
/// Idris 2 core via the Zig FFI bridge.

import CProven

public enum SafeHeader {
    /// Check if a header value contains CRLF injection characters.
    public static func hasCRLF(_ value: String) -> Result<Bool, ProvenError> {
        withStringBytes(value) { ptr, len in
            let result = proven_header_has_crlf(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Check if a header name is valid per RFC 7230.
    public static func isValidName(_ name: String) -> Result<Bool, ProvenError> {
        withStringBytes(name) { ptr, len in
            let result = proven_header_is_valid_name(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Check if a header name is considered dangerous (e.g. security-sensitive).
    public static func isDangerous(_ name: String) -> Result<Bool, ProvenError> {
        withStringBytes(name) { ptr, len in
            let result = proven_header_is_dangerous(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Render a validated header as "Name: Value" string.
    public static func render(name: String, value: String) -> Result<String, ProvenError> {
        let nameUtf8 = Array(name.utf8)
        let valueUtf8 = Array(value.utf8)
        return nameUtf8.withUnsafeBufferPointer { nameBuf in
            valueUtf8.withUnsafeBufferPointer { valueBuf in
                let result = proven_header_render(
                    nameBuf.baseAddress, nameBuf.count,
                    valueBuf.baseAddress, valueBuf.count
                )
                return consumeStringResult(result)
            }
        }
    }

    /// Build a Content-Security-Policy header from JSON-encoded directives.
    public static func buildCSP(directivesJSON: String) -> Result<String, ProvenError> {
        withStringBytes(directivesJSON) { ptr, len in
            consumeStringResult(proven_header_build_csp(ptr, len))
        }
    }

    /// Build a Strict-Transport-Security header.
    public static func buildHSTS(
        maxAge: Int64,
        includeSubdomains: Bool = false,
        preload: Bool = false
    ) -> Result<String, ProvenError> {
        let result = proven_header_build_hsts(maxAge, includeSubdomains, preload)
        return consumeStringResult(result)
    }
}
