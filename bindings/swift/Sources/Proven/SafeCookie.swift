// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe HTTP cookie operations delegated to libproven FFI.
///
/// All validation and rendering is performed by the formally verified
/// Idris 2 core via the Zig FFI bridge.

import CProven

public enum SafeCookie {
    /// Check if a cookie value contains injection characters.
    public static func hasInjection(_ value: String) -> Result<Bool, ProvenError> {
        withStringBytes(value) { ptr, len in
            let result = proven_cookie_has_injection(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Validate a cookie name per RFC 6265.
    public static func validateName(_ name: String) -> Result<Bool, ProvenError> {
        withStringBytes(name) { ptr, len in
            let result = proven_cookie_validate_name(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Validate a cookie value per RFC 6265.
    public static func validateValue(_ value: String) -> Result<Bool, ProvenError> {
        withStringBytes(value) { ptr, len in
            let result = proven_cookie_validate_value(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Get the security prefix type of a cookie name.
    /// Returns an integer code identifying the prefix (e.g. __Secure-, __Host-).
    public static func getPrefix(_ name: String) -> Result<Int64, ProvenError> {
        withStringBytes(name) { ptr, len in
            let result = proven_cookie_get_prefix(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }

    /// Build a Set-Cookie header string from validated components.
    public static func buildSetCookie(
        name: String,
        value: String,
        maxAge: Int64 = 0,
        secure: Bool = true,
        httpOnly: Bool = true,
        sameSiteStrict: Bool = true
    ) -> Result<String, ProvenError> {
        let nameUtf8 = Array(name.utf8)
        let valueUtf8 = Array(value.utf8)
        return nameUtf8.withUnsafeBufferPointer { nameBuf in
            valueUtf8.withUnsafeBufferPointer { valueBuf in
                let result = proven_cookie_build_set_cookie(
                    nameBuf.baseAddress, nameBuf.count,
                    valueBuf.baseAddress, valueBuf.count,
                    maxAge, secure, httpOnly, sameSiteStrict
                )
                return consumeStringResult(result)
            }
        }
    }

    /// Build a Set-Cookie header that deletes a cookie.
    public static func buildDelete(name: String) -> Result<String, ProvenError> {
        withStringBytes(name) { ptr, len in
            consumeStringResult(proven_cookie_build_delete(ptr, len))
        }
    }
}
