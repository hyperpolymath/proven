// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Phone number parsing delegated to libproven FFI.
///
/// E.164 phone number parsing and formatting via the formally
/// verified Idris 2 core.

import CProven

/// Parsed phone number.
public struct PhoneNumber: Equatable, Sendable {
    public let countryCode: UInt16
    public let nationalNumber: UInt64
}

public enum SafePhone {
    /// Parse a phone number from string.
    public static func parse(_ input: String) -> Result<PhoneNumber, ProvenError> {
        withStringBytes(input) { ptr, len in
            let result = proven_phone_parse(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(PhoneNumber(
                countryCode: result.country_code,
                nationalNumber: result.national_number
            ))
        }
    }

    /// Format a phone number in E.164 format (e.g., "+15551234567").
    public static func formatE164(countryCode: UInt16, nationalNumber: UInt64) -> Result<String, ProvenError> {
        consumeStringResult(proven_phone_format_e164(countryCode, nationalNumber))
    }

    /// Format a parsed phone number in E.164 format.
    public static func formatE164(_ phone: PhoneNumber) -> Result<String, ProvenError> {
        formatE164(countryCode: phone.countryCode, nationalNumber: phone.nationalNumber)
    }
}
