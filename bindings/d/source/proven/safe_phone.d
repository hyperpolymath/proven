// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe phone number validation and formatting operations.
 *
 * Thin FFI wrapper around libproven's SafePhone module. Phone number
 * parsing (E.164) and formatting are performed in formally verified
 * Idris 2 code. This module only marshals data to/from the C ABI.
 */
module proven.safe_phone;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Parsed phone number.
struct PhoneNumber
{
    ushort countryCode;
    ulong nationalNumber;
    bool isValid;
}

/// Phone number parsing result.
struct PhoneResult
{
    PhoneNumber phone;
    string error;
    bool ok;

    static PhoneResult success(PhoneNumber phone)
    {
        return PhoneResult(phone, "", true);
    }

    static PhoneResult failure(string error)
    {
        return PhoneResult(PhoneNumber.init, error, false);
    }
}

/// Parse phone number from string to E.164 format.
PhoneResult parsePhone(string input) @trusted nothrow
{
    if (input.length == 0)
        return PhoneResult.failure("Empty input");

    auto result = proven_phone_parse(
        cast(const(ubyte)*) input.ptr, input.length
    );

    if (provenFailed(result.status))
        return PhoneResult.failure("Phone parse failed");

    return PhoneResult.success(PhoneNumber(
        result.country_code,
        result.national_number,
        result.is_valid
    ));
}

/// Format phone number as E.164 string (e.g., "+15551234567").
string formatE164(ushort countryCode, ulong nationalNumber) @trusted nothrow
{
    auto result = proven_phone_format_e164(countryCode, nationalNumber);
    return provenStringToD(result);
}

/// Format a parsed PhoneNumber as E.164.
string formatE164Phone(const PhoneNumber phone) @trusted nothrow
{
    return formatE164(phone.countryCode, phone.nationalNumber);
}

/// Check if string is a valid phone number.
bool isValidPhone(string input) @trusted nothrow
{
    auto result = parsePhone(input);
    return result.ok && result.phone.isValid;
}
