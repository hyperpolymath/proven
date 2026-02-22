// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe currency operations with type-safe monetary values.
 *
 * Thin FFI wrapper around libproven's SafeCurrency module. Currency
 * parsing and formatting are performed in formally verified Idris 2
 * code. This module only marshals data to/from the C ABI.
 */
module proven.safe_currency;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Currency parse result (D-side wrapper).
struct CurrencyResult
{
    long amountMinor;
    ubyte[3] currencyCode;
    ubyte decimalPlaces;
    string error;
    bool ok;

    static CurrencyResult success(long amount, ubyte[3] code, ubyte decimals)
    {
        return CurrencyResult(amount, code, decimals, "", true);
    }

    static CurrencyResult failure(string error)
    {
        return CurrencyResult(0, [0, 0, 0], 0, error, false);
    }

    /// Get currency code as string (e.g., "USD").
    string codeString() const pure @safe
    {
        return (cast(const(char)[]) currencyCode[0 .. 3]).idup;
    }
}

/// Parse currency amount (e.g., "USD 123.45" or "123.45 EUR").
CurrencyResult parseCurrency(string input) @trusted nothrow
{
    if (input.length == 0)
        return CurrencyResult.failure("Empty input");

    auto result = proven_currency_parse(
        cast(const(ubyte)*) input.ptr, input.length
    );

    if (provenFailed(result.status))
        return CurrencyResult.failure("Currency parse failed");

    return CurrencyResult.success(
        result.amount_minor,
        result.currency_code,
        result.decimal_places
    );
}

/// Format currency amount.
/// code: 3-letter ISO 4217 currency code as bytes (e.g., "USD").
/// decimalPlaces: number of decimal places for the currency.
string formatCurrency(long amountMinor, string code, ubyte decimalPlaces) @trusted nothrow
{
    if (code.length != 3)
        return null;

    ubyte[3] codeBytes = [
        cast(ubyte) code[0],
        cast(ubyte) code[1],
        cast(ubyte) code[2]
    ];

    auto result = proven_currency_format(amountMinor, codeBytes, decimalPlaces);
    return provenStringToD(result);
}
