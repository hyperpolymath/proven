// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe currency operations via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeCurrency =

    /// Parse currency amount (e.g., "USD 123.45").
    /// Returns the CurrencyResult struct on success, or None on failure.
    let parse (s: string) : CurrencyResult option =
        let bytes = toUtf8 s
        let result = FFI.proven_currency_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result
        else None

    /// Format currency amount for display.
    /// code: 3-byte ISO 4217 currency code (e.g., [| 0x55uy; 0x53uy; 0x44uy |] for "USD").
    /// decimalPlaces: number of fractional digits for this currency.
    let format (amountMinor: int64) (code: byte array) (decimalPlaces: byte) : string option =
        FFI.proven_currency_format(amountMinor, code, decimalPlaces) |> stringResultToOption
