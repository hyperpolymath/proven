// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe phone number parsing and formatting via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafePhone =

    /// Parse phone number from E.164 string.
    /// Returns the PhoneResult struct on success, or None on failure.
    let parse (s: string) : PhoneResult option =
        let bytes = toUtf8 s
        let result = FFI.proven_phone_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result
        else None

    /// Format phone number as E.164 string (e.g., "+14155551234").
    let formatE164 (countryCode: uint16) (nationalNumber: uint64) : string option =
        FFI.proven_phone_format_e164(countryCode, nationalNumber) |> stringResultToOption
