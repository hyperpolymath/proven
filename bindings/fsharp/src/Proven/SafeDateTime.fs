// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe date/time operations via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeDateTime =

    /// Parse ISO 8601 date string into a ProvenDateTime struct.
    /// Returns None on parse failure.
    let parse (s: string) : ProvenDateTime option =
        let bytes = toUtf8 s
        let result = FFI.proven_datetime_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result.DateTime
        else None

    /// Format ProvenDateTime as ISO 8601 string.
    let formatIso8601 (dt: ProvenDateTime) : string option =
        FFI.proven_datetime_format_iso8601(dt) |> stringResultToOption

    /// Check if year is a leap year.
    let isLeapYear (year: int32) : bool =
        FFI.proven_datetime_is_leap_year(year)

    /// Get the number of days in a given month and year.
    let daysInMonth (year: int32) (month: byte) : byte =
        FFI.proven_datetime_days_in_month(year, month)
