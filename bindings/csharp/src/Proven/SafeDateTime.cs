// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeDateTime.cs - ISO 8601 date/time handling.
//
// Thin P/Invoke wrapper over libproven. ALL computation is performed in
// verified Idris 2 code via the Zig FFI bridge. No logic is reimplemented here.

namespace Proven
{
    /// <summary>
    /// Safe date/time operations backed by formally verified Idris 2 code.
    /// Provides ISO 8601 parsing and formatting, leap year detection, and
    /// days-in-month calculation. All methods delegate to the libproven FFI.
    /// </summary>
    public static class SafeDateTime
    {
        /// <summary>
        /// Parse an ISO 8601 date string into its components.
        /// Supports: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, YYYY-MM-DDTHH:MM:SSZ,
        /// YYYY-MM-DDTHH:MM:SS+HH:MM.
        /// Delegates to proven_datetime_parse via FFI.
        /// </summary>
        /// <param name="isoString">The ISO 8601 date/time string to parse.</param>
        /// <returns>Parsed date/time components, or null on parse failure.</returns>
        public static ProvenDateTime? Parse(string isoString)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(isoString);
            DateTimeResult result = LibProven.proven_datetime_parse(bytes, (nuint)bytes.Length);

            if (result.Status == 0)
            {
                return result.DateTime;
            }

            return null;
        }

        /// <summary>
        /// Format a ProvenDateTime as an ISO 8601 string.
        /// Delegates to proven_datetime_format_iso8601 via FFI.
        /// </summary>
        /// <param name="dt">The date/time components to format.</param>
        /// <returns>The ISO 8601 formatted string, or null on error.</returns>
        public static string? FormatIso8601(ProvenDateTime dt)
        {
            return MarshalHelpers.StringResultToManaged(
                LibProven.proven_datetime_format_iso8601(dt));
        }

        /// <summary>
        /// Check if a year is a leap year.
        /// Delegates to proven_datetime_is_leap_year via FFI.
        /// </summary>
        /// <param name="year">The year to check.</param>
        /// <returns>true if the year is a leap year.</returns>
        public static bool IsLeapYear(int year)
        {
            return LibProven.proven_datetime_is_leap_year(year);
        }

        /// <summary>
        /// Get the number of days in a given month.
        /// Delegates to proven_datetime_days_in_month via FFI.
        /// </summary>
        /// <param name="year">The year (needed for leap year calculation in February).</param>
        /// <param name="month">The month (1-12).</param>
        /// <returns>The number of days in the month, or 0 for an invalid month.</returns>
        public static byte DaysInMonth(int year, byte month)
        {
            return LibProven.proven_datetime_days_in_month(year, month);
        }

        /// <summary>
        /// Parse an ISO 8601 string and immediately format it back (round-trip).
        /// Useful for normalizing date/time strings.
        /// Delegates to proven_datetime_parse and proven_datetime_format_iso8601 via FFI.
        /// </summary>
        /// <param name="isoString">The ISO 8601 date/time string.</param>
        /// <returns>The normalized ISO 8601 string, or null on error.</returns>
        public static string? Normalize(string isoString)
        {
            ProvenDateTime? parsed = Parse(isoString);
            if (parsed is null)
            {
                return null;
            }
            return FormatIso8601(parsed.Value);
        }
    }
}
