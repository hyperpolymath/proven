// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeDateTime.vala -- ISO 8601 date/time parsing and formatting.
 *
 * Every method delegates to libproven via C extern calls declared in
 * lib_proven.vapi.  No datetime logic is reimplemented here.
 */
namespace Proven {

    /**
     * Parsed date/time components returned by {@link SafeDateTime.parse}.
     */
    public class DateTimeParts : GLib.Object {
        /** Year (negative for BCE). */
        public int32  year              { get; private set; }
        /** Month (1-12). */
        public uint8  month             { get; private set; }
        /** Day (1-31). */
        public uint8  day               { get; private set; }
        /** Hour (0-23). */
        public uint8  hour              { get; private set; }
        /** Minute (0-59). */
        public uint8  minute            { get; private set; }
        /** Second (0-59). */
        public uint8  second            { get; private set; }
        /** Sub-second nanoseconds. */
        public uint32 nanosecond        { get; private set; }
        /** Timezone offset in minutes from UTC (negative = west). */
        public int16  tz_offset_minutes { get; private set; }

        internal LibProven.DateTime _raw;

        internal DateTimeParts (LibProven.DateTime dt) {
            _raw              = dt;
            year              = dt.year;
            month             = dt.month;
            day               = dt.day;
            hour              = dt.hour;
            minute            = dt.minute;
            second            = dt.second;
            nanosecond        = dt.nanosecond;
            tz_offset_minutes = dt.tz_offset_minutes;
        }

        /**
         * Format this date/time as an ISO 8601 string.
         *
         * @return ISO 8601 formatted string, or null on error.
         */
        public string? to_iso8601 () {
            LibProven.StringResult r = LibProven.datetime_format_iso8601 (_raw);
            if (r.status != 0) {
                return null;
            }
            string result = ((string) r.value).substring (0, (long) r.length);
            LibProven.free_string (r.value);
            return result;
        }
    }

    /**
     * Safe date and time operations.
     *
     * Parses ISO 8601 date strings and provides calendar utilities such
     * as leap year detection and days-in-month queries.
     */
    public class SafeDateTime : GLib.Object {

        /**
         * Parse an ISO 8601 date/time string.
         *
         * Supported formats:
         *   - YYYY-MM-DD
         *   - YYYY-MM-DDTHH:MM:SS
         *   - YYYY-MM-DDTHH:MM:SSZ
         *   - YYYY-MM-DDTHH:MM:SS+HH:MM
         *
         * @param iso_string ISO 8601 date/time string.
         * @return Parsed components, or null on parse failure.
         */
        public static DateTimeParts? parse (string iso_string) {
            unowned uint8[] data = (uint8[]) iso_string.data;
            LibProven.DateTimeResult r = LibProven.datetime_parse (data);
            if (r.status != 0) {
                return null;
            }
            return new DateTimeParts (r.datetime);
        }

        /**
         * Check whether a year is a leap year.
         *
         * @param year Year to check.
         * @return true if leap year.
         */
        public static bool is_leap_year (int32 year) {
            return LibProven.datetime_is_leap_year (year);
        }

        /**
         * Get the number of days in a given month.
         *
         * @param year  Year (needed for February leap-year check).
         * @param month Month (1-12).
         * @return Number of days, or 0 if the month is invalid.
         */
        public static uint8 days_in_month (int32 year, uint8 month) {
            return LibProven.datetime_days_in_month (year, month);
        }
    }
}
