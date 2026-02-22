// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe date/time operations via libproven FFI.
//!
//! Parses and formats ISO 8601 dates, validates calendar properties.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// DateTime components.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DateTime {
    /// Year (negative for BCE).
    pub year: i32,
    /// Month (1-12).
    pub month: u8,
    /// Day of month (1-31).
    pub day: u8,
    /// Hour (0-23).
    pub hour: u8,
    /// Minute (0-59).
    pub minute: u8,
    /// Second (0-59).
    pub second: u8,
    /// Nanosecond (0-999999999).
    pub nanosecond: u32,
    /// Timezone offset in minutes from UTC (negative for west).
    pub tz_offset_minutes: i16,
}

/// Safe date/time operations.
pub struct SafeDateTime;

impl SafeDateTime {
    /// Parse an ISO 8601 date/time string.
    ///
    /// Supports formats like `2024-01-15T10:30:00Z` and
    /// `2024-01-15T10:30:00+05:30`.
    pub fn parse(input: &str) -> Result<DateTime> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_datetime_parse(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;
        Ok(DateTime {
            year: result.datetime.year,
            month: result.datetime.month,
            day: result.datetime.day,
            hour: result.datetime.hour,
            minute: result.datetime.minute,
            second: result.datetime.second,
            nanosecond: result.datetime.nanosecond,
            tz_offset_minutes: result.datetime.tz_offset_minutes,
        })
    }

    /// Format a DateTime as an ISO 8601 string.
    pub fn format_iso8601(dt: &DateTime) -> Result<String> {
        let ffi_dt = ffi::DateTime {
            year: dt.year,
            month: dt.month,
            day: dt.day,
            hour: dt.hour,
            minute: dt.minute,
            second: dt.second,
            nanosecond: dt.nanosecond,
            tz_offset_minutes: dt.tz_offset_minutes,
        };
        // SAFETY: proven_datetime_format_iso8601 takes a value-type DateTime;
        // always safe to call. Returns a StringResult that we free.
        let result = unsafe { ffi::proven_datetime_format_iso8601(ffi_dt) };
        core::string_result_to_result(result)
    }

    /// Check if a year is a leap year.
    pub fn is_leap_year(year: i32) -> bool {
        // SAFETY: proven_datetime_is_leap_year takes a value-type i32;
        // always safe to call.
        unsafe { ffi::proven_datetime_is_leap_year(year) }
    }

    /// Get the number of days in a given month.
    ///
    /// Accounts for leap years when month is February.
    pub fn days_in_month(year: i32, month: u8) -> u8 {
        // SAFETY: proven_datetime_days_in_month takes value-type arguments;
        // always safe to call.
        unsafe { ffi::proven_datetime_days_in_month(year, month) }
    }
}
