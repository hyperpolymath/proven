// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe datetime parsing and validation.

use crate::core::{Error, Result};

/// Safe datetime operations.
pub struct SafeDateTime;

/// Parsed date components.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Date {
    /// Year (e.g., 2025)
    pub year: i32,
    /// Month (1-12)
    pub month: u8,
    /// Day (1-31)
    pub day: u8,
}

/// Parsed time components.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Time {
    /// Hour (0-23)
    pub hour: u8,
    /// Minute (0-59)
    pub minute: u8,
    /// Second (0-59)
    pub second: u8,
    /// Nanoseconds
    pub nanos: u32,
}

impl SafeDateTime {
    /// Check if a year is a leap year.
    pub fn is_leap_year(year: i32) -> bool {
        (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
    }

    /// Get days in a month.
    pub fn days_in_month(year: i32, month: u8) -> Option<u8> {
        match month {
            1 | 3 | 5 | 7 | 8 | 10 | 12 => Some(31),
            4 | 6 | 9 | 11 => Some(30),
            2 => Some(if Self::is_leap_year(year) { 29 } else { 28 }),
            _ => None,
        }
    }

    /// Validate a date.
    pub fn validate_date(year: i32, month: u8, day: u8) -> Result<Date> {
        if !(1..=12).contains(&month) {
            return Err(Error::ValidationError(format!(
                "Invalid month: {}",
                month
            )));
        }

        let max_day = Self::days_in_month(year, month)
            .ok_or_else(|| Error::ValidationError(format!("Invalid month: {}", month)))?;

        if day < 1 || day > max_day {
            return Err(Error::ValidationError(format!(
                "Invalid day {} for month {}",
                day, month
            )));
        }

        Ok(Date { year, month, day })
    }

    /// Validate a time.
    pub fn validate_time(hour: u8, minute: u8, second: u8) -> Result<Time> {
        if hour > 23 {
            return Err(Error::ValidationError(format!("Invalid hour: {}", hour)));
        }
        if minute > 59 {
            return Err(Error::ValidationError(format!(
                "Invalid minute: {}",
                minute
            )));
        }
        if second > 59 {
            return Err(Error::ValidationError(format!(
                "Invalid second: {}",
                second
            )));
        }

        Ok(Time {
            hour,
            minute,
            second,
            nanos: 0,
        })
    }

    /// Parse ISO 8601 date (YYYY-MM-DD).
    pub fn parse_iso_date(s: &str) -> Result<Date> {
        let parts: Vec<&str> = s.split('-').collect();
        if parts.len() != 3 {
            return Err(Error::ParseError("Invalid date format".into()));
        }

        let year = parts[0]
            .parse::<i32>()
            .map_err(|_| Error::ParseError("Invalid year".into()))?;
        let month = parts[1]
            .parse::<u8>()
            .map_err(|_| Error::ParseError("Invalid month".into()))?;
        let day = parts[2]
            .parse::<u8>()
            .map_err(|_| Error::ParseError("Invalid day".into()))?;

        Self::validate_date(year, month, day)
    }

    /// Parse ISO 8601 time (HH:MM:SS).
    pub fn parse_iso_time(s: &str) -> Result<Time> {
        let parts: Vec<&str> = s.split(':').collect();
        if parts.len() < 2 || parts.len() > 3 {
            return Err(Error::ParseError("Invalid time format".into()));
        }

        let hour = parts[0]
            .parse::<u8>()
            .map_err(|_| Error::ParseError("Invalid hour".into()))?;
        let minute = parts[1]
            .parse::<u8>()
            .map_err(|_| Error::ParseError("Invalid minute".into()))?;
        let second = if parts.len() > 2 {
            parts[2]
                .split('.')
                .next()
                .unwrap_or("0")
                .parse::<u8>()
                .map_err(|_| Error::ParseError("Invalid second".into()))?
        } else {
            0
        };

        Self::validate_time(hour, minute, second)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_leap_year() {
        assert!(SafeDateTime::is_leap_year(2000));
        assert!(SafeDateTime::is_leap_year(2024));
        assert!(!SafeDateTime::is_leap_year(1900));
        assert!(!SafeDateTime::is_leap_year(2023));
    }

    #[test]
    fn test_parse_date() {
        let date = SafeDateTime::parse_iso_date("2025-01-12").unwrap();
        assert_eq!(date.year, 2025);
        assert_eq!(date.month, 1);
        assert_eq!(date.day, 12);
    }

    #[test]
    fn test_invalid_date() {
        assert!(SafeDateTime::parse_iso_date("2025-13-01").is_err());
        assert!(SafeDateTime::parse_iso_date("2025-02-30").is_err());
    }
}
