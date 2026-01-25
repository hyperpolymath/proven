// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe phone number validation following E.164.

use crate::core::{Error, Result};
use std::fmt;

/// Country calling codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CountryCode {
    Us = 1,   // USA, Canada
    Ru = 7,   // Russia
    Eg = 20,  // Egypt
    Za = 27,  // South Africa
    Fr = 33,  // France
    Es = 34,  // Spain
    It = 39,  // Italy
    Uk = 44,  // UK
    De = 49,  // Germany
    Mx = 52,  // Mexico
    Br = 55,  // Brazil
    Au = 61,  // Australia
    Jp = 81,  // Japan
    Kr = 82,  // South Korea
    Cn = 86,  // China
    In = 91,  // India
    Unknown = 0,
}

impl CountryCode {
    /// Get numeric value.
    pub fn value(&self) -> u16 {
        *self as u16
    }

    /// Parse from number.
    pub fn from_value(v: u16) -> Self {
        match v {
            1 => CountryCode::Us,
            7 => CountryCode::Ru,
            20 => CountryCode::Eg,
            27 => CountryCode::Za,
            33 => CountryCode::Fr,
            34 => CountryCode::Es,
            39 => CountryCode::It,
            44 => CountryCode::Uk,
            49 => CountryCode::De,
            52 => CountryCode::Mx,
            55 => CountryCode::Br,
            61 => CountryCode::Au,
            81 => CountryCode::Jp,
            82 => CountryCode::Kr,
            86 => CountryCode::Cn,
            91 => CountryCode::In,
            _ => CountryCode::Unknown,
        }
    }
}

/// Validated phone number.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhoneNumber {
    country_code: CountryCode,
    national_number: String,
}

impl PhoneNumber {
    /// Get country code.
    pub fn country_code(&self) -> CountryCode {
        self.country_code
    }

    /// Get national number.
    pub fn national_number(&self) -> &str {
        &self.national_number
    }

    /// Format in E.164 format.
    pub fn to_e164(&self) -> String {
        format!("+{}{}", self.country_code.value(), self.national_number)
    }

    /// Format with spaces.
    pub fn to_international(&self) -> String {
        let cc = self.country_code.value();
        let nat = &self.national_number;
        let len = nat.len();

        if len <= 4 {
            format!("+{} {}", cc, nat)
        } else if len <= 7 {
            format!("+{} {} {}", cc, &nat[..3], &nat[3..])
        } else if len <= 10 {
            format!("+{} {} {} {}", cc, &nat[..3], &nat[3..6], &nat[6..])
        } else {
            format!("+{} {}", cc, nat)
        }
    }

    /// Get total digit count.
    pub fn digit_count(&self) -> usize {
        let cc_digits = if self.country_code.value() >= 100 {
            3
        } else if self.country_code.value() >= 10 {
            2
        } else {
            1
        };
        cc_digits + self.national_number.len()
    }
}

impl fmt::Display for PhoneNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_e164())
    }
}

/// Safe phone operations.
pub struct SafePhone;

impl SafePhone {
    /// Parse phone number from string.
    pub fn parse(input: &str) -> Result<PhoneNumber> {
        let trimmed = input.trim();
        if trimmed.is_empty() {
            return Err(Error::EmptyInput);
        }

        // Extract digits only
        let digits: String = trimmed
            .chars()
            .filter(|c| c.is_ascii_digit())
            .collect();

        if digits.len() < 7 {
            return Err(Error::InvalidFormat("Phone number too short".into()));
        }
        if digits.len() > 15 {
            return Err(Error::InvalidFormat("Phone number too long".into()));
        }

        // Try to parse country code
        let (cc, national_start) = Self::parse_country_code(&digits)?;

        if digits.len() - national_start < 4 {
            return Err(Error::InvalidFormat("National number too short".into()));
        }

        Ok(PhoneNumber {
            country_code: cc,
            national_number: digits[national_start..].to_string(),
        })
    }

    /// Check if valid phone number.
    pub fn is_valid(input: &str) -> bool {
        Self::parse(input).is_ok()
    }

    fn parse_country_code(digits: &str) -> Result<(CountryCode, usize)> {
        // Try 3-digit codes first, then 2, then 1
        for len in [3, 2, 1] {
            if digits.len() >= len {
                if let Ok(value) = digits[..len].parse::<u16>() {
                    let cc = CountryCode::from_value(value);
                    if cc != CountryCode::Unknown {
                        return Ok((cc, len));
                    }
                }
            }
        }
        Err(Error::InvalidFormat("Unknown country code".into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_valid() {
        let phone = SafePhone::parse("+1 555 123 4567").unwrap();
        assert_eq!(phone.country_code(), CountryCode::Us);
        assert_eq!(phone.national_number(), "5551234567");
    }

    #[test]
    fn test_format_e164() {
        let phone = SafePhone::parse("+1 555 123 4567").unwrap();
        assert_eq!(phone.to_e164(), "+15551234567");
    }

    #[test]
    fn test_invalid() {
        assert!(SafePhone::parse("123").is_err());
        assert!(SafePhone::parse("").is_err());
    }
}
