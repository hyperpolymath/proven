// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe currency operations with type-safe monetary values.

use crate::core::{Error, Result};
use std::fmt;
use std::str::FromStr;

/// ISO 4217 currency codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CurrencyCode {
    USD, EUR, GBP, JPY, CHF, CAD, AUD, NZD, CNY, INR,
    BRL, MXN, KRW, SGD, HKD, SEK, NOK, DKK, PLN, RUB,
    ZAR, TRY, THB, MYR, IDR, PHP, VND, AED, SAR, ILS,
    CZK, HUF, RON, BGN, HRK, ISK, CLP, COP, PEN, ARS,
    BTC, ETH,
}

impl CurrencyCode {
    /// Get number of decimal places.
    pub fn decimals(&self) -> u8 {
        match self {
            CurrencyCode::JPY | CurrencyCode::KRW | CurrencyCode::VND => 0,
            CurrencyCode::BTC | CurrencyCode::ETH => 8,
            _ => 2,
        }
    }

    /// Get currency symbol.
    pub fn symbol(&self) -> &'static str {
        match self {
            CurrencyCode::USD => "$",
            CurrencyCode::EUR => "€",
            CurrencyCode::GBP => "£",
            CurrencyCode::JPY | CurrencyCode::CNY => "¥",
            CurrencyCode::CHF => "Fr",
            CurrencyCode::INR => "₹",
            CurrencyCode::KRW => "₩",
            CurrencyCode::RUB => "₽",
            CurrencyCode::BTC => "₿",
            CurrencyCode::ETH => "Ξ",
            _ => "",
        }
    }

    /// Get currency name.
    pub fn name(&self) -> &'static str {
        match self {
            CurrencyCode::USD => "US Dollar",
            CurrencyCode::EUR => "Euro",
            CurrencyCode::GBP => "British Pound",
            CurrencyCode::JPY => "Japanese Yen",
            CurrencyCode::CHF => "Swiss Franc",
            CurrencyCode::BTC => "Bitcoin",
            CurrencyCode::ETH => "Ethereum",
            _ => "Currency",
        }
    }
}

impl FromStr for CurrencyCode {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s.to_uppercase().as_str() {
            "USD" => Ok(CurrencyCode::USD),
            "EUR" => Ok(CurrencyCode::EUR),
            "GBP" => Ok(CurrencyCode::GBP),
            "JPY" => Ok(CurrencyCode::JPY),
            "CHF" => Ok(CurrencyCode::CHF),
            "CAD" => Ok(CurrencyCode::CAD),
            "AUD" => Ok(CurrencyCode::AUD),
            "NZD" => Ok(CurrencyCode::NZD),
            "CNY" => Ok(CurrencyCode::CNY),
            "INR" => Ok(CurrencyCode::INR),
            "BRL" => Ok(CurrencyCode::BRL),
            "MXN" => Ok(CurrencyCode::MXN),
            "KRW" => Ok(CurrencyCode::KRW),
            "BTC" => Ok(CurrencyCode::BTC),
            "ETH" => Ok(CurrencyCode::ETH),
            _ => Err(Error::InvalidFormat(format!("Unknown currency code: {}", s))),
        }
    }
}

impl fmt::Display for CurrencyCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Type-safe monetary value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Money {
    /// Amount in minor units (cents, satoshis, etc.)
    minor_units: i64,
    /// Currency code
    currency: CurrencyCode,
}

impl Money {
    /// Create from major units.
    pub fn from_major(amount: i64, currency: CurrencyCode) -> Self {
        let multiplier = 10_i64.pow(currency.decimals() as u32);
        Money {
            minor_units: amount * multiplier,
            currency,
        }
    }

    /// Create from minor units.
    pub fn from_minor(amount: i64, currency: CurrencyCode) -> Self {
        Money {
            minor_units: amount,
            currency,
        }
    }

    /// Create zero amount.
    pub fn zero(currency: CurrencyCode) -> Self {
        Money {
            minor_units: 0,
            currency,
        }
    }

    /// Get the currency.
    pub fn currency(&self) -> CurrencyCode {
        self.currency
    }

    /// Get major units (truncated).
    pub fn major(&self) -> i64 {
        let divisor = 10_i64.pow(self.currency.decimals() as u32);
        self.minor_units / divisor
    }

    /// Get minor units.
    pub fn minor(&self) -> i64 {
        self.minor_units
    }

    /// Add two monetary values.
    pub fn add(&self, other: &Money) -> Result<Money> {
        if self.currency != other.currency {
            return Err(Error::InvalidFormat("Currency mismatch".into()));
        }
        Ok(Money {
            minor_units: self.minor_units + other.minor_units,
            currency: self.currency,
        })
    }

    /// Subtract two monetary values.
    pub fn sub(&self, other: &Money) -> Result<Money> {
        if self.currency != other.currency {
            return Err(Error::InvalidFormat("Currency mismatch".into()));
        }
        Ok(Money {
            minor_units: self.minor_units - other.minor_units,
            currency: self.currency,
        })
    }

    /// Multiply by scalar.
    pub fn mul(&self, scalar: i64) -> Money {
        Money {
            minor_units: self.minor_units * scalar,
            currency: self.currency,
        }
    }

    /// Divide by scalar.
    pub fn div(&self, scalar: i64) -> Result<Money> {
        if scalar == 0 {
            return Err(Error::DivisionByZero);
        }
        Ok(Money {
            minor_units: self.minor_units / scalar,
            currency: self.currency,
        })
    }

    /// Check if zero.
    pub fn is_zero(&self) -> bool {
        self.minor_units == 0
    }

    /// Check if positive.
    pub fn is_positive(&self) -> bool {
        self.minor_units > 0
    }

    /// Check if negative.
    pub fn is_negative(&self) -> bool {
        self.minor_units < 0
    }

    /// Absolute value.
    pub fn abs(&self) -> Money {
        Money {
            minor_units: self.minor_units.abs(),
            currency: self.currency,
        }
    }
}

impl fmt::Display for Money {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let dec = self.currency.decimals();
        let divisor = 10_i64.pow(dec as u32);
        let abs_units = self.minor_units.abs();
        let major = abs_units / divisor;
        let minor = abs_units % divisor;
        let sign = if self.minor_units < 0 { "-" } else { "" };

        if dec == 0 {
            write!(f, "{}{}{}", sign, self.currency.symbol(), major)
        } else {
            write!(
                f,
                "{}{}{}.{:0width$}",
                sign,
                self.currency.symbol(),
                major,
                minor,
                width = dec as usize
            )
        }
    }
}

/// Safe currency operations.
pub struct SafeCurrency;

impl SafeCurrency {
    /// Parse currency code from string.
    pub fn parse_code(s: &str) -> Result<CurrencyCode> {
        s.parse()
    }

    /// Check if valid currency code.
    pub fn is_valid_code(s: &str) -> bool {
        CurrencyCode::from_str(s).is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_money_basic() {
        let a = Money::from_major(100, CurrencyCode::USD);
        let b = Money::from_major(50, CurrencyCode::USD);
        assert_eq!(a.add(&b).unwrap().minor(), 15000);
        assert_eq!(a.sub(&b).unwrap().minor(), 5000);
    }

    #[test]
    fn test_currency_mismatch() {
        let usd = Money::from_major(100, CurrencyCode::USD);
        let eur = Money::from_major(100, CurrencyCode::EUR);
        assert!(usd.add(&eur).is_err());
    }

    #[test]
    fn test_format() {
        let m = Money::from_minor(12345, CurrencyCode::USD);
        assert_eq!(format!("{}", m), "$123.45");
    }
}
