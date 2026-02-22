// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe currency operations via libproven FFI.
//!
//! Type-safe monetary values with ISO 4217 currency codes.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// ISO 4217 currency code (3 ASCII characters).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CurrencyCode(pub [u8; 3]);

impl std::fmt::Display for CurrencyCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.0[0] as char, self.0[1] as char, self.0[2] as char
        )
    }
}

/// Monetary value in minor units (e.g., cents).
#[derive(Debug, Clone, Copy)]
pub struct Money {
    /// Amount in minor units (e.g., 12345 = $123.45 for USD).
    pub amount_minor: i64,
    /// ISO 4217 currency code.
    pub currency_code: CurrencyCode,
    /// Number of decimal places for this currency.
    pub decimal_places: u8,
}

/// Safe currency operations.
pub struct SafeCurrency;

impl SafeCurrency {
    /// Parse a currency string (e.g., "USD 123.45" or "123.45 EUR").
    ///
    /// Returns a `Money` value with the amount in minor units.
    pub fn parse(input: &str) -> Result<Money> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer and returns a
        // value-type struct.
        let result = unsafe {
            ffi::proven_currency_parse(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;
        Ok(Money {
            amount_minor: result.amount_minor,
            currency_code: CurrencyCode(result.currency_code),
            decimal_places: result.decimal_places,
        })
    }

    /// Format a monetary value as a string.
    pub fn format(money: &Money) -> Result<String> {
        // SAFETY: proven_currency_format takes value-type arguments;
        // always safe to call.
        let result = unsafe {
            ffi::proven_currency_format(
                money.amount_minor,
                money.currency_code.0,
                money.decimal_places,
            )
        };
        core::string_result_to_result(result)
    }
}
