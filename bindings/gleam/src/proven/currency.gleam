// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeCurrency - Currency operations that cannot crash.
////
//// Thin FFI wrapper over libproven currency_* and money_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// ISO 4217 currency code (opaque integer matching CurrencyCode enum).
pub type CurrencyCode =
  Int

/// Type-safe monetary value stored in minor units.
pub type Money {
  Money(minor_units: Int, currency: CurrencyCode)
}

/// Parse a currency code from string (e.g., "USD", "EUR").
@external(erlang, "proven_nif", "currency_parse_code")
pub fn parse_code(input: String) -> Result(CurrencyCode, String)

/// Check if a string is a valid currency code.
@external(erlang, "proven_nif", "currency_is_valid_code")
pub fn is_valid_code(input: String) -> Bool

/// Get the number of decimal places for a currency.
@external(erlang, "proven_nif", "currency_get_decimals")
pub fn get_decimals(code: CurrencyCode) -> Int

/// Get the currency symbol (e.g., "$", "E").
@external(erlang, "proven_nif", "currency_get_symbol")
pub fn get_symbol(code: CurrencyCode) -> Result(String, String)

/// Get the full currency name (e.g., "US Dollar").
@external(erlang, "proven_nif", "currency_get_name")
pub fn get_name(code: CurrencyCode) -> Result(String, String)

/// Create money from major units (dollars, euros, etc.).
@external(erlang, "proven_nif", "money_from_major")
pub fn from_major(amount: Int, currency: CurrencyCode) -> Money

/// Create money from minor units (cents, satoshis, etc.).
@external(erlang, "proven_nif", "money_from_minor")
pub fn from_minor(amount: Int, currency: CurrencyCode) -> Money

/// Add two money values of the same currency.
/// Returns Error on currency mismatch or overflow.
@external(erlang, "proven_nif", "money_add")
pub fn add(
  minor_a: Int,
  currency_a: CurrencyCode,
  minor_b: Int,
  currency_b: CurrencyCode,
) -> Result(Money, String)

/// Subtract two money values of the same currency.
/// Returns Error on currency mismatch or underflow.
@external(erlang, "proven_nif", "money_sub")
pub fn subtract(
  minor_a: Int,
  currency_a: CurrencyCode,
  minor_b: Int,
  currency_b: CurrencyCode,
) -> Result(Money, String)

/// Multiply money by a scalar.
/// Returns Error on overflow.
@external(erlang, "proven_nif", "money_mul")
pub fn multiply(minor: Int, currency: CurrencyCode, scalar: Int) -> Result(Money, String)

/// Divide money by a scalar.
/// Returns Error on division by zero.
@external(erlang, "proven_nif", "money_div")
pub fn divide(minor: Int, currency: CurrencyCode, divisor: Int) -> Result(Money, String)

/// Format money as a string with symbol (e.g., "$123.45").
@external(erlang, "proven_nif", "money_format")
pub fn format(minor: Int, currency: CurrencyCode) -> Result(String, String)
