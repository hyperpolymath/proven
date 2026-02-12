// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeCurrency - Currency operations that cannot crash.
////
//// Provides type-safe monetary values following ISO 4217.

import gleam/int
import gleam/order
import gleam/string

/// ISO 4217 currency codes.
pub type CurrencyCode {
  Usd
  Eur
  Gbp
  Jpy
  Chf
  Cad
  Aud
  Nzd
  Cny
  Inr
  Brl
  Mxn
  Krw
  Sgd
  Hkd
  Sek
  Nok
  Dkk
  Pln
  Rub
  Zar
  Try
  Thb
  Myr
  Idr
  Php
  Vnd
  Aed
  Sar
  Ils
  Czk
  Huf
  Ron
  Bgn
  Hrk
  Isk
  Clp
  Cop
  Pen
  Ars
  Btc
  Eth
}

/// Error types for currency operations.
pub type CurrencyError {
  UnknownCurrencyCode(code: String)
  CurrencyMismatch(expected: CurrencyCode, actual: CurrencyCode)
  DivisionByZero
}

/// Type-safe monetary value.
/// Uses minor units (cents, satoshis, etc.) internally to avoid floating point issues.
pub opaque type Money {
  Money(minor_units: Int, currency: CurrencyCode)
}

/// Get the number of decimal places for a currency.
pub fn decimals(currency: CurrencyCode) -> Int {
  case currency {
    Jpy | Krw | Vnd -> 0
    Btc | Eth -> 8
    _ -> 2
  }
}

/// Get the currency symbol.
pub fn symbol(currency: CurrencyCode) -> String {
  case currency {
    Usd | Cad | Aud | Nzd | Sgd | Hkd | Mxn | Clp | Cop | Ars -> "$"
    Eur -> "E"
    Gbp -> "L"
    Jpy | Cny -> "Y"
    Chf -> "Fr"
    Inr -> "Rs"
    Krw -> "W"
    Rub -> "R"
    Btc -> "B"
    Eth -> "E"
    Brl -> "R$"
    Zar -> "R"
    Try -> "TL"
    Thb -> "B"
    Myr -> "RM"
    Idr -> "Rp"
    Php -> "P"
    Vnd -> "D"
    Aed -> "Dh"
    Sar -> "SR"
    Ils -> "NIS"
    Czk -> "Kc"
    Huf -> "Ft"
    Ron -> "lei"
    Bgn -> "lv"
    Hrk -> "kn"
    Isk -> "kr"
    Sek | Nok | Dkk -> "kr"
    Pln -> "zl"
    Pen -> "S/"
  }
}

/// Get the full currency name.
pub fn name(currency: CurrencyCode) -> String {
  case currency {
    Usd -> "US Dollar"
    Eur -> "Euro"
    Gbp -> "British Pound"
    Jpy -> "Japanese Yen"
    Chf -> "Swiss Franc"
    Cad -> "Canadian Dollar"
    Aud -> "Australian Dollar"
    Nzd -> "New Zealand Dollar"
    Cny -> "Chinese Yuan"
    Inr -> "Indian Rupee"
    Brl -> "Brazilian Real"
    Mxn -> "Mexican Peso"
    Krw -> "South Korean Won"
    Sgd -> "Singapore Dollar"
    Hkd -> "Hong Kong Dollar"
    Sek -> "Swedish Krona"
    Nok -> "Norwegian Krone"
    Dkk -> "Danish Krone"
    Pln -> "Polish Zloty"
    Rub -> "Russian Ruble"
    Zar -> "South African Rand"
    Try -> "Turkish Lira"
    Thb -> "Thai Baht"
    Myr -> "Malaysian Ringgit"
    Idr -> "Indonesian Rupiah"
    Php -> "Philippine Peso"
    Vnd -> "Vietnamese Dong"
    Aed -> "UAE Dirham"
    Sar -> "Saudi Riyal"
    Ils -> "Israeli Shekel"
    Czk -> "Czech Koruna"
    Huf -> "Hungarian Forint"
    Ron -> "Romanian Leu"
    Bgn -> "Bulgarian Lev"
    Hrk -> "Croatian Kuna"
    Isk -> "Icelandic Krona"
    Clp -> "Chilean Peso"
    Cop -> "Colombian Peso"
    Pen -> "Peruvian Sol"
    Ars -> "Argentine Peso"
    Btc -> "Bitcoin"
    Eth -> "Ethereum"
  }
}

/// Parse a currency code from string.
pub fn parse_code(input: String) -> Result(CurrencyCode, CurrencyError) {
  case string.uppercase(input) {
    "USD" -> Ok(Usd)
    "EUR" -> Ok(Eur)
    "GBP" -> Ok(Gbp)
    "JPY" -> Ok(Jpy)
    "CHF" -> Ok(Chf)
    "CAD" -> Ok(Cad)
    "AUD" -> Ok(Aud)
    "NZD" -> Ok(Nzd)
    "CNY" -> Ok(Cny)
    "INR" -> Ok(Inr)
    "BRL" -> Ok(Brl)
    "MXN" -> Ok(Mxn)
    "KRW" -> Ok(Krw)
    "SGD" -> Ok(Sgd)
    "HKD" -> Ok(Hkd)
    "SEK" -> Ok(Sek)
    "NOK" -> Ok(Nok)
    "DKK" -> Ok(Dkk)
    "PLN" -> Ok(Pln)
    "RUB" -> Ok(Rub)
    "ZAR" -> Ok(Zar)
    "TRY" -> Ok(Try)
    "THB" -> Ok(Thb)
    "MYR" -> Ok(Myr)
    "IDR" -> Ok(Idr)
    "PHP" -> Ok(Php)
    "VND" -> Ok(Vnd)
    "AED" -> Ok(Aed)
    "SAR" -> Ok(Sar)
    "ILS" -> Ok(Ils)
    "CZK" -> Ok(Czk)
    "HUF" -> Ok(Huf)
    "RON" -> Ok(Ron)
    "BGN" -> Ok(Bgn)
    "HRK" -> Ok(Hrk)
    "ISK" -> Ok(Isk)
    "CLP" -> Ok(Clp)
    "COP" -> Ok(Cop)
    "PEN" -> Ok(Pen)
    "ARS" -> Ok(Ars)
    "BTC" -> Ok(Btc)
    "ETH" -> Ok(Eth)
    code -> Error(UnknownCurrencyCode(code: code))
  }
}

/// Check if a string is a valid currency code.
pub fn is_valid_code(input: String) -> Bool {
  case parse_code(input) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Convert currency code to string.
pub fn code_to_string(currency: CurrencyCode) -> String {
  case currency {
    Usd -> "USD"
    Eur -> "EUR"
    Gbp -> "GBP"
    Jpy -> "JPY"
    Chf -> "CHF"
    Cad -> "CAD"
    Aud -> "AUD"
    Nzd -> "NZD"
    Cny -> "CNY"
    Inr -> "INR"
    Brl -> "BRL"
    Mxn -> "MXN"
    Krw -> "KRW"
    Sgd -> "SGD"
    Hkd -> "HKD"
    Sek -> "SEK"
    Nok -> "NOK"
    Dkk -> "DKK"
    Pln -> "PLN"
    Rub -> "RUB"
    Zar -> "ZAR"
    Try -> "TRY"
    Thb -> "THB"
    Myr -> "MYR"
    Idr -> "IDR"
    Php -> "PHP"
    Vnd -> "VND"
    Aed -> "AED"
    Sar -> "SAR"
    Ils -> "ILS"
    Czk -> "CZK"
    Huf -> "HUF"
    Ron -> "RON"
    Bgn -> "BGN"
    Hrk -> "HRK"
    Isk -> "ISK"
    Clp -> "CLP"
    Cop -> "COP"
    Pen -> "PEN"
    Ars -> "ARS"
    Btc -> "BTC"
    Eth -> "ETH"
  }
}

/// Create money from major units (dollars, euros, etc.).
pub fn from_major(amount: Int, currency: CurrencyCode) -> Money {
  let multiplier = pow10(decimals(currency))
  Money(minor_units: amount * multiplier, currency: currency)
}

/// Create money from minor units (cents, satoshis, etc.).
pub fn from_minor(amount: Int, currency: CurrencyCode) -> Money {
  Money(minor_units: amount, currency: currency)
}

/// Create zero money for a currency.
pub fn zero(currency: CurrencyCode) -> Money {
  Money(minor_units: 0, currency: currency)
}

/// Get the currency of a money value.
pub fn get_currency(money: Money) -> CurrencyCode {
  money.currency
}

/// Get major units (dollars, euros, etc.) - truncated.
pub fn major(money: Money) -> Int {
  let divisor = pow10(decimals(money.currency))
  money.minor_units / divisor
}

/// Get minor units (cents, satoshis, etc.).
pub fn minor(money: Money) -> Int {
  money.minor_units
}

/// Add two money values of the same currency.
pub fn add(money_a: Money, money_b: Money) -> Result(Money, CurrencyError) {
  case money_a.currency == money_b.currency {
    True ->
      Ok(Money(
        minor_units: money_a.minor_units + money_b.minor_units,
        currency: money_a.currency,
      ))
    False ->
      Error(CurrencyMismatch(
        expected: money_a.currency,
        actual: money_b.currency,
      ))
  }
}

/// Subtract two money values of the same currency.
pub fn subtract(money_a: Money, money_b: Money) -> Result(Money, CurrencyError) {
  case money_a.currency == money_b.currency {
    True ->
      Ok(Money(
        minor_units: money_a.minor_units - money_b.minor_units,
        currency: money_a.currency,
      ))
    False ->
      Error(CurrencyMismatch(
        expected: money_a.currency,
        actual: money_b.currency,
      ))
  }
}

/// Multiply money by a scalar.
pub fn multiply(money: Money, scalar: Int) -> Money {
  Money(minor_units: money.minor_units * scalar, currency: money.currency)
}

/// Divide money by a scalar.
pub fn divide(money: Money, scalar: Int) -> Result(Money, CurrencyError) {
  case scalar {
    0 -> Error(DivisionByZero)
    _ ->
      Ok(Money(
        minor_units: money.minor_units / scalar,
        currency: money.currency,
      ))
  }
}

/// Check if money is zero.
pub fn is_zero(money: Money) -> Bool {
  money.minor_units == 0
}

/// Check if money is positive.
pub fn is_positive(money: Money) -> Bool {
  money.minor_units > 0
}

/// Check if money is negative.
pub fn is_negative(money: Money) -> Bool {
  money.minor_units < 0
}

/// Get absolute value.
pub fn abs(money: Money) -> Money {
  Money(minor_units: int.absolute_value(money.minor_units), currency: money.currency)
}

/// Negate the money value.
pub fn negate(money: Money) -> Money {
  Money(minor_units: -money.minor_units, currency: money.currency)
}

/// Compare two money values of the same currency.
/// Returns Error if currencies don't match.
pub fn compare(
  money_a: Money,
  money_b: Money,
) -> Result(order.Order, CurrencyError) {
  case money_a.currency == money_b.currency {
    True -> Ok(int.compare(money_a.minor_units, money_b.minor_units))
    False ->
      Error(CurrencyMismatch(
        expected: money_a.currency,
        actual: money_b.currency,
      ))
  }
}

/// Format money as a string with symbol.
pub fn format(money: Money) -> String {
  let dec = decimals(money.currency)
  let divisor = pow10(dec)
  let abs_units = int.absolute_value(money.minor_units)
  let major_part = abs_units / divisor
  let minor_part = abs_units % divisor
  let sign = case money.minor_units < 0 {
    True -> "-"
    False -> ""
  }

  case dec {
    0 -> sign <> symbol(money.currency) <> int.to_string(major_part)
    _ ->
      sign
      <> symbol(money.currency)
      <> int.to_string(major_part)
      <> "."
      <> pad_left(int.to_string(minor_part), dec, "0")
  }
}

/// Format money as a plain string without symbol.
pub fn format_plain(money: Money) -> String {
  let dec = decimals(money.currency)
  let divisor = pow10(dec)
  let abs_units = int.absolute_value(money.minor_units)
  let major_part = abs_units / divisor
  let minor_part = abs_units % divisor
  let sign = case money.minor_units < 0 {
    True -> "-"
    False -> ""
  }

  case dec {
    0 -> sign <> int.to_string(major_part)
    _ ->
      sign
      <> int.to_string(major_part)
      <> "."
      <> pad_left(int.to_string(minor_part), dec, "0")
  }
}

// Helper: power of 10
fn pow10(exponent: Int) -> Int {
  pow10_acc(exponent, 1)
}

fn pow10_acc(exponent: Int, accumulated: Int) -> Int {
  case exponent <= 0 {
    True -> accumulated
    False -> pow10_acc(exponent - 1, accumulated * 10)
  }
}

// Helper: pad string on left
fn pad_left(input: String, target_length: Int, pad_char: String) -> String {
  let current_length = string.length(input)
  case current_length >= target_length {
    True -> input
    False -> pad_left(pad_char <> input, target_length, pad_char)
  }
}
