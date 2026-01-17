# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeCurrency - Currency handling for Nickel
#
# Provides safe currency operations with precision handling,
# rounding, and ISO 4217 validation.

{
  # Result type
  Result = {
    value | Number,
    ok | Bool,
  },

  # Money representation (uses smallest unit to avoid floating point issues)
  Money = {
    amount_cents | Number,  # Amount in smallest unit (cents, pence, etc.)
    currency_code | String, # ISO 4217 code
    decimal_places | Number,
  },

  # Common currency decimal places
  DECIMAL_PLACES = {
    USD = 2,
    EUR = 2,
    GBP = 2,
    JPY = 0,
    CHF = 2,
    CAD = 2,
    AUD = 2,
    CNY = 2,
    INR = 2,
    BTC = 8,
    ETH = 18,
  },

  # ISO 4217 currency codes (subset of common ones)
  VALID_CURRENCY_CODES = [
    "USD", "EUR", "GBP", "JPY", "CHF", "CAD", "AUD", "NZD",
    "CNY", "INR", "BRL", "MXN", "KRW", "SGD", "HKD", "SEK",
    "NOK", "DKK", "PLN", "ZAR", "RUB", "TRY", "THB", "MYR"
  ],

  # Check if currency code is valid
  is_valid_currency_code
    : String -> Bool
    = fun code =>
      let upper = std.string.uppercase code in
      std.string.length upper == 3 &&
      std.array.any (fun c => c == upper) VALID_CURRENCY_CODES,

  # Get decimal places for currency
  get_decimal_places
    : String -> Number
    = fun code =>
      let upper = std.string.uppercase code in
      if upper == "USD" then 2
      else if upper == "EUR" then 2
      else if upper == "GBP" then 2
      else if upper == "JPY" then 0
      else if upper == "CHF" then 2
      else if upper == "BTC" then 8
      else if upper == "ETH" then 18
      else 2,  # Default

  # Create money from major units (e.g., dollars)
  from_major_units
    : Number -> String -> Money
    = fun amount code =>
      let decimals = get_decimal_places code in
      let multiplier = std.number.pow 10 decimals in
      {
        amount_cents = std.number.truncate (amount * multiplier),
        currency_code = std.string.uppercase code,
        decimal_places = decimals,
      },

  # Create money from minor units (e.g., cents)
  from_minor_units
    : Number -> String -> Money
    = fun cents code =>
      {
        amount_cents = std.number.truncate cents,
        currency_code = std.string.uppercase code,
        decimal_places = get_decimal_places code,
      },

  # Convert money to major units
  to_major_units
    : Money -> Number
    = fun money =>
      let divisor = std.number.pow 10 money.decimal_places in
      money.amount_cents / divisor,

  # Add two money values (same currency)
  add_money
    : Money -> Money -> { value : Money, ok : Bool }
    = fun a b =>
      if a.currency_code != b.currency_code then
        { value = a, ok = false }
      else
        {
          value = {
            amount_cents = a.amount_cents + b.amount_cents,
            currency_code = a.currency_code,
            decimal_places = a.decimal_places,
          },
          ok = true
        },

  # Subtract money (same currency)
  sub_money
    : Money -> Money -> { value : Money, ok : Bool }
    = fun a b =>
      if a.currency_code != b.currency_code then
        { value = a, ok = false }
      else
        {
          value = {
            amount_cents = a.amount_cents - b.amount_cents,
            currency_code = a.currency_code,
            decimal_places = a.decimal_places,
          },
          ok = true
        },

  # Multiply money by scalar
  multiply_money
    : Money -> Number -> Money
    = fun money scalar =>
      {
        amount_cents = std.number.truncate (money.amount_cents * scalar),
        currency_code = money.currency_code,
        decimal_places = money.decimal_places,
      },

  # Check if money is positive
  is_positive
    : Money -> Bool
    = fun money => money.amount_cents > 0,

  # Check if money is negative
  is_negative
    : Money -> Bool
    = fun money => money.amount_cents < 0,

  # Check if money is zero
  is_zero
    : Money -> Bool
    = fun money => money.amount_cents == 0,

  # Compare two money values
  compare_money
    : Money -> Money -> { value : Number, ok : Bool }
    = fun a b =>
      if a.currency_code != b.currency_code then
        { value = 0, ok = false }
      else if a.amount_cents < b.amount_cents then
        { value = -1, ok = true }
      else if a.amount_cents > b.amount_cents then
        { value = 1, ok = true }
      else
        { value = 0, ok = true },

  # Format money as string
  format_money
    : Money -> String
    = fun money =>
      let major = to_major_units money in
      let formatted = std.string.from_number major in
      money.currency_code ++ " " ++ formatted,

  # Currency code contract
  CurrencyCode = std.contract.from_predicate is_valid_currency_code,

  # Positive money contract
  PositiveMoney = std.contract.from_predicate (fun m => m.amount_cents > 0),

  # Non-negative money contract
  NonNegativeMoney = std.contract.from_predicate (fun m => m.amount_cents >= 0),
}
