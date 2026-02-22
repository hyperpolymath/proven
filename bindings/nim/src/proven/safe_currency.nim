# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe currency operations with ISO 4217 currency codes.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  CurrencyParseError* = object of CatchableError
    ## Error raised when currency parsing fails.

  Money* = object
    ## Parsed monetary value with currency information.
    ## Stores the amount in minor units (e.g. cents) as returned by libproven.
    amountMinor*: int64
    currencyCode*: string        ## 3-letter ISO 4217 code (e.g. "USD")
    decimalPlaces*: uint8        ## Number of decimal places for this currency

proc parseCurrency*(input: string): Option[Money] =
  ## Parse a currency amount string (e.g. "USD 123.45" or "123.45 EUR")
  ## via libproven.  Returns None if parsing fails.
  if input.len == 0:
    return none(Money)
  let res = provenCurrencyParse(unsafeAddr input[0], csize_t(input.len))
  if res.status != PROVEN_OK:
    return none(Money)
  # Convert the 3-byte currency code array to a string.
  var code = newString(3)
  code[0] = char(res.currency_code[0])
  code[1] = char(res.currency_code[1])
  code[2] = char(res.currency_code[2])
  some(Money(
    amountMinor: res.amount_minor,
    currencyCode: code,
    decimalPlaces: res.decimal_places
  ))

proc parseCurrencyStrict*(input: string): Money {.raises: [CurrencyParseError].} =
  ## Parse a currency amount string.  Raises CurrencyParseError on failure.
  let parsed = parseCurrency(input)
  if parsed.isNone:
    raise newException(CurrencyParseError,
      "Invalid currency string: " & input)
  parsed.get

proc isValidCurrency*(input: string): bool =
  ## Check if a string is a valid currency amount.
  parseCurrency(input).isSome

proc formatCurrency*(money: Money): Option[string] =
  ## Format a Money value as a string via libproven.
  ## Returns None if formatting fails.
  var code: array[3, uint8]
  if money.currencyCode.len >= 3:
    code[0] = uint8(money.currencyCode[0])
    code[1] = uint8(money.currencyCode[1])
    code[2] = uint8(money.currencyCode[2])
  else:
    return none(string)
  let res = provenCurrencyFormat(money.amountMinor, code, money.decimalPlaces)
  if res.status != PROVEN_OK or res.value == nil:
    return none(string)
  result = some($res.value)
  provenFreeString(res.value)

proc `$`*(money: Money): string =
  ## Format Money as a string.  Falls back to a basic representation if
  ## libproven formatting fails.
  let formatted = formatCurrency(money)
  if formatted.isSome:
    formatted.get
  else:
    $money.amountMinor & " " & money.currencyCode

proc fromMinor*(amountMinor: int64, currencyCode: string,
                decimalPlaces: uint8): Money =
  ## Create a Money value from minor units (e.g. cents).
  Money(
    amountMinor: amountMinor,
    currencyCode: currencyCode,
    decimalPlaces: decimalPlaces
  )

proc isZero*(money: Money): bool =
  ## Check if the amount is zero.
  money.amountMinor == 0

proc isPositive*(money: Money): bool =
  ## Check if the amount is positive.
  money.amountMinor > 0

proc isNegative*(money: Money): bool =
  ## Check if the amount is negative.
  money.amountMinor < 0

proc `==`*(a, b: Money): bool =
  ## Check equality of two monetary values.
  a.amountMinor == b.amountMinor and a.currencyCode == b.currencyCode
