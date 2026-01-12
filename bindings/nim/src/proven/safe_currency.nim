# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe currency operations with type-safe monetary values.
## Follows ISO 4217 currency codes.

import std/[options, strutils, tables]

type
  CurrencyCode* = enum
    ## ISO 4217 currency codes.
    ccUsd = "USD"   ## US Dollar
    ccEur = "EUR"   ## Euro
    ccGbp = "GBP"   ## British Pound
    ccJpy = "JPY"   ## Japanese Yen
    ccChf = "CHF"   ## Swiss Franc
    ccCad = "CAD"   ## Canadian Dollar
    ccAud = "AUD"   ## Australian Dollar
    ccNzd = "NZD"   ## New Zealand Dollar
    ccCny = "CNY"   ## Chinese Yuan
    ccInr = "INR"   ## Indian Rupee
    ccBrl = "BRL"   ## Brazilian Real
    ccMxn = "MXN"   ## Mexican Peso
    ccKrw = "KRW"   ## South Korean Won
    ccSgd = "SGD"   ## Singapore Dollar
    ccHkd = "HKD"   ## Hong Kong Dollar
    ccSek = "SEK"   ## Swedish Krona
    ccNok = "NOK"   ## Norwegian Krone
    ccDkk = "DKK"   ## Danish Krone
    ccPln = "PLN"   ## Polish Zloty
    ccRub = "RUB"   ## Russian Ruble
    ccZar = "ZAR"   ## South African Rand
    ccTry = "TRY"   ## Turkish Lira
    ccThb = "THB"   ## Thai Baht
    ccMyr = "MYR"   ## Malaysian Ringgit
    ccIdr = "IDR"   ## Indonesian Rupiah
    ccPhp = "PHP"   ## Philippine Peso
    ccVnd = "VND"   ## Vietnamese Dong
    ccAed = "AED"   ## UAE Dirham
    ccSar = "SAR"   ## Saudi Riyal
    ccIls = "ILS"   ## Israeli Shekel
    ccCzk = "CZK"   ## Czech Koruna
    ccHuf = "HUF"   ## Hungarian Forint
    ccRon = "RON"   ## Romanian Leu
    ccBgn = "BGN"   ## Bulgarian Lev
    ccIsk = "ISK"   ## Icelandic Krona
    ccClp = "CLP"   ## Chilean Peso
    ccCop = "COP"   ## Colombian Peso
    ccPen = "PEN"   ## Peruvian Sol
    ccArs = "ARS"   ## Argentine Peso
    ccBtc = "BTC"   ## Bitcoin
    ccEth = "ETH"   ## Ethereum

  Money* = object
    ## Type-safe monetary value stored in minor units.
    minorUnits*: int64
    currency*: CurrencyCode

  CurrencyError* = object of CatchableError
    ## Error raised for currency operations.

  CurrencyMismatchError* = object of CurrencyError
    ## Error raised when currencies do not match.

  DivisionByZeroError* = object of CurrencyError
    ## Error raised for division by zero.

const
  CurrencyDecimals: array[CurrencyCode, uint8] = [
    ccUsd: 2'u8, ccEur: 2, ccGbp: 2, ccJpy: 0, ccChf: 2,
    ccCad: 2, ccAud: 2, ccNzd: 2, ccCny: 2, ccInr: 2,
    ccBrl: 2, ccMxn: 2, ccKrw: 0, ccSgd: 2, ccHkd: 2,
    ccSek: 2, ccNok: 2, ccDkk: 2, ccPln: 2, ccRub: 2,
    ccZar: 2, ccTry: 2, ccThb: 2, ccMyr: 2, ccIdr: 2,
    ccPhp: 2, ccVnd: 0, ccAed: 2, ccSar: 2, ccIls: 2,
    ccCzk: 2, ccHuf: 2, ccRon: 2, ccBgn: 2, ccIsk: 0,
    ccClp: 0, ccCop: 2, ccPen: 2, ccArs: 2, ccBtc: 8,
    ccEth: 8
  ]

  CurrencySymbols: array[CurrencyCode, string] = [
    ccUsd: "$", ccEur: "€", ccGbp: "£", ccJpy: "¥", ccChf: "Fr",
    ccCad: "C$", ccAud: "A$", ccNzd: "NZ$", ccCny: "¥", ccInr: "₹",
    ccBrl: "R$", ccMxn: "MX$", ccKrw: "₩", ccSgd: "S$", ccHkd: "HK$",
    ccSek: "kr", ccNok: "kr", ccDkk: "kr", ccPln: "zł", ccRub: "₽",
    ccZar: "R", ccTry: "₺", ccThb: "฿", ccMyr: "RM", ccIdr: "Rp",
    ccPhp: "₱", ccVnd: "₫", ccAed: "د.إ", ccSar: "﷼", ccIls: "₪",
    ccCzk: "Kč", ccHuf: "Ft", ccRon: "lei", ccBgn: "лв", ccIsk: "kr",
    ccClp: "CLP$", ccCop: "COP$", ccPen: "S/", ccArs: "ARS$", ccBtc: "₿",
    ccEth: "Ξ"
  ]

  CurrencyNames: array[CurrencyCode, string] = [
    ccUsd: "US Dollar", ccEur: "Euro", ccGbp: "British Pound",
    ccJpy: "Japanese Yen", ccChf: "Swiss Franc", ccCad: "Canadian Dollar",
    ccAud: "Australian Dollar", ccNzd: "New Zealand Dollar", ccCny: "Chinese Yuan",
    ccInr: "Indian Rupee", ccBrl: "Brazilian Real", ccMxn: "Mexican Peso",
    ccKrw: "South Korean Won", ccSgd: "Singapore Dollar", ccHkd: "Hong Kong Dollar",
    ccSek: "Swedish Krona", ccNok: "Norwegian Krone", ccDkk: "Danish Krone",
    ccPln: "Polish Zloty", ccRub: "Russian Ruble", ccZar: "South African Rand",
    ccTry: "Turkish Lira", ccThb: "Thai Baht", ccMyr: "Malaysian Ringgit",
    ccIdr: "Indonesian Rupiah", ccPhp: "Philippine Peso", ccVnd: "Vietnamese Dong",
    ccAed: "UAE Dirham", ccSar: "Saudi Riyal", ccIls: "Israeli Shekel",
    ccCzk: "Czech Koruna", ccHuf: "Hungarian Forint", ccRon: "Romanian Leu",
    ccBgn: "Bulgarian Lev", ccIsk: "Icelandic Krona", ccClp: "Chilean Peso",
    ccCop: "Colombian Peso", ccPen: "Peruvian Sol", ccArs: "Argentine Peso",
    ccBtc: "Bitcoin", ccEth: "Ethereum"
  ]

proc decimals*(currency: CurrencyCode): uint8 =
  ## Get number of decimal places for a currency.
  CurrencyDecimals[currency]

proc symbol*(currency: CurrencyCode): string =
  ## Get currency symbol.
  CurrencySymbols[currency]

proc name*(currency: CurrencyCode): string =
  ## Get full currency name.
  CurrencyNames[currency]

proc multiplier(currency: CurrencyCode): int64 =
  ## Get the multiplier to convert major to minor units.
  var mult: int64 = 1
  for i in 0..<currency.decimals:
    mult *= 10
  result = mult

proc parseCurrencyCode*(codeString: string): Option[CurrencyCode] =
  ## Parse currency code from string (case-insensitive).
  let upper = codeString.toUpperAscii
  for code in CurrencyCode:
    if $code == upper:
      return some(code)
  result = none(CurrencyCode)

proc isValidCurrencyCode*(codeString: string): bool =
  ## Check if string is a valid currency code.
  parseCurrencyCode(codeString).isSome

proc fromMajor*(amount: int64, currency: CurrencyCode): Money =
  ## Create Money from major units (e.g., dollars).
  Money(
    minorUnits: amount * currency.multiplier,
    currency: currency
  )

proc fromMinor*(amount: int64, currency: CurrencyCode): Money =
  ## Create Money from minor units (e.g., cents).
  Money(minorUnits: amount, currency: currency)

proc zero*(currency: CurrencyCode): Money =
  ## Create zero amount for a currency.
  Money(minorUnits: 0, currency: currency)

proc major*(money: Money): int64 =
  ## Get major units (truncated).
  money.minorUnits div money.currency.multiplier

proc minor*(money: Money): int64 =
  ## Get minor units.
  money.minorUnits

proc isZero*(money: Money): bool =
  ## Check if amount is zero.
  money.minorUnits == 0

proc isPositive*(money: Money): bool =
  ## Check if amount is positive.
  money.minorUnits > 0

proc isNegative*(money: Money): bool =
  ## Check if amount is negative.
  money.minorUnits < 0

proc abs*(money: Money): Money =
  ## Get absolute value.
  Money(minorUnits: abs(money.minorUnits), currency: money.currency)

proc negate*(money: Money): Money =
  ## Negate the amount.
  Money(minorUnits: -money.minorUnits, currency: money.currency)

proc `-`*(money: Money): Money =
  ## Unary minus operator.
  money.negate

proc add*(a, b: Money): Option[Money] =
  ## Add two monetary values. Returns None if currencies mismatch.
  if a.currency != b.currency:
    return none(Money)
  result = some(Money(
    minorUnits: a.minorUnits + b.minorUnits,
    currency: a.currency
  ))

proc `+`*(a, b: Money): Money {.raises: [CurrencyMismatchError].} =
  ## Add two monetary values. Raises if currencies mismatch.
  if a.currency != b.currency:
    raise newException(CurrencyMismatchError,
      "Cannot add " & $a.currency & " and " & $b.currency)
  Money(minorUnits: a.minorUnits + b.minorUnits, currency: a.currency)

proc sub*(a, b: Money): Option[Money] =
  ## Subtract two monetary values. Returns None if currencies mismatch.
  if a.currency != b.currency:
    return none(Money)
  result = some(Money(
    minorUnits: a.minorUnits - b.minorUnits,
    currency: a.currency
  ))

proc `-`*(a, b: Money): Money {.raises: [CurrencyMismatchError].} =
  ## Subtract two monetary values. Raises if currencies mismatch.
  if a.currency != b.currency:
    raise newException(CurrencyMismatchError,
      "Cannot subtract " & $a.currency & " and " & $b.currency)
  Money(minorUnits: a.minorUnits - b.minorUnits, currency: a.currency)

proc mul*(money: Money, scalar: int64): Money =
  ## Multiply by scalar.
  Money(minorUnits: money.minorUnits * scalar, currency: money.currency)

proc `*`*(money: Money, scalar: int64): Money =
  ## Multiply by scalar operator.
  money.mul(scalar)

proc `*`*(scalar: int64, money: Money): Money =
  ## Multiply by scalar operator (commutative).
  money.mul(scalar)

proc divide*(money: Money, scalar: int64): Option[Money] =
  ## Divide by scalar. Returns None on division by zero.
  if scalar == 0:
    return none(Money)
  result = some(Money(
    minorUnits: money.minorUnits div scalar,
    currency: money.currency
  ))

proc `/`*(money: Money, scalar: int64): Money {.raises: [DivisionByZeroError].} =
  ## Divide by scalar. Raises on division by zero.
  if scalar == 0:
    raise newException(DivisionByZeroError, "Division by zero")
  Money(minorUnits: money.minorUnits div scalar, currency: money.currency)

proc `==`*(a, b: Money): bool =
  ## Check equality.
  a.currency == b.currency and a.minorUnits == b.minorUnits

proc `<`*(a, b: Money): bool {.raises: [CurrencyMismatchError].} =
  ## Compare less than. Raises if currencies mismatch.
  if a.currency != b.currency:
    raise newException(CurrencyMismatchError,
      "Cannot compare " & $a.currency & " and " & $b.currency)
  a.minorUnits < b.minorUnits

proc `<=`*(a, b: Money): bool {.raises: [CurrencyMismatchError].} =
  ## Compare less than or equal. Raises if currencies mismatch.
  if a.currency != b.currency:
    raise newException(CurrencyMismatchError,
      "Cannot compare " & $a.currency & " and " & $b.currency)
  a.minorUnits <= b.minorUnits

proc `$`*(money: Money): string =
  ## Format Money as string with symbol.
  let dec = money.currency.decimals
  let divisor = money.currency.multiplier
  let absUnits = abs(money.minorUnits)
  let majorPart = absUnits div divisor
  let minorPart = absUnits mod divisor
  let sign = if money.minorUnits < 0: "-" else: ""

  if dec == 0:
    result = sign & money.currency.symbol & $majorPart
  else:
    let minorStr = align($minorPart, int(dec), '0')
    result = sign & money.currency.symbol & $majorPart & "." & minorStr

proc formatWithCode*(money: Money): string =
  ## Format Money with currency code instead of symbol.
  let dec = money.currency.decimals
  let divisor = money.currency.multiplier
  let absUnits = abs(money.minorUnits)
  let majorPart = absUnits div divisor
  let minorPart = absUnits mod divisor
  let sign = if money.minorUnits < 0: "-" else: ""

  if dec == 0:
    result = sign & $majorPart & " " & $money.currency
  else:
    let minorStr = align($minorPart, int(dec), '0')
    result = sign & $majorPart & "." & minorStr & " " & $money.currency
