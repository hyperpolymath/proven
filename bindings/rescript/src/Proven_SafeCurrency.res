// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCurrency - Currency and money operations that cannot crash.
 *
 * All monetary values are stored in minor units (cents, pence, etc.) to avoid
 * floating-point precision issues.
 */

/** ISO 4217 currency codes */
type currencyCode =
  | USD
  | EUR
  | GBP
  | JPY
  | CHF
  | CAD
  | AUD
  | NZD
  | CNY
  | HKD
  | SGD
  | SEK
  | NOK
  | DKK
  | KRW
  | INR
  | BRL
  | MXN
  | ZAR
  | RUB
  | TRY
  | PLN
  | THB
  | IDR
  | MYR
  | PHP
  | VND
  | AED
  | SAR
  | ILS
  | TWD
  | CZK
  | HUF
  | CLP
  | COP
  | PEN
  | ARS
  | EGP
  | NGN
  | KES
  | GHS
  | UAH
  | BTC
  | ETH
  | Other(string)

/** Money type with currency and minor units */
type money = {
  currency: currencyCode,
  minorUnits: int,
}

/** Error types for currency operations */
type currencyError =
  | InvalidAmount
  | InvalidCurrencyCode
  | CurrencyMismatch
  | Overflow
  | Underflow
  | DivisionByZero

/** Get the number of decimal places for a currency */
let getDecimalPlaces = (currency: currencyCode): int => {
  switch currency {
  | JPY | KRW | VND | CLP => 0
  | BTC => 8
  | ETH => 18
  | _ => 2
  }
}

/** Parse a currency code string */
let parseCurrencyCode = (code: string): result<currencyCode, currencyError> => {
  let upper = Js.String2.toUpperCase(Js.String2.trim(code))
  switch upper {
  | "USD" => Ok(USD)
  | "EUR" => Ok(EUR)
  | "GBP" => Ok(GBP)
  | "JPY" => Ok(JPY)
  | "CHF" => Ok(CHF)
  | "CAD" => Ok(CAD)
  | "AUD" => Ok(AUD)
  | "NZD" => Ok(NZD)
  | "CNY" => Ok(CNY)
  | "HKD" => Ok(HKD)
  | "SGD" => Ok(SGD)
  | "SEK" => Ok(SEK)
  | "NOK" => Ok(NOK)
  | "DKK" => Ok(DKK)
  | "KRW" => Ok(KRW)
  | "INR" => Ok(INR)
  | "BRL" => Ok(BRL)
  | "MXN" => Ok(MXN)
  | "ZAR" => Ok(ZAR)
  | "RUB" => Ok(RUB)
  | "TRY" => Ok(TRY)
  | "PLN" => Ok(PLN)
  | "THB" => Ok(THB)
  | "IDR" => Ok(IDR)
  | "MYR" => Ok(MYR)
  | "PHP" => Ok(PHP)
  | "VND" => Ok(VND)
  | "AED" => Ok(AED)
  | "SAR" => Ok(SAR)
  | "ILS" => Ok(ILS)
  | "TWD" => Ok(TWD)
  | "CZK" => Ok(CZK)
  | "HUF" => Ok(HUF)
  | "CLP" => Ok(CLP)
  | "COP" => Ok(COP)
  | "PEN" => Ok(PEN)
  | "ARS" => Ok(ARS)
  | "EGP" => Ok(EGP)
  | "NGN" => Ok(NGN)
  | "KES" => Ok(KES)
  | "GHS" => Ok(GHS)
  | "UAH" => Ok(UAH)
  | "BTC" => Ok(BTC)
  | "ETH" => Ok(ETH)
  | s if Js.String2.length(s) >= 3 && Js.String2.length(s) <= 4 => Ok(Other(s))
  | _ => Error(InvalidCurrencyCode)
  }
}

/** Convert currency code to string */
let currencyCodeToString = (currency: currencyCode): string => {
  switch currency {
  | USD => "USD"
  | EUR => "EUR"
  | GBP => "GBP"
  | JPY => "JPY"
  | CHF => "CHF"
  | CAD => "CAD"
  | AUD => "AUD"
  | NZD => "NZD"
  | CNY => "CNY"
  | HKD => "HKD"
  | SGD => "SGD"
  | SEK => "SEK"
  | NOK => "NOK"
  | DKK => "DKK"
  | KRW => "KRW"
  | INR => "INR"
  | BRL => "BRL"
  | MXN => "MXN"
  | ZAR => "ZAR"
  | RUB => "RUB"
  | TRY => "TRY"
  | PLN => "PLN"
  | THB => "THB"
  | IDR => "IDR"
  | MYR => "MYR"
  | PHP => "PHP"
  | VND => "VND"
  | AED => "AED"
  | SAR => "SAR"
  | ILS => "ILS"
  | TWD => "TWD"
  | CZK => "CZK"
  | HUF => "HUF"
  | CLP => "CLP"
  | COP => "COP"
  | PEN => "PEN"
  | ARS => "ARS"
  | EGP => "EGP"
  | NGN => "NGN"
  | KES => "KES"
  | GHS => "GHS"
  | UAH => "UAH"
  | BTC => "BTC"
  | ETH => "ETH"
  | Other(s) => s
  }
}

/** Create money from minor units */
let fromMinorUnits = (currency: currencyCode, minorUnits: int): money => {
  currency: currency,
  minorUnits: minorUnits,
}

/** Create money from major units (dollars, euros, etc.) */
let fromMajorUnits = (currency: currencyCode, majorUnits: float): result<money, currencyError> => {
  let decimalPlaces = getDecimalPlaces(currency)
  let multiplier = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(decimalPlaces))
  let minorUnitsFloat = majorUnits *. multiplier
  let minorUnits = Belt.Float.toInt(Js.Math.round(minorUnitsFloat))

  // Check for overflow
  if minorUnitsFloat > Belt.Int.toFloat(max_int) || minorUnitsFloat < Belt.Int.toFloat(min_int) {
    Error(Overflow)
  } else {
    Ok({currency: currency, minorUnits: minorUnits})
  }
}

/** Get the minor units value */
let toMinorUnits = (money: money): int => money.minorUnits

/** Convert to major units as float */
let toMajorUnits = (money: money): float => {
  let decimalPlaces = getDecimalPlaces(money.currency)
  let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(decimalPlaces))
  Belt.Int.toFloat(money.minorUnits) /. divisor
}

/** Safe addition of money values */
let add = (moneyA: money, moneyB: money): result<money, currencyError> => {
  if moneyA.currency != moneyB.currency {
    Error(CurrencyMismatch)
  } else {
    let result = moneyA.minorUnits + moneyB.minorUnits
    // Check for overflow
    if moneyA.minorUnits > 0 && moneyB.minorUnits > 0 && result < 0 {
      Error(Overflow)
    } else if moneyA.minorUnits < 0 && moneyB.minorUnits < 0 && result > 0 {
      Error(Underflow)
    } else {
      Ok({currency: moneyA.currency, minorUnits: result})
    }
  }
}

/** Safe subtraction of money values */
let subtract = (moneyA: money, moneyB: money): result<money, currencyError> => {
  if moneyA.currency != moneyB.currency {
    Error(CurrencyMismatch)
  } else {
    let result = moneyA.minorUnits - moneyB.minorUnits
    // Check for underflow
    if moneyA.minorUnits > 0 && moneyB.minorUnits < 0 && result < 0 {
      Error(Overflow)
    } else if moneyA.minorUnits < 0 && moneyB.minorUnits > 0 && result > 0 {
      Error(Underflow)
    } else {
      Ok({currency: moneyA.currency, minorUnits: result})
    }
  }
}

/** Safe multiplication by a scalar */
let multiply = (money: money, multiplier: int): result<money, currencyError> => {
  if multiplier == 0 {
    Ok({currency: money.currency, minorUnits: 0})
  } else {
    let result = money.minorUnits * multiplier
    // Check for overflow
    if result / multiplier != money.minorUnits {
      if multiplier > 0 && money.minorUnits > 0 || multiplier < 0 && money.minorUnits < 0 {
        Error(Overflow)
      } else {
        Error(Underflow)
      }
    } else {
      Ok({currency: money.currency, minorUnits: result})
    }
  }
}

/** Safe division by a scalar */
let divide = (money: money, divisor: int): result<money, currencyError> => {
  if divisor == 0 {
    Error(DivisionByZero)
  } else {
    Ok({currency: money.currency, minorUnits: money.minorUnits / divisor})
  }
}

/** Calculate percentage of money */
let percentOf = (money: money, percent: int): result<money, currencyError> => {
  let scaled = money.minorUnits * percent
  // Check for overflow during multiplication
  if percent != 0 && scaled / percent != money.minorUnits {
    Error(Overflow)
  } else {
    Ok({currency: money.currency, minorUnits: scaled / 100})
  }
}

/** Check if money is zero */
let isZero = (money: money): bool => money.minorUnits == 0

/** Check if money is positive */
let isPositive = (money: money): bool => money.minorUnits > 0

/** Check if money is negative */
let isNegative = (money: money): bool => money.minorUnits < 0

/** Negate money value */
let negate = (money: money): result<money, currencyError> => {
  if money.minorUnits == min_int {
    Error(Overflow)
  } else {
    Ok({currency: money.currency, minorUnits: -money.minorUnits})
  }
}

/** Get absolute value */
let abs = (money: money): result<money, currencyError> => {
  if money.minorUnits == min_int {
    Error(Overflow)
  } else if money.minorUnits < 0 {
    Ok({currency: money.currency, minorUnits: -money.minorUnits})
  } else {
    Ok(money)
  }
}

/** Compare two money values */
let compare = (moneyA: money, moneyB: money): result<int, currencyError> => {
  if moneyA.currency != moneyB.currency {
    Error(CurrencyMismatch)
  } else if moneyA.minorUnits < moneyB.minorUnits {
    Ok(-1)
  } else if moneyA.minorUnits > moneyB.minorUnits {
    Ok(1)
  } else {
    Ok(0)
  }
}

/** Check equality of money values */
let equal = (moneyA: money, moneyB: money): result<bool, currencyError> => {
  if moneyA.currency != moneyB.currency {
    Error(CurrencyMismatch)
  } else {
    Ok(moneyA.minorUnits == moneyB.minorUnits)
  }
}

/** Format money as string with currency symbol */
let format = (money: money): string => {
  let decimalPlaces = getDecimalPlaces(money.currency)
  let majorUnits = toMajorUnits(money)
  let absValue = Js.Math.abs_float(majorUnits)
  let sign = if majorUnits < 0.0 {
    "-"
  } else {
    ""
  }

  let formatted = if decimalPlaces == 0 {
    Belt.Float.toString(absValue)
  } else {
    let fixed = Js.Float.toFixedWithPrecision(absValue, ~digits=decimalPlaces)
    fixed
  }

  `${sign}${currencyCodeToString(money.currency)} ${formatted}`
}

/** Format money with symbol prefix */
let formatWithSymbol = (money: money): string => {
  let symbol = switch money.currency {
  | USD | CAD | AUD | NZD | HKD | SGD | MXN | COP => "$"
  | EUR => "\u20AC"
  | GBP => "\u00A3"
  | JPY | CNY => "\u00A5"
  | CHF => "CHF "
  | INR => "\u20B9"
  | BRL => "R$"
  | KRW => "\u20A9"
  | THB => "\u0E3F"
  | RUB => "\u20BD"
  | TRY => "\u20BA"
  | PLN => "z\u0142"
  | ILS => "\u20AA"
  | BTC => "\u20BF"
  | ETH => "\u039E"
  | _ => currencyCodeToString(money.currency) ++ " "
  }

  let decimalPlaces = getDecimalPlaces(money.currency)
  let majorUnits = toMajorUnits(money)
  let absValue = Js.Math.abs_float(majorUnits)
  let sign = if majorUnits < 0.0 {
    "-"
  } else {
    ""
  }

  let formatted = if decimalPlaces == 0 {
    Belt.Float.toString(absValue)
  } else {
    Js.Float.toFixedWithPrecision(absValue, ~digits=decimalPlaces)
  }

  `${sign}${symbol}${formatted}`
}

/** Zero money for a currency */
let zero = (currency: currencyCode): money => {
  currency: currency,
  minorUnits: 0,
}
