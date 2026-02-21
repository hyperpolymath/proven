// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe currency operations with type-safe monetary values.
/// All monetary values use integer minor units (cents) to avoid floating-point errors.
module SafeCurrency =
    open System

    /// ISO 4217 currency codes.
    type CurrencyCode =
        | USD   // US Dollar
        | EUR   // Euro
        | GBP   // British Pound
        | JPY   // Japanese Yen
        | CHF   // Swiss Franc
        | CAD   // Canadian Dollar
        | AUD   // Australian Dollar
        | NZD   // New Zealand Dollar
        | CNY   // Chinese Yuan
        | INR   // Indian Rupee
        | BRL   // Brazilian Real
        | MXN   // Mexican Peso
        | KRW   // South Korean Won
        | SGD   // Singapore Dollar
        | HKD   // Hong Kong Dollar
        | SEK   // Swedish Krona
        | NOK   // Norwegian Krone
        | DKK   // Danish Krone
        | PLN   // Polish Zloty
        | RUB   // Russian Ruble
        | ZAR   // South African Rand
        | TRY   // Turkish Lira
        | THB   // Thai Baht
        | MYR   // Malaysian Ringgit
        | IDR   // Indonesian Rupiah
        | PHP   // Philippine Peso
        | VND   // Vietnamese Dong
        | AED   // UAE Dirham
        | SAR   // Saudi Riyal
        | ILS   // Israeli Shekel
        | CZK   // Czech Koruna
        | HUF   // Hungarian Forint
        | RON   // Romanian Leu
        | BGN   // Bulgarian Lev
        | HRK   // Croatian Kuna
        | ISK   // Icelandic Krona
        | CLP   // Chilean Peso
        | COP   // Colombian Peso
        | PEN   // Peruvian Sol
        | ARS   // Argentine Peso
        | BTC   // Bitcoin
        | ETH   // Ethereum

    /// Currency operation errors.
    type CurrencyError =
        | InvalidAmount of string
        | CurrencyMismatch of CurrencyCode * CurrencyCode
        | DivisionByZero
        | Overflow
        | InvalidCurrencyCode of string
        | NegativeAmount

    /// Monetary value in minor units with associated currency code.
    type Money = {
        MinorUnits: int64
        Currency: CurrencyCode
    }

    /// Get the number of decimal places for a currency.
    let getDecimals (currency: CurrencyCode) : int =
        match currency with
        | JPY | KRW | VND -> 0  // Currencies with no decimals
        | BTC -> 8              // Bitcoin uses satoshis
        | ETH -> 8              // Ethereum (capped for practicality)
        | _ -> 2                // Most currencies use 2 decimal places

    /// Get the currency symbol.
    let getSymbol (currency: CurrencyCode) : string =
        match currency with
        | USD -> "$"
        | EUR -> "\u20AC"  // Euro sign
        | GBP -> "\u00A3"  // Pound sign
        | JPY -> "\u00A5"  // Yen sign
        | CHF -> "Fr"
        | CAD -> "C$"
        | AUD -> "A$"
        | NZD -> "NZ$"
        | CNY -> "\u00A5"
        | INR -> "\u20B9"  // Rupee sign
        | BRL -> "R$"
        | MXN -> "Mex$"
        | KRW -> "\u20A9"  // Won sign
        | SGD -> "S$"
        | HKD -> "HK$"
        | SEK -> "kr"
        | NOK -> "kr"
        | DKK -> "kr"
        | PLN -> "z\u0142"
        | RUB -> "\u20BD"  // Ruble sign
        | ZAR -> "R"
        | TRY -> "\u20BA"  // Lira sign
        | THB -> "\u0E3F"  // Baht sign
        | MYR -> "RM"
        | IDR -> "Rp"
        | PHP -> "\u20B1"  // Peso sign
        | VND -> "\u20AB"  // Dong sign
        | AED -> "\u062F.\u0625"
        | SAR -> "\uFDFC"  // Rial sign
        | ILS -> "\u20AA"  // Shekel sign
        | CZK -> "K\u010D"
        | HUF -> "Ft"
        | RON -> "lei"
        | BGN -> "\u043B\u0432"
        | HRK -> "kn"
        | ISK -> "kr"
        | CLP -> "$"
        | COP -> "$"
        | PEN -> "S/"
        | ARS -> "$"
        | BTC -> "\u20BF"  // Bitcoin sign
        | ETH -> "\u039E"  // Xi (Ethereum)

    /// Get the currency name.
    let getName (currency: CurrencyCode) : string =
        match currency with
        | USD -> "US Dollar"
        | EUR -> "Euro"
        | GBP -> "British Pound Sterling"
        | JPY -> "Japanese Yen"
        | CHF -> "Swiss Franc"
        | CAD -> "Canadian Dollar"
        | AUD -> "Australian Dollar"
        | NZD -> "New Zealand Dollar"
        | CNY -> "Chinese Yuan"
        | INR -> "Indian Rupee"
        | BRL -> "Brazilian Real"
        | MXN -> "Mexican Peso"
        | KRW -> "South Korean Won"
        | SGD -> "Singapore Dollar"
        | HKD -> "Hong Kong Dollar"
        | SEK -> "Swedish Krona"
        | NOK -> "Norwegian Krone"
        | DKK -> "Danish Krone"
        | PLN -> "Polish Zloty"
        | RUB -> "Russian Ruble"
        | ZAR -> "South African Rand"
        | TRY -> "Turkish Lira"
        | THB -> "Thai Baht"
        | MYR -> "Malaysian Ringgit"
        | IDR -> "Indonesian Rupiah"
        | PHP -> "Philippine Peso"
        | VND -> "Vietnamese Dong"
        | AED -> "UAE Dirham"
        | SAR -> "Saudi Riyal"
        | ILS -> "Israeli New Shekel"
        | CZK -> "Czech Koruna"
        | HUF -> "Hungarian Forint"
        | RON -> "Romanian Leu"
        | BGN -> "Bulgarian Lev"
        | HRK -> "Croatian Kuna"
        | ISK -> "Icelandic Krona"
        | CLP -> "Chilean Peso"
        | COP -> "Colombian Peso"
        | PEN -> "Peruvian Sol"
        | ARS -> "Argentine Peso"
        | BTC -> "Bitcoin"
        | ETH -> "Ethereum"

    /// Get currency code as string.
    let currencyCodeToString (currency: CurrencyCode) : string =
        match currency with
        | USD -> "USD" | EUR -> "EUR" | GBP -> "GBP" | JPY -> "JPY"
        | CHF -> "CHF" | CAD -> "CAD" | AUD -> "AUD" | NZD -> "NZD"
        | CNY -> "CNY" | INR -> "INR" | BRL -> "BRL" | MXN -> "MXN"
        | KRW -> "KRW" | SGD -> "SGD" | HKD -> "HKD" | SEK -> "SEK"
        | NOK -> "NOK" | DKK -> "DKK" | PLN -> "PLN" | RUB -> "RUB"
        | ZAR -> "ZAR" | TRY -> "TRY" | THB -> "THB" | MYR -> "MYR"
        | IDR -> "IDR" | PHP -> "PHP" | VND -> "VND" | AED -> "AED"
        | SAR -> "SAR" | ILS -> "ILS" | CZK -> "CZK" | HUF -> "HUF"
        | RON -> "RON" | BGN -> "BGN" | HRK -> "HRK" | ISK -> "ISK"
        | CLP -> "CLP" | COP -> "COP" | PEN -> "PEN" | ARS -> "ARS"
        | BTC -> "BTC" | ETH -> "ETH"

    /// Parse currency code from string.
    let parseCurrencyCode (input: string) : Result<CurrencyCode, CurrencyError> =
        match input.ToUpperInvariant().Trim() with
        | "USD" -> Ok USD | "EUR" -> Ok EUR | "GBP" -> Ok GBP | "JPY" -> Ok JPY
        | "CHF" -> Ok CHF | "CAD" -> Ok CAD | "AUD" -> Ok AUD | "NZD" -> Ok NZD
        | "CNY" -> Ok CNY | "INR" -> Ok INR | "BRL" -> Ok BRL | "MXN" -> Ok MXN
        | "KRW" -> Ok KRW | "SGD" -> Ok SGD | "HKD" -> Ok HKD | "SEK" -> Ok SEK
        | "NOK" -> Ok NOK | "DKK" -> Ok DKK | "PLN" -> Ok PLN | "RUB" -> Ok RUB
        | "ZAR" -> Ok ZAR | "TRY" -> Ok TRY | "THB" -> Ok THB | "MYR" -> Ok MYR
        | "IDR" -> Ok IDR | "PHP" -> Ok PHP | "VND" -> Ok VND | "AED" -> Ok AED
        | "SAR" -> Ok SAR | "ILS" -> Ok ILS | "CZK" -> Ok CZK | "HUF" -> Ok HUF
        | "RON" -> Ok RON | "BGN" -> Ok BGN | "HRK" -> Ok HRK | "ISK" -> Ok ISK
        | "CLP" -> Ok CLP | "COP" -> Ok COP | "PEN" -> Ok PEN | "ARS" -> Ok ARS
        | "BTC" -> Ok BTC | "ETH" -> Ok ETH
        | s -> Error(InvalidCurrencyCode s)

    /// Try to parse currency code.
    let tryParseCurrencyCode (input: string) : CurrencyCode option =
        match parseCurrencyCode input with
        | Ok code -> Some code
        | Error _ -> None

    /// Check if string is a valid currency code.
    let isValidCurrencyCode (input: string) : bool =
        (tryParseCurrencyCode input).IsSome

    let private getMultiplier (currency: CurrencyCode) : int64 =
        let decimals = getDecimals currency
        pown 10L decimals

    /// Create money from major units (dollars, euros, etc.).
    let fromMajorUnits (currency: CurrencyCode) (majorUnits: int64) : Money =
        { MinorUnits = majorUnits * getMultiplier currency; Currency = currency }

    /// Create money from minor units (cents, pence, etc.).
    let fromMinorUnits (currency: CurrencyCode) (minorUnits: int64) : Money =
        { MinorUnits = minorUnits; Currency = currency }

    /// Zero amount for a currency.
    let zero (currency: CurrencyCode) : Money =
        { MinorUnits = 0L; Currency = currency }

    /// Get major units (dollars) from money (truncated).
    let getMajorUnits (money: Money) : int64 =
        money.MinorUnits / getMultiplier money.Currency

    /// Get remainder minor units after extracting major units.
    let getRemainderMinorUnits (money: Money) : int64 =
        money.MinorUnits % getMultiplier money.Currency

    /// Add two monetary values (same currency).
    let add (a: Money) (b: Money) : Result<Money, CurrencyError> =
        if a.Currency <> b.Currency then
            Error(CurrencyMismatch(a.Currency, b.Currency))
        else
            try
                Ok { MinorUnits = Checked.(+) a.MinorUnits b.MinorUnits; Currency = a.Currency }
            with
            | :? OverflowException -> Error Overflow

    /// Subtract two monetary values (same currency).
    let subtract (a: Money) (b: Money) : Result<Money, CurrencyError> =
        if a.Currency <> b.Currency then
            Error(CurrencyMismatch(a.Currency, b.Currency))
        else
            try
                Ok { MinorUnits = Checked.(-) a.MinorUnits b.MinorUnits; Currency = a.Currency }
            with
            | :? OverflowException -> Error Overflow

    /// Multiply money by a scalar.
    let multiply (money: Money) (scalar: int64) : Result<Money, CurrencyError> =
        try
            Ok { MinorUnits = Checked.(*) money.MinorUnits scalar; Currency = money.Currency }
        with
        | :? OverflowException -> Error Overflow

    /// Divide money by a scalar (truncates).
    let divide (money: Money) (divisor: int64) : Result<Money, CurrencyError> =
        if divisor = 0L then Error DivisionByZero
        else Ok { MinorUnits = money.MinorUnits / divisor; Currency = money.Currency }

    /// Negate a monetary value.
    let negate (money: Money) : Result<Money, CurrencyError> =
        try
            Ok { MinorUnits = Checked.(~-) money.MinorUnits; Currency = money.Currency }
        with
        | :? OverflowException -> Error Overflow

    /// Absolute value of money.
    let abs (money: Money) : Money =
        { MinorUnits = System.Math.Abs(money.MinorUnits); Currency = money.Currency }

    /// Check if money is zero.
    let isZero (money: Money) : bool = money.MinorUnits = 0L

    /// Check if money is positive.
    let isPositive (money: Money) : bool = money.MinorUnits > 0L

    /// Check if money is negative.
    let isNegative (money: Money) : bool = money.MinorUnits < 0L

    /// Compare two monetary values.
    let compare (a: Money) (b: Money) : Result<int, CurrencyError> =
        if a.Currency <> b.Currency then
            Error(CurrencyMismatch(a.Currency, b.Currency))
        else
            Ok(Operators.compare a.MinorUnits b.MinorUnits)

    /// Check equality of two monetary values.
    let equals (a: Money) (b: Money) : bool =
        a.Currency = b.Currency && a.MinorUnits = b.MinorUnits

    let private padLeft (totalWidth: int) (paddingChar: char) (s: string) : string =
        if s.Length >= totalWidth then s
        else String(paddingChar, totalWidth - s.Length) + s

    /// Format money with symbol (e.g., "$123.45").
    let format (money: Money) : string =
        let decimals = getDecimals money.Currency
        let symbol = getSymbol money.Currency
        let isNeg = money.MinorUnits < 0L
        let absUnits = System.Math.Abs(money.MinorUnits)
        let multiplier = getMultiplier money.Currency
        let major = absUnits / multiplier
        let minor = absUnits % multiplier
        let negSign = if isNeg then "-" else ""
        if decimals = 0 then
            sprintf "%s%s%d" negSign symbol major
        else
            sprintf "%s%s%d.%s" negSign symbol major (padLeft decimals '0' (string minor))

    /// Format money with currency code (e.g., "123.45 USD").
    let formatWithCode (money: Money) : string =
        let decimals = getDecimals money.Currency
        let code = currencyCodeToString money.Currency
        let isNeg = money.MinorUnits < 0L
        let absUnits = System.Math.Abs(money.MinorUnits)
        let multiplier = getMultiplier money.Currency
        let major = absUnits / multiplier
        let minor = absUnits % multiplier
        let negSign = if isNeg then "-" else ""
        if decimals = 0 then
            sprintf "%s%d %s" negSign major code
        else
            sprintf "%s%d.%s %s" negSign major (padLeft decimals '0' (string minor)) code

    /// Exchange rate between two currencies.
    /// Rate is expressed as: 1 unit of 'from' = rate units of 'to'.
    /// Rate is stored with 6 decimal places (1000000 = 1.0).
    type ExchangeRate = {
        FromCurrency: CurrencyCode
        ToCurrency: CurrencyCode
        RateX1M: int64
    }

    /// Create an exchange rate.
    let createExchangeRate (fromCurrency: CurrencyCode) (toCurrency: CurrencyCode) (whole: int64) (decimal: int64) : ExchangeRate =
        { FromCurrency = fromCurrency; ToCurrency = toCurrency; RateX1M = whole * 1000000L + decimal }

    /// Convert money using exchange rate.
    let convert (rate: ExchangeRate) (money: Money) : Result<Money, CurrencyError> =
        if money.Currency <> rate.FromCurrency then
            Error(CurrencyMismatch(money.Currency, rate.FromCurrency))
        else
            let converted = (money.MinorUnits * rate.RateX1M) / 1000000L
            Ok { MinorUnits = converted; Currency = rate.ToCurrency }

    /// Sum a list of money values (must all be same currency).
    let sum (monies: Money list) : Result<Money, CurrencyError> =
        match monies with
        | [] -> Error(InvalidAmount "empty list")
        | head :: tail ->
            tail |> List.fold (fun acc m ->
                match acc with
                | Error e -> Error e
                | Ok total -> add total m
            ) (Ok head)

    /// Allocate money into n equal parts (handles remainder).
    let allocate (money: Money) (parts: int) : Result<Money list, CurrencyError> =
        if parts <= 0 then Error(InvalidAmount "parts must be positive")
        else
            let each = money.MinorUnits / int64 parts
            let remainder = money.MinorUnits % int64 parts
            let result = [
                for i in 0 .. parts - 1 do
                    let extra = if int64 i < remainder then 1L else 0L
                    yield { MinorUnits = each + extra; Currency = money.Currency }
            ]
            Ok result
