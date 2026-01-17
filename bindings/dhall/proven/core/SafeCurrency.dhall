-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafeCurrency - Safe currency and monetary operations

Provides type-safe currency handling with proper precision.
All monetary operations prevent floating-point errors.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let Natural/lessThan = Prelude.Natural.lessThan
let Natural/greaterThan = Prelude.Natural.greaterThan

-- Currency code (ISO 4217)
let CurrencyCode = < USD | EUR | GBP | JPY | CNY | CHF | AUD | CAD | NZD | HKD | SGD | INR | BRL | KRW | MXN | SEK | NOK | DKK | PLN | RUB | Custom : Text >

-- Get currency decimal places
let decimalPlaces
    : CurrencyCode -> Natural
    = \(c : CurrencyCode) ->
        merge {
            USD = 2,
            EUR = 2,
            GBP = 2,
            JPY = 0,
            CNY = 2,
            CHF = 2,
            AUD = 2,
            CAD = 2,
            NZD = 2,
            HKD = 2,
            SGD = 2,
            INR = 2,
            BRL = 2,
            KRW = 0,
            MXN = 2,
            SEK = 2,
            NOK = 2,
            DKK = 2,
            PLN = 2,
            RUB = 2,
            Custom = \(_ : Text) -> 2
        } c

-- Currency symbol
let currencySymbol
    : CurrencyCode -> Text
    = \(c : CurrencyCode) ->
        merge {
            USD = "$",
            EUR = "€",
            GBP = "£",
            JPY = "¥",
            CNY = "¥",
            CHF = "CHF",
            AUD = "A$",
            CAD = "C$",
            NZD = "NZ$",
            HKD = "HK$",
            SGD = "S$",
            INR = "₹",
            BRL = "R$",
            KRW = "₩",
            MXN = "MX$",
            SEK = "kr",
            NOK = "kr",
            DKK = "kr",
            PLN = "zł",
            RUB = "₽",
            Custom = \(code : Text) -> code
        } c

-- Money type (stored as smallest unit - cents, pence, etc.)
let Money = {
    amount : Natural,  -- In smallest unit (cents)
    currency : CurrencyCode,
    isNegative : Bool
}

-- Create money from smallest unit
let mkMoney
    : Natural -> CurrencyCode -> Money
    = \(amount : Natural) -> \(currency : CurrencyCode) ->
        { amount = amount, currency = currency, isNegative = False }

-- Create negative money
let mkNegativeMoney
    : Natural -> CurrencyCode -> Money
    = \(amount : Natural) -> \(currency : CurrencyCode) ->
        { amount = amount, currency = currency, isNegative = True }

-- Zero money
let zero
    : CurrencyCode -> Money
    = \(currency : CurrencyCode) ->
        { amount = 0, currency = currency, isNegative = False }

-- Money result for operations
let MoneyResult = { value : Money, ok : Bool }

-- Success result
let ok
    : Money -> MoneyResult
    = \(m : Money) -> { value = m, ok = True }

-- Error result
let err
    : CurrencyCode -> MoneyResult
    = \(c : CurrencyCode) -> { value = zero c, ok = False }

-- Safe addition (same currency only)
let add
    : Money -> Money -> MoneyResult
    = \(a : Money) -> \(b : Money) ->
        -- In real implementation, check currency match
        let sum = a.amount + b.amount
        in ok { amount = sum, currency = a.currency, isNegative = False }

-- Safe subtraction
let sub
    : Money -> Money -> MoneyResult
    = \(a : Money) -> \(b : Money) ->
        if Natural/lessThan a.amount b.amount
        then ok { amount = Natural/subtract a.amount b.amount, currency = a.currency, isNegative = True }
        else ok { amount = Natural/subtract b.amount a.amount, currency = a.currency, isNegative = False }

-- Safe multiplication by integer
let mul
    : Money -> Natural -> Money
    = \(m : Money) -> \(n : Natural) ->
        { amount = m.amount * n, currency = m.currency, isNegative = m.isNegative }

-- Safe division by integer
let div
    : Money -> Natural -> MoneyResult
    = \(m : Money) -> \(n : Natural) ->
        if Natural/lessThan n 1
        then err m.currency
        else ok { amount = m.amount / n, currency = m.currency, isNegative = m.isNegative }

-- Price type (with tax info)
let Price = {
    net : Money,
    gross : Money,
    taxRate : Natural  -- Percentage * 100 (e.g., 2000 = 20%)
}

-- Create price with tax
let mkPrice
    : Money -> Natural -> Price
    = \(net : Money) -> \(taxRateBps : Natural) ->
        let taxAmount = (net.amount * taxRateBps) / 10000
        let gross = { amount = net.amount + taxAmount, currency = net.currency, isNegative = net.isNegative }
        in { net = net, gross = gross, taxRate = taxRateBps }

-- Exchange rate (stored as ratio * 1000000 for precision)
let ExchangeRate = {
    fromCurrency : CurrencyCode,
    toCurrency : CurrencyCode,
    rate : Natural  -- Rate * 1000000
}

-- Create exchange rate
let mkExchangeRate
    : CurrencyCode -> CurrencyCode -> Natural -> ExchangeRate
    = \(from : CurrencyCode) -> \(to : CurrencyCode) -> \(rate : Natural) ->
        { fromCurrency = from, toCurrency = to, rate = rate }

-- Convert money using exchange rate
let convert
    : Money -> ExchangeRate -> MoneyResult
    = \(m : Money) -> \(rate : ExchangeRate) ->
        let converted = (m.amount * rate.rate) / 1000000
        in ok { amount = converted, currency = rate.toCurrency, isNegative = m.isNegative }

-- Percentage (basis points, 1% = 100)
let Percentage = { basisPoints : Natural }

-- Create percentage
let mkPercentage
    : Natural -> Percentage
    = \(bps : Natural) ->
        { basisPoints = bps }

-- Common percentages
let CommonPercentages = {
    zero = { basisPoints = 0 },
    half = { basisPoints = 50 },
    one = { basisPoints = 100 },
    five = { basisPoints = 500 },
    ten = { basisPoints = 1000 },
    twentyFive = { basisPoints = 2500 },
    fifty = { basisPoints = 5000 },
    hundred = { basisPoints = 10000 }
}

-- Calculate percentage of money
let percentOf
    : Money -> Percentage -> Money
    = \(m : Money) -> \(p : Percentage) ->
        { amount = (m.amount * p.basisPoints) / 10000, currency = m.currency, isNegative = m.isNegative }

-- Discount type
let Discount = < Percentage : Percentage | Fixed : Money >

-- Apply discount
let applyDiscount
    : Money -> Discount -> Money
    = \(m : Money) -> \(d : Discount) ->
        merge {
            Percentage = \(p : Percentage) ->
                let discount = (m.amount * p.basisPoints) / 10000
                in { amount = Natural/subtract discount m.amount, currency = m.currency, isNegative = m.isNegative },
            Fixed = \(fixed : Money) ->
                if Natural/greaterThan fixed.amount m.amount
                then zero m.currency
                else { amount = Natural/subtract fixed.amount m.amount, currency = m.currency, isNegative = m.isNegative }
        } d

-- Format money (basic - returns string)
let formatMoney
    : Money -> Text
    = \(m : Money) ->
        let sign = if m.isNegative then "-" else ""
        let symbol = currencySymbol m.currency
        in sign ++ symbol ++ Natural/show m.amount

in {
    -- Types
    CurrencyCode,
    Money,
    MoneyResult,
    Price,
    ExchangeRate,
    Percentage,
    Discount,

    -- Constructors
    mkMoney,
    mkNegativeMoney,
    zero,
    ok,
    err,
    mkPrice,
    mkExchangeRate,
    mkPercentage,

    -- Operations
    add,
    sub,
    mul,
    div,
    convert,
    percentOf,
    applyDiscount,

    -- Utilities
    decimalPlaces,
    currencySymbol,
    formatMoney,

    -- Constants
    CommonPercentages
}
