-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath


module Proven.SafeCurrency exposing
    ( Money, money, fromMoney
    , amount, currency
    , add, subtract, multiply, divide
    , isPositive, isNegative, isZero
    , CurrencyCode(..), currencyCodeToString, currencyCodeFromString
    , format, round_, floor_, ceiling_
    , Error(..)
    )

{-| Safe currency and monetary value handling.

Provides type-safe monetary values with ISO 4217 currency codes.


# Money Type

@docs Money, money, fromMoney


# Accessors

@docs amount, currency


# Arithmetic

@docs add, subtract, multiply, divide


# Predicates

@docs isPositive, isNegative, isZero


# Currency Codes

@docs CurrencyCode, currencyCodeToString, currencyCodeFromString


# Formatting

@docs format, round_, floor_, ceiling_


# Errors

@docs Error

-}


{-| Error types for currency operations.
-}
type Error
    = CurrencyMismatch
    | DivisionByZero
    | InvalidAmount
    | InvalidCurrencyCode



-- ============================================================================
-- CURRENCY CODES
-- ============================================================================


{-| ISO 4217 currency codes.
-}
type CurrencyCode
    = USD
    | EUR
    | GBP
    | JPY
    | CHF
    | CAD
    | AUD
    | CNY
    | INR
    | BRL
    | MXN
    | KRW
    | RUB
    | SEK
    | NOK
    | DKK
    | PLN
    | CZK
    | HUF
    | Other String


{-| Convert currency code to string.
-}
currencyCodeToString : CurrencyCode -> String
currencyCodeToString code =
    case code of
        USD ->
            "USD"

        EUR ->
            "EUR"

        GBP ->
            "GBP"

        JPY ->
            "JPY"

        CHF ->
            "CHF"

        CAD ->
            "CAD"

        AUD ->
            "AUD"

        CNY ->
            "CNY"

        INR ->
            "INR"

        BRL ->
            "BRL"

        MXN ->
            "MXN"

        KRW ->
            "KRW"

        RUB ->
            "RUB"

        SEK ->
            "SEK"

        NOK ->
            "NOK"

        DKK ->
            "DKK"

        PLN ->
            "PLN"

        CZK ->
            "CZK"

        HUF ->
            "HUF"

        Other s ->
            s


{-| Parse currency code from string.
-}
currencyCodeFromString : String -> Maybe CurrencyCode
currencyCodeFromString str =
    case String.toUpper str of
        "USD" ->
            Just USD

        "EUR" ->
            Just EUR

        "GBP" ->
            Just GBP

        "JPY" ->
            Just JPY

        "CHF" ->
            Just CHF

        "CAD" ->
            Just CAD

        "AUD" ->
            Just AUD

        "CNY" ->
            Just CNY

        "INR" ->
            Just INR

        "BRL" ->
            Just BRL

        "MXN" ->
            Just MXN

        "KRW" ->
            Just KRW

        "RUB" ->
            Just RUB

        "SEK" ->
            Just SEK

        "NOK" ->
            Just NOK

        "DKK" ->
            Just DKK

        "PLN" ->
            Just PLN

        "CZK" ->
            Just CZK

        "HUF" ->
            Just HUF

        other ->
            if String.length other == 3 && String.all Char.isAlpha other then
                Just (Other (String.toUpper other))

            else
                Nothing



-- ============================================================================
-- MONEY TYPE
-- ============================================================================


{-| A monetary value with a currency.
-}
type Money
    = Money
        { amount_ : Int -- Stored as minor units (cents)
        , currency_ : CurrencyCode
        }


{-| Create a money value from major units (e.g., dollars).
-}
money : Float -> CurrencyCode -> Money
money majorUnits curr =
    let
        minorUnits =
            Basics.round (majorUnits * 100)
    in
    Money { amount_ = minorUnits, currency_ = curr }


{-| Get the money as (amount in major units, currency code string).
-}
fromMoney : Money -> ( Float, String )
fromMoney (Money m) =
    ( toFloat m.amount_ / 100, currencyCodeToString m.currency_ )



-- ============================================================================
-- ACCESSORS
-- ============================================================================


{-| Get the amount in major units.
-}
amount : Money -> Float
amount (Money m) =
    toFloat m.amount_ / 100


{-| Get the currency code.
-}
currency : Money -> CurrencyCode
currency (Money m) =
    m.currency_



-- ============================================================================
-- ARITHMETIC
-- ============================================================================


{-| Add two money values. Returns Nothing if currencies don't match.
-}
add : Money -> Money -> Maybe Money
add (Money a) (Money b) =
    if currencyCodeToString a.currency_ == currencyCodeToString b.currency_ then
        Just (Money { amount_ = a.amount_ + b.amount_, currency_ = a.currency_ })

    else
        Nothing


{-| Subtract two money values. Returns Nothing if currencies don't match.
-}
subtract : Money -> Money -> Maybe Money
subtract (Money a) (Money b) =
    if currencyCodeToString a.currency_ == currencyCodeToString b.currency_ then
        Just (Money { amount_ = a.amount_ - b.amount_, currency_ = a.currency_ })

    else
        Nothing


{-| Multiply money by a scalar.
-}
multiply : Float -> Money -> Money
multiply factor (Money m) =
    Money { m | amount_ = Basics.round (toFloat m.amount_ * factor) }


{-| Divide money by a scalar. Returns Nothing if divisor is zero.
-}
divide : Float -> Money -> Maybe Money
divide divisor (Money m) =
    if divisor == 0 then
        Nothing

    else
        Just (Money { m | amount_ = Basics.round (toFloat m.amount_ / divisor) })



-- ============================================================================
-- PREDICATES
-- ============================================================================


{-| Check if money amount is positive.
-}
isPositive : Money -> Bool
isPositive (Money m) =
    m.amount_ > 0


{-| Check if money amount is negative.
-}
isNegative : Money -> Bool
isNegative (Money m) =
    m.amount_ < 0


{-| Check if money amount is zero.
-}
isZero : Money -> Bool
isZero (Money m) =
    m.amount_ == 0



-- ============================================================================
-- FORMATTING
-- ============================================================================


{-| Format money as a string with currency symbol.
-}
format : Money -> String
format (Money m) =
    let
        symbol =
            case m.currency_ of
                USD ->
                    "$"

                EUR ->
                    "\u{20AC}"

                GBP ->
                    "\u{00A3}"

                JPY ->
                    "\u{00A5}"

                _ ->
                    currencyCodeToString m.currency_ ++ " "

        absAmount =
            abs m.amount_

        major =
            absAmount // 100

        minor =
            modBy 100 absAmount

        sign =
            if m.amount_ < 0 then
                "-"

            else
                ""
    in
    sign ++ symbol ++ String.fromInt major ++ "." ++ String.padLeft 2 '0' (String.fromInt minor)


{-| Round money to nearest cent.
-}
round_ : Money -> Money
round_ m =
    m


{-| Floor money to lower cent.
-}
floor_ : Money -> Money
floor_ m =
    m


{-| Ceiling money to higher cent.
-}
ceiling_ : Money -> Money
ceiling_ m =
    m
