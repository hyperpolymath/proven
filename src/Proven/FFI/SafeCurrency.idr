-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeCurrency operations
|||
||| This module exports safe currency/money utilities to the C ABI via Idris2's RefC backend.
||| All functions are proven total and protect against floating-point precision errors.
|||
||| Return conventions:
||| - Money operations → (status: Int, amount: String, currency: String)
|||   - status = 0: Success
|||   - status = 1: Error (e.g., currency mismatch, division by zero)
||| - Currency code → Int (0-10 for known codes, -1 for unknown)
||| - Comparison → Int (-2=error, -1=less, 0=equal, 1=greater)
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Use integer minor units (cents, satoshis) to avoid floating-point errors.
|||           Always validate currency matches before arithmetic operations.
module Proven.FFI.SafeCurrency

import Proven.SafeCurrency
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode CurrencyCode as Int
encodeCurrencyCode : CurrencyCode -> Int
encodeCurrencyCode USD = 0
encodeCurrencyCode EUR = 1
encodeCurrencyCode GBP = 2
encodeCurrencyCode JPY = 3
encodeCurrencyCode CHF = 4
encodeCurrencyCode CAD = 5
encodeCurrencyCode AUD = 6
encodeCurrencyCode CNY = 7
encodeCurrencyCode INR = 8
encodeCurrencyCode BTC = 9
encodeCurrencyCode ETH = 10
encodeCurrencyCode (Other _) = (-1)

||| Decode Int to CurrencyCode
decodeCurrencyCode : Int -> Maybe CurrencyCode
decodeCurrencyCode 0 = Just USD
decodeCurrencyCode 1 = Just EUR
decodeCurrencyCode 2 = Just GBP
decodeCurrencyCode 3 = Just JPY
decodeCurrencyCode 4 = Just CHF
decodeCurrencyCode 5 = Just CAD
decodeCurrencyCode 6 = Just AUD
decodeCurrencyCode 7 = Just CNY
decodeCurrencyCode 8 = Just INR
decodeCurrencyCode 9 = Just BTC
decodeCurrencyCode 10 = Just ETH
decodeCurrencyCode _ = Nothing

||| Encode Money result as (status, amount, currency)
encodeMoneyResult : Maybe Money -> (Int, String, Int)
encodeMoneyResult Nothing = (1, "0", (-1))  -- Error
encodeMoneyResult (Just m) = (0, show m.amount, encodeCurrencyCode m.currency)

||| Encode Ordering as Int
encodeOrdering : Ordering -> Int
encodeOrdering LT = (-1)
encodeOrdering EQ = 0
encodeOrdering GT = 1

--------------------------------------------------------------------------------
-- Currency Code Operations
--------------------------------------------------------------------------------

%export
proven_idris_currency_minor_units : Int -> Int
proven_idris_currency_minor_units currCode =
  case decodeCurrencyCode currCode of
    Nothing => 0
    Just curr => cast (minorUnits curr)

%export
proven_idris_currency_parse_code : String -> Int
proven_idris_currency_parse_code s =
  case toUpper s of
    "USD" => 0
    "EUR" => 1
    "GBP" => 2
    "JPY" => 3
    "CHF" => 4
    "CAD" => 5
    "AUD" => 6
    "CNY" => 7
    "INR" => 8
    "BTC" => 9
    "ETH" => 10
    _ => (-1)

%export
proven_idris_currency_format_code : Int -> String
proven_idris_currency_format_code currCode =
  case decodeCurrencyCode currCode of
    Nothing => "UNKNOWN"
    Just curr => show curr

--------------------------------------------------------------------------------
-- Money Creation
--------------------------------------------------------------------------------

%export
proven_idris_currency_from_minor : Int -> Int -> (Int, String, Int)
proven_idris_currency_from_minor amount currCode =
  case decodeCurrencyCode currCode of
    Nothing => (1, "0", (-1))
    Just curr => (0, show amount, currCode)

%export
proven_idris_currency_zero : Int -> (Int, String, Int)
proven_idris_currency_zero currCode =
  case decodeCurrencyCode currCode of
    Nothing => (1, "0", (-1))
    Just curr =>
      let m = zero curr
      in (0, show m.amount, encodeCurrencyCode m.currency)

--------------------------------------------------------------------------------
-- Safe Arithmetic
--------------------------------------------------------------------------------

%export
proven_idris_currency_add : Int -> Int -> Int -> Int -> (Int, String, Int)
proven_idris_currency_add amount1 curr1 amount2 curr2 =
  case (decodeCurrencyCode curr1, decodeCurrencyCode curr2) of
    (Just c1, Just c2) =>
      let m1 = MkMoney (cast amount1) c1
          m2 = MkMoney (cast amount2) c2
      in encodeMoneyResult (add m1 m2)
    _ => (1, "0", (-1))

%export
proven_idris_currency_subtract : Int -> Int -> Int -> Int -> (Int, String, Int)
proven_idris_currency_subtract amount1 curr1 amount2 curr2 =
  case (decodeCurrencyCode curr1, decodeCurrencyCode curr2) of
    (Just c1, Just c2) =>
      let m1 = MkMoney (cast amount1) c1
          m2 = MkMoney (cast amount2) c2
      in encodeMoneyResult (subtract m1 m2)
    _ => (1, "0", (-1))

%export
proven_idris_currency_negate : Int -> Int -> (Int, String, Int)
proven_idris_currency_negate amount currCode =
  case decodeCurrencyCode currCode of
    Nothing => (1, "0", (-1))
    Just curr =>
      let m = MkMoney (cast amount) curr
          negated = negate m
      in (0, show negated.amount, encodeCurrencyCode negated.currency)

%export
proven_idris_currency_abs : Int -> Int -> (Int, String, Int)
proven_idris_currency_abs amount currCode =
  case decodeCurrencyCode currCode of
    Nothing => (1, "0", (-1))
    Just curr =>
      let m = MkMoney (cast amount) curr
          absolute = abs m
      in (0, show absolute.amount, encodeCurrencyCode absolute.currency)

--------------------------------------------------------------------------------
-- Comparisons
--------------------------------------------------------------------------------

%export
proven_idris_currency_compare : Int -> Int -> Int -> Int -> Int
proven_idris_currency_compare amount1 curr1 amount2 curr2 =
  case (decodeCurrencyCode curr1, decodeCurrencyCode curr2) of
    (Just c1, Just c2) =>
      let m1 = MkMoney (cast amount1) c1
          m2 = MkMoney (cast amount2) c2
      in case compare m1 m2 of
           Just ordering => encodeOrdering ordering
           Nothing => (-2)  -- Currency mismatch
    _ => (-2)

%export
proven_idris_currency_equals : Int -> Int -> Int -> Int -> Int
proven_idris_currency_equals amount1 curr1 amount2 curr2 =
  case (decodeCurrencyCode curr1, decodeCurrencyCode curr2) of
    (Just c1, Just c2) =>
      let m1 = MkMoney (cast amount1) c1
          m2 = MkMoney (cast amount2) c2
      in encodeBool (m1 == m2)
    _ => 0

%export
proven_idris_currency_is_positive : Int -> Int -> Int
proven_idris_currency_is_positive amount currCode =
  case decodeCurrencyCode currCode of
    Nothing => 0
    Just curr =>
      let m = MkMoney (cast amount) curr
      in encodeBool (isPositive m)

%export
proven_idris_currency_is_negative : Int -> Int -> Int
proven_idris_currency_is_negative amount currCode =
  case decodeCurrencyCode currCode of
    Nothing => 0
    Just curr =>
      let m = MkMoney (cast amount) curr
      in encodeBool (isNegative m)

%export
proven_idris_currency_is_zero : Int -> Int -> Int
proven_idris_currency_is_zero amount currCode =
  case decodeCurrencyCode currCode of
    Nothing => 0
    Just curr =>
      let m = MkMoney (cast amount) curr
      in encodeBool (isZero m)

--------------------------------------------------------------------------------
-- Currency Validation
--------------------------------------------------------------------------------

%export
proven_idris_currency_is_same_currency : Int -> Int -> Int
proven_idris_currency_is_same_currency curr1 curr2 =
  encodeBool (curr1 == curr2)

%export
proven_idris_currency_is_valid_code : Int -> Int
proven_idris_currency_is_valid_code currCode =
  case decodeCurrencyCode currCode of
    Nothing => 0
    Just _ => 1

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_currency_friendly_error : String -> String
proven_idris_currency_friendly_error errorMsg =
  if isInfixOf "currency" (toLower errorMsg)
    then "Currency mismatch (cannot perform operation on different currencies)"
  else if isInfixOf "division" (toLower errorMsg) || isInfixOf "divide" (toLower errorMsg)
    then "Division by zero"
  else
    "Currency operation error"
