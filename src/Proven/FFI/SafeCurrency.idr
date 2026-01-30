-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeCurrency operations (59/75)
module Proven.FFI.SafeCurrency

import Proven.SafeCurrency
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

-- Currency codes: USD=0, EUR=1, GBP=2, JPY=3, CHF=4, CAD=5, AUD=6, CNY=7, INR=8, BTC=9, ETH=10, Other=999
encodeCurrency : CurrencyCode -> Int
encodeCurrency USD = 0
encodeCurrency EUR = 1
encodeCurrency GBP = 2
encodeCurrency JPY = 3
encodeCurrency CHF = 4
encodeCurrency CAD = 5
encodeCurrency AUD = 6
encodeCurrency CNY = 7
encodeCurrency INR = 8
encodeCurrency BTC = 9
encodeCurrency ETH = 10
encodeCurrency (Other _) = 999

-- Rounding modes: RoundUp=0, RoundDown=1, RoundHalfUp=2, RoundHalfDown=3, RoundHalfEven=4
encodeRoundingMode : RoundingMode -> Int
encodeRoundingMode RoundUp = 0
encodeRoundingMode RoundDown = 1
encodeRoundingMode RoundHalfUp = 2
encodeRoundingMode RoundHalfDown = 3
encodeRoundingMode RoundHalfEven = 4

export
proven_idris_currency_minor_units : Int -> Int
proven_idris_currency_minor_units code =
  if code == 3 then 0       -- JPY
  else if code == 9 then 8  -- BTC
  else if code == 10 then 18 -- ETH
  else 2                     -- Most currencies

export
proven_idris_currency_same : Int -> Int -> Int
proven_idris_currency_same c1 c2 = encodeBool (c1 == c2)

export
proven_idris_money_is_positive : Int -> Int
proven_idris_money_is_positive amt = encodeBool (amt > 0)

export
proven_idris_money_is_negative : Int -> Int
proven_idris_money_is_negative amt = encodeBool (amt < 0)

export
proven_idris_money_is_zero : Int -> Int
proven_idris_money_is_zero amt = encodeBool (amt == 0)
