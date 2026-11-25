-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCurrencyProps

import Proven.Core
import Proven.SafeCurrency
import Data.List
import Data.String
import System

%default total

-- Real API: `CurrencyCode` is a closed enumeration (plus `Other String`),
-- `Money` is a record of an Integer amount in minor units and a currency,
-- `add`/`subtract : Money -> Money -> Maybe Money` refuse mixed currencies,
-- `minorUnits` gives ISO 4217 decimal places and `format` renders a symbol.
--
-- Retired because the API they targeted does not exist (no `parseCurrency`
-- or `parseAmount` anywhere under src/; there is no string parser for
-- currency codes or amounts): prop_validISOCode, prop_lowercaseParsed,
-- prop_invalidCodeFails, prop_validAmountParses, prop_invalidAmountFails.
-- The negative-amount intent survives as prop_negativeAmountRepresentable.

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| Negative amounts are representable (in minor units) and detected.
prop_negativeAmountRepresentable : isNegative (fromMinor (-5000) USD) = True
prop_negativeAmountRepresentable = Refl

||| Addition preserves currency (universal): adding same-currency values
||| yields a value in that currency.
prop_additionPreservesCurrency : (a, b : Money) -> (a.currency == b.currency) = True ->
                                 map (.currency) (add a b) = Just a.currency
prop_additionPreservesCurrency a b same = rewrite same in Refl

||| Addition of same-currency values sums the minor-unit amounts (universal).
prop_additionSumsAmounts : (a, b : Money) -> (a.currency == b.currency) = True ->
                           map (.amount) (add a b) = Just (a.amount + b.amount)
prop_additionSumsAmounts a b same = rewrite same in Refl

||| Different currencies cannot add (universal).
prop_differentCurrenciesCannotAdd : (a, b : Money) -> (a.currency == b.currency) = False ->
                                    add a b = Nothing
prop_differentCurrenciesCannotAdd a b differ = rewrite differ in Refl

||| Concrete instance: USD + EUR is refused.
prop_usdPlusEurRefused : (x, y : Integer) -> add (fromMinor x USD) (fromMinor y EUR) = Nothing
prop_usdPlusEurRefused x y = Refl

||| USD has 2 decimal places (ISO 4217).
prop_correctDecimalPlaces : minorUnits USD = 2
prop_correctDecimalPlaces = Refl

||| JPY has 0 decimal places (ISO 4217).
prop_jpyZeroDecimals : minorUnits JPY = 0
prop_jpyZeroDecimals = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: formatting a USD amount includes the '$' symbol. Blocked by:
||| `format` builds the string with String `++` and `show` on Double, which
||| do not reduce on a variable amount, so `isPrefixOf` cannot be decided
||| symbolically. This names the proposition; it is not a witness.
public export
FormatIncludesSymbolObligation : Type
FormatIncludesSymbolObligation =
  (m : Money) -> m.currency = USD -> isPrefixOf "$" (format m) = True

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
amounts : List Integer
amounts = [-100000, -5000, -1, 0, 1, 12345, 99999999]

||| Test runner for currency properties
export
runCurrencyProperties : IO ()
runCurrencyProperties = do
  putStrLn "SafeCurrency Property Tests"
  putStrLn "==========================="
  putStrLn "prop_negativeAmountRepresentable: PASS (proven by type)"
  putStrLn "prop_additionPreservesCurrency, prop_additionSumsAmounts: PASS (proven by type)"
  putStrLn "prop_differentCurrenciesCannotAdd, prop_usdPlusEurRefused: PASS (proven by type)"
  putStrLn "prop_correctDecimalPlaces, prop_jpyZeroDecimals: PASS (proven by type)"
  check "formatting a USD amount includes the $ symbol"
    (all (\a => isPrefixOf "$" (format (fromMinor a USD))) amounts)
  check "123.45 USD (12345 minor units) converts to 123.45 major units"
    (toMajor (fromMinor 12345 USD) == 123.45)
  check "same-currency add on corpus keeps currency and sums"
    (all (\a => all (\b => add (fromMinor a GBP) (fromMinor b GBP) == Just (fromMinor (a + b) GBP))
                    amounts) amounts)
  check "mixed-currency add and subtract refused on corpus"
    (all (\a => add (fromMinor a USD) (fromMinor a JPY) == Nothing &&
                subtract (fromMinor a USD) (fromMinor a EUR) == Nothing) amounts)
  putStrLn ""
