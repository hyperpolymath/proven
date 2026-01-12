-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCurrencyUnit

import Proven.Core
import Proven.SafeCurrency

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

export
runCurrencyUnitTests : IO ()
runCurrencyUnitTests = do
  putStrLn "SafeCurrency Unit Tests"
  putStrLn "======================="

  -- Currency code validation tests
  putStrLn "\n[Currency Code Validation]"
  assertOk "parseCurrency \"USD\" valid" (parseCurrency "USD")
  assertOk "parseCurrency \"EUR\" valid" (parseCurrency "EUR")
  assertOk "parseCurrency \"GBP\" valid" (parseCurrency "GBP")
  assertOk "parseCurrency \"JPY\" valid" (parseCurrency "JPY")
  assertOk "parseCurrency \"usd\" lowercase valid" (parseCurrency "usd")
  assertErr "parseCurrency \"XXX\" invalid code" (parseCurrency "XXX")
  assertErr "parseCurrency \"US\" too short" (parseCurrency "US")
  assertErr "parseCurrency \"USDD\" too long" (parseCurrency "USDD")
  assertErr "parseCurrency \"\" empty" (parseCurrency "")

  -- Money creation tests
  putStrLn "\n[Money Creation]"
  assertOk "money 100 USD creates valid money" (money 100 USD)
  assertOk "money 0 EUR creates zero money" (money 0 EUR)
  assertOk "money negative allowed" (money (-50) GBP)

  -- Minor units tests
  putStrLn "\n[Minor Units]"
  assertEq "getMinorUnits USD = 2" 2 (getMinorUnits USD)
  assertEq "getMinorUnits EUR = 2" 2 (getMinorUnits EUR)
  assertEq "getMinorUnits JPY = 0" 0 (getMinorUnits JPY)
  assertEq "getMinorUnits BHD = 3" 3 (getMinorUnits BHD)

  -- Safe arithmetic tests
  putStrLn "\n[Safe Arithmetic]"
  case (money 100 USD, money 50 USD) of
    (Ok m1, Ok m2) => do
      assertOk "add same currency" (addMoney m1 m2)
      assertOk "subtract same currency" (subtractMoney m1 m2)
      assertOk "multiply by scalar" (multiplyMoney m1 2)
    _ => putStrLn "  ✗ Failed to create money"

  -- Currency mismatch tests
  putStrLn "\n[Currency Mismatch]"
  case (money 100 USD, money 50 EUR) of
    (Ok m1, Ok m2) => do
      assertErr "add different currencies fails" (addMoney m1 m2)
      assertErr "subtract different currencies fails" (subtractMoney m1 m2)
    _ => putStrLn "  ✗ Failed to create money"

  -- Formatting tests
  putStrLn "\n[Formatting]"
  case money 12345 USD of
    Ok m => do
      assertEq "formatMoney USD" "$123.45" (formatMoney m)
      assertEq "formatMoneyISO USD" "USD 123.45" (formatMoneyISO m)
    Err _ => putStrLn "  ✗ Failed to create money"

  case money 1000 JPY of
    Ok m => assertEq "formatMoney JPY no decimals" "¥1000" (formatMoney m)
    Err _ => putStrLn "  ✗ Failed to create JPY money"

  -- Division tests
  putStrLn "\n[Division]"
  case money 100 USD of
    Ok m => do
      assertOk "divide by non-zero" (divideMoney m 2)
      assertErr "divide by zero fails" (divideMoney m 0)
    Err _ => putStrLn "  ✗ Failed to create money"

  -- Allocation tests (banker's rounding)
  putStrLn "\n[Allocation]"
  case money 100 USD of
    Ok m => do
      -- Allocate $1.00 into 3 parts: should be [34, 33, 33] cents
      assertOk "allocate evenly" (allocateMoney m [1, 1, 1])
    Err _ => putStrLn "  ✗ Failed to create money"

  -- Comparison tests
  putStrLn "\n[Comparison]"
  case (money 100 USD, money 200 USD, money 100 USD) of
    (Ok m1, Ok m2, Ok m3) => do
      assertTrue "100 USD < 200 USD" (compareMoney m1 m2 == LT)
      assertTrue "200 USD > 100 USD" (compareMoney m2 m1 == GT)
      assertTrue "100 USD == 100 USD" (compareMoney m1 m3 == EQ)
    _ => putStrLn "  ✗ Failed to create money"

  -- ISO 4217 tests
  putStrLn "\n[ISO 4217]"
  assertEq "getNumericCode USD = 840" 840 (getNumericCode USD)
  assertEq "getNumericCode EUR = 978" 978 (getNumericCode EUR)
  assertEq "getSymbol USD = \"$\"" "$" (getSymbol USD)
  assertEq "getSymbol EUR = \"€\"" "€" (getSymbol EUR)
  assertEq "getSymbol GBP = \"£\"" "£" (getSymbol GBP)

  putStrLn "\n✓ SafeCurrency unit tests complete"
