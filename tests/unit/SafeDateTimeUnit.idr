-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeDateTimeUnit

import Proven.Core
import Proven.SafeDateTime

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
runDateTimeUnitTests : IO ()
runDateTimeUnitTests = do
  putStrLn "SafeDateTime Unit Tests"
  putStrLn "======================="

  -- ISO 8601 parsing tests
  putStrLn "\n[ISO 8601 Parsing]"
  assertOk "parseISO8601 \"2025-01-15\"" (parseISO8601 "2025-01-15")
  assertOk "parseISO8601 \"2025-01-15T10:30:00\"" (parseISO8601 "2025-01-15T10:30:00")
  assertOk "parseISO8601 \"2025-01-15T10:30:00Z\"" (parseISO8601 "2025-01-15T10:30:00Z")
  assertOk "parseISO8601 \"2025-01-15T10:30:00+05:30\"" (parseISO8601 "2025-01-15T10:30:00+05:30")
  assertOk "parseISO8601 \"2025-01-15T10:30:00.123Z\"" (parseISO8601 "2025-01-15T10:30:00.123Z")
  assertErr "parseISO8601 invalid" (parseISO8601 "not a date")
  assertErr "parseISO8601 \"2025-13-01\" invalid month" (parseISO8601 "2025-13-01")
  assertErr "parseISO8601 \"2025-02-30\" invalid day" (parseISO8601 "2025-02-30")

  -- Date component extraction tests
  putStrLn "\n[Component Extraction]"
  case parseISO8601 "2025-06-15T14:30:45Z" of
    Ok dt => do
      assertEq "getYear = 2025" 2025 (getYear dt)
      assertEq "getMonth = 6" 6 (getMonth dt)
      assertEq "getDay = 15" 15 (getDay dt)
      assertEq "getHour = 14" 14 (getHour dt)
      assertEq "getMinute = 30" 30 (getMinute dt)
      assertEq "getSecond = 45" 45 (getSecond dt)
    Err _ => putStrLn "  ✗ Failed to parse"

  -- Leap year tests
  putStrLn "\n[Leap Year]"
  assertTrue "2024 is leap year" (isLeapYear 2024)
  assertTrue "2000 is leap year" (isLeapYear 2000)
  assertTrue "2025 is not leap year" (not $ isLeapYear 2025)
  assertTrue "1900 is not leap year" (not $ isLeapYear 1900)
  assertOk "parseISO8601 \"2024-02-29\" leap day" (parseISO8601 "2024-02-29")
  assertErr "parseISO8601 \"2025-02-29\" non-leap" (parseISO8601 "2025-02-29")

  -- Timezone handling tests
  putStrLn "\n[Timezone Handling]"
  assertOk "parseTimezone \"UTC\"" (parseTimezone "UTC")
  assertOk "parseTimezone \"America/New_York\"" (parseTimezone "America/New_York")
  assertOk "parseTimezone \"Europe/London\"" (parseTimezone "Europe/London")
  assertOk "parseTimezone \"Asia/Tokyo\"" (parseTimezone "Asia/Tokyo")
  assertErr "parseTimezone \"Invalid/Zone\"" (parseTimezone "Invalid/Zone")

  -- Duration parsing tests
  putStrLn "\n[Duration Parsing]"
  assertOk "parseDuration \"P1D\"" (parseDuration "P1D")
  assertOk "parseDuration \"PT1H30M\"" (parseDuration "PT1H30M")
  assertOk "parseDuration \"P1Y2M3D\"" (parseDuration "P1Y2M3D")
  assertOk "parseDuration \"PT0S\"" (parseDuration "PT0S")
  assertErr "parseDuration \"invalid\"" (parseDuration "invalid")

  -- Date arithmetic tests
  putStrLn "\n[Date Arithmetic]"
  case parseISO8601 "2025-01-15" of
    Ok dt => do
      assertOk "addDays 10" (addDays dt 10)
      assertOk "addMonths 2" (addMonths dt 2)
      assertOk "addYears 1" (addYears dt 1)
      assertOk "subtractDays 5" (subtractDays dt 5)
    Err _ => putStrLn "  ✗ Failed to parse"

  -- Comparison tests
  putStrLn "\n[Comparison]"
  case (parseISO8601 "2025-01-15", parseISO8601 "2025-06-15") of
    (Ok d1, Ok d2) => do
      assertTrue "Jan < Jun" (compareDateTime d1 d2 == LT)
      assertTrue "Jun > Jan" (compareDateTime d2 d1 == GT)
    _ => putStrLn "  ✗ Failed to parse"
  case (parseISO8601 "2025-01-15", parseISO8601 "2025-01-15") of
    (Ok d1, Ok d2) => assertTrue "same dates equal" (compareDateTime d1 d2 == EQ)
    _ => putStrLn "  ✗ Failed to parse"

  -- Formatting tests
  putStrLn "\n[Formatting]"
  case parseISO8601 "2025-06-15T14:30:00Z" of
    Ok dt => do
      assertTrue "formatISO8601 roundtrips" (contains "2025-06-15" (formatISO8601 dt))
      assertTrue "formatRFC2822 works" (contains "Jun" (formatRFC2822 dt))
    Err _ => putStrLn "  ✗ Failed to parse"

  putStrLn "\n✓ SafeDateTime unit tests complete"
