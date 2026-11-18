-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeDateTimeProps

import Proven.Core
import Proven.SafeDateTime
import Data.Maybe
import System

%default total

-- Real API: `makeDate : Nat -> Nat -> Nat -> Maybe Date`,
-- `makeTime : Nat -> Nat -> Nat -> Maybe Time`, `Duration`/`addDuration`,
-- and ISO-style rendering via `Show DateTime`. There is no ISO 8601 parser:
-- `Proven.SafeDateTime.Parse` is a header-only module, so the original
-- `parseISO8601` properties are recorded below as an obligation on any
-- future parser rather than tested.

--------------------------------------------------------------------------------
-- Type-level proofs (closed terms that reduce, and universal ones)
--------------------------------------------------------------------------------

||| A valid calendar date constructs.
prop_validDateComponents : isJust (makeDate 2024 1 15) = True
prop_validDateComponents = Refl

||| Month 13 is rejected.
prop_invalidMonthFails : makeDate 2024 13 1 = Nothing
prop_invalidMonthFails = Refl

||| Month 0 is rejected for every year and day.
prop_monthZeroFails : (y, d : Nat) -> makeDate y 0 d = Nothing
prop_monthZeroFails y d = Refl

||| Month 13 is rejected for every year and day.
prop_monthThirteenFails : (y, d : Nat) -> makeDate y 13 d = Nothing
prop_monthThirteenFails y d = Refl

||| January 32 is rejected.
prop_invalidDayFails : makeDate 2024 1 32 = Nothing
prop_invalidDayFails = Refl

||| A valid time of day constructs.
prop_validTimeComponents : isJust (makeTime 10 30 45) = True
prop_validTimeComponents = Refl

||| Hour 24 is rejected for every minute and second.
prop_invalidHourFails : (m, s : Nat) -> makeTime 24 m s = Nothing
prop_invalidHourFails m s = Refl

||| Minute 60 is rejected.
prop_invalidMinuteFails : makeTime 12 60 0 = Nothing
prop_invalidMinuteFails = Refl

||| Second 60 is rejected (no leap-second support).
prop_invalidSecondFails : makeTime 12 30 60 = Nothing
prop_invalidSecondFails = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: ISO 8601 parse/format round-trip and rejection of junk.
||| Blocked by: no parser exists in the exported API (Proven.SafeDateTime.Parse
||| is header-only). This names the specification any future parser `parse`
||| must meet against the existing `Show DateTime` renderer; it is not a witness.
public export
ISO8601ParserObligation : (parse : String -> Maybe DateTime) -> Type
ISO8601ParserObligation parse =
  ((dt : DateTime) -> parse (show dt) = Just dt, parse "not-a-date" = Nothing)

||| OWED: duration addition is associative.
||| Blocked by: `addDuration` adds the underlying `Integer`s, and Integer `+`
||| is a primitive whose associativity has no definitional proof in Idris2
||| 0.8.0. This names the proposition; it is not a witness.
public export
DurationAssociativityObligation : Type
DurationAssociativityObligation =
  (d1, d2, d3 : Duration) ->
  addDuration d1 (addDuration d2 d3) = addDuration (addDuration d1 d2) d3

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
durationSamples : List Duration
durationSamples =
  [ MkDuration 0, MkDuration 1, MkDuration (-1), seconds 59, minutes 61
  , hours (-25), days 365, MkDuration 9223372036854775807 ]

private
renders : Maybe Date -> Maybe Time -> String -> Bool
renders (Just d) (Just t) expected = show (makeDateTime d t) == expected
renders _ _ _ = False

||| Test runner for datetime properties
export
runDateTimeProperties : IO ()
runDateTimeProperties = do
  putStrLn "SafeDateTime Property Tests"
  putStrLn "==========================="
  check "leap year: 2024-02-29 constructs" (isJust (makeDate 2024 2 29))
  check "non-leap year: 2023-02-29 rejected" (isNothing (makeDate 2023 2 29))
  check "century rule: 1900-02-29 rejected, 2000-02-29 constructs"
    (isNothing (makeDate 1900 2 29) && isJust (makeDate 2000 2 29))
  check "year bounds: year 0 and 10000 rejected"
    (isNothing (makeDate 0 1 1) && isNothing (makeDate 10000 1 1))
  check "30-day months reject day 31"
    (all (\m => isNothing (makeDate 2024 m 31)) [4, 6, 9, 11])
  check "ISO-style rendering of a constructed DateTime"
    (renders (makeDate 2024 1 15) (makeTime 10 30 0) "2024-01-15T10:30:00")
  check "duration addition associative on sample corpus"
    (all (\a => all (\b => all (\c =>
       addDuration a (addDuration b c) == addDuration (addDuration a b) c)
       durationSamples) durationSamples) durationSamples)
  check "duration unit conversions agree"
    (toSeconds (days 1) == 86400 && toSeconds (hours 2) == 7200 &&
     toMilliseconds (seconds 3) == 3000)
  putStrLn ""
