-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeDateTimeProps

import Proven.Core
import Proven.SafeDateTime

%default total

||| Property: Valid ISO date parses
prop_validISODate : isOk (parseISO8601 "2024-01-15T10:30:00Z") = True
prop_validISODate = Refl

||| Property: Invalid date fails
prop_invalidDateFails : isErr (parseISO8601 "not-a-date") = True
prop_invalidDateFails = Refl

||| Property: Valid date components construct
prop_validDateComponents : isOk (makeDate 2024 1 15) = True
prop_validDateComponents = Refl

||| Property: Invalid month fails
prop_invalidMonthFails : isErr (makeDate 2024 13 1) = True
prop_invalidMonthFails = Refl

||| Property: Invalid day fails
prop_invalidDayFails : isErr (makeDate 2024 1 32) = True
prop_invalidDayFails = Refl

||| Property: Leap year Feb 29 valid
prop_leapYearValid : isOk (makeDate 2024 2 29) = True
prop_leapYearValid = Refl

||| Property: Non-leap year Feb 29 fails
prop_nonLeapYearFails : isErr (makeDate 2023 2 29) = True
prop_nonLeapYearFails = Refl

||| Property: Valid time components construct
prop_validTimeComponents : isOk (makeTime 10 30 45) = True
prop_validTimeComponents = Refl

||| Property: Invalid hour fails
prop_invalidHourFails : isErr (makeTime 24 0 0) = True
prop_invalidHourFails = Refl

||| Property: Invalid minute fails
prop_invalidMinuteFails : isErr (makeTime 12 60 0) = True
prop_invalidMinuteFails = Refl

||| Property: Invalid second fails
prop_invalidSecondFails : isErr (makeTime 12 30 60) = True
prop_invalidSecondFails = Refl

||| Property: Format and parse roundtrip
prop_formatParseRoundtrip : (dt : ValidDateTime) ->
                            parseISO8601 (formatISO8601 dt) = Ok dt
prop_formatParseRoundtrip dt = ?prop_formatParseRoundtrip_rhs

||| Property: Duration addition is associative
prop_durationAssociative : (d1, d2, d3 : Duration) ->
                           addDuration d1 (addDuration d2 d3) = addDuration (addDuration d1 d2) d3
prop_durationAssociative d1 d2 d3 = ?prop_durationAssociative_rhs

||| Test runner for datetime properties
export
runDateTimeProperties : IO ()
runDateTimeProperties = do
  putStrLn "SafeDateTime Property Tests"
  putStrLn "==========================="
  putStrLn "prop_validISODate: PASS (proven by type)"
  putStrLn "prop_invalidDateFails: PASS (proven by type)"
  putStrLn "prop_validDateComponents: PASS (proven by type)"
  putStrLn "prop_invalidMonthFails: PASS (proven by type)"
  putStrLn "prop_invalidDayFails: PASS (proven by type)"
  putStrLn "prop_leapYearValid: PASS (proven by type)"
  putStrLn "prop_nonLeapYearFails: PASS (proven by type)"
  putStrLn "prop_validTimeComponents: PASS (proven by type)"
  putStrLn "prop_invalidHourFails: PASS (proven by type)"
  putStrLn "prop_invalidMinuteFails: PASS (proven by type)"
  putStrLn "prop_invalidSecondFails: PASS (proven by type)"
  putStrLn ""
