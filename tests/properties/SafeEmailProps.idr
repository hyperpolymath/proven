-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeEmailProps

import Proven.Core
import Proven.SafeEmail

%default total

||| Property: Simple valid email validates
prop_simpleEmailValid : isOk (validateEmail "user@example.com") = True
prop_simpleEmailValid = Refl

||| Property: Email without @ fails
prop_noAtFails : isErr (validateEmail "userexample.com") = True
prop_noAtFails = Refl

||| Property: Empty email fails
prop_emptyEmailFails : isErr (validateEmail "") = True
prop_emptyEmailFails = Refl

||| Property: Email with subdomain validates
prop_subdomainValid : isOk (validateEmail "user@mail.example.com") = True
prop_subdomainValid = Refl

||| Property: Email with plus addressing validates
prop_plusAddressingValid : isOk (validateEmail "user+tag@example.com") = True
prop_plusAddressingValid = Refl

||| Property: Email with dots in local part validates
prop_dotsInLocalValid : isOk (validateEmail "first.last@example.com") = True
prop_dotsInLocalValid = Refl

||| Property: Email starting with dot fails
prop_startingDotFails : isErr (validateEmail ".user@example.com") = True
prop_startingDotFails = ?prop_startingDotFails_rhs

||| Property: Normalized email is lowercase domain
prop_normalizeLowercase : (e : ValidEmail) ->
                          getDomain (normalizeEmail e) = toLower (getDomain e)
prop_normalizeLowercase e = ?prop_normalizeLowercase_rhs

||| Test runner for email properties
export
runEmailProperties : IO ()
runEmailProperties = do
  putStrLn "SafeEmail Property Tests"
  putStrLn "========================"
  putStrLn "prop_simpleEmailValid: PASS (proven by type)"
  putStrLn "prop_noAtFails: PASS (proven by type)"
  putStrLn "prop_emptyEmailFails: PASS (proven by type)"
  putStrLn "prop_subdomainValid: PASS (proven by type)"
  putStrLn "prop_plusAddressingValid: PASS (proven by type)"
  putStrLn "prop_dotsInLocalValid: PASS (proven by type)"
  putStrLn ""
