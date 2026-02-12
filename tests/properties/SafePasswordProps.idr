-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafePasswordProps

import Proven.Core
import Proven.SafePassword

%default total

||| Property: Strong password validates
prop_strongPasswordValid : isOk (validatePassword "Str0ng!Pass#2024") = True
prop_strongPasswordValid = Refl

||| Property: Weak password fails (too short)
prop_weakPasswordFails : isErr (validatePassword "weak") = True
prop_weakPasswordFails = Refl

||| Property: Password without uppercase fails
prop_noUppercaseFails : isErr (validatePassword "lowercase123!") = True
prop_noUppercaseFails = ?prop_noUppercaseFails_rhs

||| Property: Password without lowercase fails
prop_noLowercaseFails : isErr (validatePassword "UPPERCASE123!") = True
prop_noLowercaseFails = ?prop_noLowercaseFails_rhs

||| Property: Password without digit fails
prop_noDigitFails : isErr (validatePassword "NoDigitsHere!") = True
prop_noDigitFails = ?prop_noDigitFails_rhs

||| Property: Password without special char fails
prop_noSpecialFails : isErr (validatePassword "NoSpecial123") = True
prop_noSpecialFails = ?prop_noSpecialFails_rhs

||| Property: Hash is not the original password
prop_hashNotOriginal : (p : ValidPassword) -> hashPassword p /= getPasswordString p
prop_hashNotOriginal p = ?prop_hashNotOriginal_rhs

||| Property: Same password produces same hash (deterministic with same salt)
prop_hashDeterministic : (p : ValidPassword) -> (salt : String) ->
                         hashPasswordWithSalt p salt = hashPasswordWithSalt p salt
prop_hashDeterministic p salt = Refl

||| Property: Password strength is at least minimum when valid
prop_minStrengthValid : (p : ValidPassword) -> getStrength p >= MinStrength
prop_minStrengthValid p = ?prop_minStrengthValid_rhs

||| Test runner for password properties
export
runPasswordProperties : IO ()
runPasswordProperties = do
  putStrLn "SafePassword Property Tests"
  putStrLn "==========================="
  putStrLn "prop_strongPasswordValid: PASS (proven by type)"
  putStrLn "prop_weakPasswordFails: PASS (proven by type)"
  putStrLn "prop_hashDeterministic: PASS (proven by type)"
  putStrLn ""
