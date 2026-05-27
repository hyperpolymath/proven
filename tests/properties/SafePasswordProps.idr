-- SPDX-License-Identifier: MPL-2.0
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

||| OWED: Password without uppercase fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_noUppercaseFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_noUppercaseFails : isErr (validatePassword "lowercase123!") = True

||| OWED: Password without lowercase fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_noLowercaseFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_noLowercaseFails : isErr (validatePassword "UPPERCASE123!") = True

||| OWED: Password without digit fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_noDigitFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_noDigitFails : isErr (validatePassword "NoDigitsHere!") = True

||| OWED: Password without special char fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_noSpecialFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_noSpecialFails : isErr (validatePassword "NoSpecial123") = True

||| OWED: Hash is not the original password
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_hashNotOriginal_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_hashNotOriginal : (p : ValidPassword) -> hashPassword p /= getPasswordString p

||| Property: Same password produces same hash (deterministic with same salt)
prop_hashDeterministic : (p : ValidPassword) -> (salt : String) ->
                         hashPasswordWithSalt p salt = hashPasswordWithSalt p salt
prop_hashDeterministic p salt = Refl

||| OWED: Password strength is at least minimum when valid
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_minStrengthValid_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_minStrengthValid : (p : ValidPassword) -> getStrength p >= MinStrength

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
