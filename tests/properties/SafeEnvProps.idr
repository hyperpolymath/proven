-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeEnvProps

import Proven.Core
import Proven.SafeEnv

%default total

||| Property: Valid env var name passes
prop_validEnvName : isOk (validateEnvName "MY_VAR") = True
prop_validEnvName = Refl

||| Property: Env var with digits passes
prop_envWithDigits : isOk (validateEnvName "VAR123") = True
prop_envWithDigits = Refl

||| Property: Starting with digit fails
prop_startDigitFails : isErr (validateEnvName "123VAR") = True
prop_startDigitFails = Refl

||| Property: Empty name fails
prop_emptyNameFails : isErr (validateEnvName "") = True
prop_emptyNameFails = Refl

||| Property: Special chars in name fail
prop_specialCharsFail : isErr (validateEnvName "MY-VAR") = True
prop_specialCharsFail = Refl

||| OWED: Value sanitization removes nulls
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_sanitizeRemovesNulls_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_sanitizeRemovesNulls : containsNull (sanitizeEnvValue "test\0value") = False

||| OWED: Value sanitization removes newlines
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_sanitizeRemovesNewlines_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_sanitizeRemovesNewlines : containsNewline (sanitizeEnvValue "test\nvalue") = False

||| OWED: Get env returns type-safe result
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_getEnvTypeSafe_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_getEnvTypeSafe : (name : ValidEnvName) ->
                        Either String String = getEnv name

||| OWED: Required env fails when missing
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_requiredEnvFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_requiredEnvFails : (name : ValidEnvName) -> envNotSet name = True ->
                          isErr (requireEnv name) = True

||| OWED: Default value used when missing
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_defaultUsed_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_defaultUsed : (name : ValidEnvName) -> (def : String) ->
                     envNotSet name = True ->
                     getEnvOr name def = def

||| Test runner for env properties
export
runEnvProperties : IO ()
runEnvProperties = do
  putStrLn "SafeEnv Property Tests"
  putStrLn "======================"
  putStrLn "prop_validEnvName: PASS (proven by type)"
  putStrLn "prop_envWithDigits: PASS (proven by type)"
  putStrLn "prop_startDigitFails: PASS (proven by type)"
  putStrLn "prop_emptyNameFails: PASS (proven by type)"
  putStrLn "prop_specialCharsFail: PASS (proven by type)"
  putStrLn ""
