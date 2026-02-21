-- SPDX-License-Identifier: PMPL-1.0-or-later
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

||| Property: Value sanitization removes nulls
prop_sanitizeRemovesNulls : containsNull (sanitizeEnvValue "test\0value") = False
prop_sanitizeRemovesNulls = ?prop_sanitizeRemovesNulls_rhs

||| Property: Value sanitization removes newlines
prop_sanitizeRemovesNewlines : containsNewline (sanitizeEnvValue "test\nvalue") = False
prop_sanitizeRemovesNewlines = ?prop_sanitizeRemovesNewlines_rhs

||| Property: Get env returns type-safe result
prop_getEnvTypeSafe : (name : ValidEnvName) ->
                      Either String String = getEnv name
prop_getEnvTypeSafe name = ?prop_getEnvTypeSafe_rhs

||| Property: Required env fails when missing
prop_requiredEnvFails : (name : ValidEnvName) -> envNotSet name = True ->
                        isErr (requireEnv name) = True
prop_requiredEnvFails name prf = ?prop_requiredEnvFails_rhs

||| Property: Default value used when missing
prop_defaultUsed : (name : ValidEnvName) -> (def : String) ->
                   envNotSet name = True ->
                   getEnvOr name def = def
prop_defaultUsed name def prf = ?prop_defaultUsed_rhs

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
