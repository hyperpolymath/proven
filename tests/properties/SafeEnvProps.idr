-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeEnvProps

import Proven.Core
import Proven.SafeEnv
import System

%default total

-- Real API: names are validated by `validateName : String -> EnvResult EnvName`
-- (predicate `isValidEnvName`); lookups are pure over an `EnvLookup`
-- (`String -> Maybe String`): `getEnv`, `getEnvOr`, `requireAll`. The access
-- core (`getEnvPure`, `checkAccess`) is `export`, not `public export`, so
-- lookup results do not reduce here and are checked by execution.

--------------------------------------------------------------------------------
-- Type-level proofs (the name predicate `validateName` is built on)
--------------------------------------------------------------------------------

||| An upper-case name with underscore is valid.
prop_validEnvName : isValidEnvName "MY_VAR" = True
prop_validEnvName = Refl

||| Digits after the first character are allowed.
prop_envWithDigits : isValidEnvName "VAR123" = True
prop_envWithDigits = Refl

||| A leading digit is rejected.
prop_startDigitFails : isValidEnvName "123VAR" = False
prop_startDigitFails = Refl

||| The empty name is rejected.
prop_emptyNameFails : isValidEnvName "" = False
prop_emptyNameFails = Refl

||| A hyphen is not a valid name character.
prop_specialCharsFail : isValidEnvName "MY-VAR" = False
prop_specialCharsFail = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: looking up an unset variable never succeeds.
||| Blocked by: `getEnvPure`/`checkAccess` are `export` (bodies opaque outside
||| Proven.SafeEnv.Access). This names the proposition; it is not a witness.
public export
RequiredEnvFailsObligation : Type
RequiredEnvFailsObligation =
  (lookup : EnvLookup) -> (name : String) -> lookup name = Nothing ->
  isErr (getEnv lookup name) = True

||| OWED: the default is returned for an unset, non-sensitive variable.
||| Blocked by: `getEnvOrPure`/`checkAccess` are `export`. This names the
||| proposition; it is not a witness.
public export
DefaultUsedObligation : Type
DefaultUsedObligation =
  (lookup : EnvLookup) -> (name, def : String) -> lookup name = Nothing ->
  isSensitiveName name = False -> getEnvOr lookup name def = Ok def

||| OWED: value sanitisation strips NUL and newline characters.
||| Blocked by: no value sanitiser is exported by Proven.SafeEnv
||| (`validateValue` only bounds length). This names the specification any
||| future sanitiser `sanitize` must meet; it is not a witness.
public export
EnvValueSanitizerObligation : (sanitize : String -> String) -> Type
EnvValueSanitizerObligation sanitize =
  (s : String) ->
  (elem '\0' (unpack (sanitize s)) = False, elem '\n' (unpack (sanitize s)) = False)

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
env : EnvLookup
env "HOME" = Just "/home/test"
env "PORT" = Just "8080"
env "DATABASE_PASSWORD" = Just "hunter2"
env _ = Nothing

private
okIs : Eq a => a -> EnvResult a -> Bool
okIs x (Ok y) = x == y
okIs _ (Err _) = False

private
errIs : EnvError -> EnvResult a -> Bool
errIs e (Err e') = e == e'
errIs _ (Ok _) = False

||| Test runner for env properties
export
runEnvProperties : IO ()
runEnvProperties = do
  putStrLn "SafeEnv Property Tests"
  putStrLn "======================"
  check "validateName accepts MY_VAR and VAR123"
    (isOk (validateName "MY_VAR") && isOk (validateName "VAR123"))
  check "validateName rejects 123VAR, empty and MY-VAR"
    (isErr (validateName "123VAR") && isErr (validateName "") &&
     isErr (validateName "MY-VAR"))
  check "getEnv returns the value of a set public variable"
    (okIs "/home/test" (getEnv env "HOME"))
  check "getEnv on an unset variable is NotFound"
    (errIs (NotFound "MISSING") (getEnv env "MISSING"))
  check "requireAll fails when any variable is missing"
    (isErr (requireAll env ["HOME", "MISSING"]) && isOk (requireAll env ["HOME", "PORT"]))
  check "getEnvOr uses the default only when unset"
    (okIs "dflt" (getEnvOr env "MISSING" "dflt") && okIs "/home/test" (getEnvOr env "HOME" "dflt"))
  check "sensitive variables are refused under default options"
    (errIs (SensitiveVariable "DATABASE_PASSWORD") (getEnv env "DATABASE_PASSWORD"))
  check "typed getters parse and default"
    (okIs 8080 (getPort env "PORT") && okIs 7 (getIntOr env "MISSING" 7))
  putStrLn ""
