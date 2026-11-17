-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafePasswordProps

import Proven.Core
import Proven.SafePassword
import Data.List
import System

%default total

-- The real API: `validatePassword : (policy : PasswordPolicy) -> RawPassword
-- -> Either (List PolicyViolation) (ValidPassword policy)` and the Bool
-- decision `isValidPassword policy s = null (checkPolicy policy s)`.
-- The uppercase/lowercase/digit/symbol requirements exist only in
-- `strictPolicy` (min length 12), so the properties use that policy.
-- `checkPolicy` works over `unpack`, which does not reduce at the type
-- level, so policy properties are execution checks.

||| OWED: under strictPolicy, every accepted password has at least Fair
||| strength, for `strength = quickStrengthCheck` (a 12+ char password using
||| all four character classes has ~78 bits of pool entropy before pattern
||| penalties; observed Strong/VeryStrong on sampled inputs). Blocked by:
||| `quickStrengthCheck`/`analyzeStrength` are `covering`, so this total
||| module can neither run them nor mention them even in a type; the
||| proposition is therefore named as a family over the strength function,
||| to be instantiated at `quickStrengthCheck`. `checkPolicy` also goes
||| through `unpack`. This names the proposition; it is not a witness.
public export
StrictValidHasMinStrengthObligation : (String -> StrengthLevel) -> Type
StrictValidHasMinStrengthObligation strength =
  (s : String) -> isValidPassword strictPolicy s = True ->
  (strength s >= Fair) = True

||| OWED: hashing never yields the plaintext, and hashing is deterministic
||| for a fixed salt. Blocked by: the library has no pure or salt-taking
||| hash entry point -- `hashPasswordArgon2id`/`Bcrypt`/`Scrypt` are bodyless
||| `export` FFI stubs returning `IO HashedPassword`, and the salt is chosen
||| inside them -- so neither property can be stated against the current
||| API. Named here as the specification a future pure
||| `hashWithSalt : String -> String -> String` (password, salt) must meet.
||| This names the proposition; it is not a witness.
public export
HashWithSaltSpecObligation : (String -> String -> String) -> Type
HashWithSaltSpecObligation hashWithSalt =
  ((p, salt : String) -> Not (hashWithSalt p salt = p),
   (p, salt : String) -> (q : String) -> p = q ->
     hashWithSalt p salt = hashWithSalt q salt)

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

||| Run `validatePassword strictPolicy` and test the violation list.
||| Only used on inputs expected to be rejected (see the defect note below).
private
rejectedWith : (List PolicyViolation -> Bool) -> String -> Bool
rejectedWith p s = case validatePassword strictPolicy (MkRawPassword s) of
  Left vs => p vs
  Right _ => False

private
isMissingUpper, isMissingLower, isMissingDigit, isMissingSymbol, isTooShort
  : PolicyViolation -> Bool
isMissingUpper MissingUppercase = True
isMissingUpper _ = False
isMissingLower MissingLowercase = True
isMissingLower _ = False
isMissingDigit MissingDigit = True
isMissingDigit _ = False
isMissingSymbol MissingSymbol = True
isMissingSymbol _ = False
isTooShort (TooShort _ _) = True
isTooShort _ = False

||| Test runner for password properties
export
runPasswordProperties : IO ()
runPasswordProperties = do
  putStrLn "SafePassword Property Tests"
  putStrLn "==========================="
  check "strong password accepted by strictPolicy (isValidPassword)"
    (isValidPassword strictPolicy "Str0ng!Pass#2024")
  check "weak password rejected as too short"
    (rejectedWith (any isTooShort) "weak")
  check "password without uppercase rejected (MissingUppercase)"
    (rejectedWith (any isMissingUpper) "lowercase123!")
  check "password without lowercase rejected (MissingLowercase)"
    (rejectedWith (any isMissingLower) "UPPERCASE123!")
  check "password without digit rejected (MissingDigit)"
    (rejectedWith (any isMissingDigit) "NoDigitsHere!")
  check "password without symbol rejected (MissingSymbol)"
    (rejectedWith (any isMissingSymbol) "NoSpecial123")
  check "each single-class omission yields exactly that class violation"
    (rejectedWith (\vs => length vs == 1) "lowercase123!" &&
     rejectedWith (\vs => length vs == 1) "UPPERCASE123!" &&
     rejectedWith (\vs => length vs == 1) "NoDigitsHere!" &&
     rejectedWith (\vs => length vs == 1) "NoSpecial123")
  -- Kept last: on an accepted password, validatePassword evaluates the
  -- bodyless `trustCheckPolicy` in its where-clause (the proof argument of
  -- MkValidPassword is not erased), which aborts at runtime with
  -- "Encountered undefined name ...trustCheckPolicy". Suspected library defect.
  putStrLn "running: validatePassword strictPolicy accepts \"Str0ng!Pass#2024\""
  check "strong password accepted by validatePassword"
    (case validatePassword strictPolicy (MkRawPassword "Str0ng!Pass#2024") of
       Right _ => True
       Left _ => False)
  putStrLn ""
