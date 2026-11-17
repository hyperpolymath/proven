-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeEmailProps

import Proven.Core
import Proven.SafeEmail
import Data.Maybe
import System

%default total

-- Real API: `validateEmail : String -> Maybe EmailAddress` (not a `Result`),
-- `normalize : EmailAddress -> EmailAddress` (lower-cases the domain) and the
-- accessors `getDomain`/`getLocalPart`. Closed `validateEmail` literals do not
-- reduce at the type level (String splitting in `parseEmail`), so the
-- point cases run as execution checks below.

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| Normalisation lower-cases the domain, for every address.
prop_normalizeLowercase : (e : EmailAddress) ->
                          getDomain (normalize e) = toLower (getDomain e)
prop_normalizeLowercase e = Refl

||| Normalisation leaves the (possibly case-sensitive) local part untouched.
prop_normalizePreservesLocal : (e : EmailAddress) ->
                               getLocalPart (normalize e) = getLocalPart e
prop_normalizePreservesLocal e = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: soundness of `validateEmail` - any address it returns passes both
||| the local-part and the domain validators. Blocked by: the result is built
||| with `parsedToEmailAddress`, which is private to Proven.SafeEmail, so its
||| fields do not reduce here. This names the proposition; it is not a witness.
public export
ValidateEmailSoundObligation : Type
ValidateEmailSoundObligation =
  (s : String) -> (e : EmailAddress) -> validateEmail s = Just e ->
  (isValidLocalPart (getLocalPart e) && isValidDomain (getDomain e)) = True

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
accepts : String -> Bool
accepts = isJust . validateEmail

||| Test runner for email properties
export
runEmailProperties : IO ()
runEmailProperties = do
  putStrLn "SafeEmail Property Tests"
  putStrLn "========================"
  check "simple address validates" (accepts "user@example.com")
  check "subdomain validates" (accepts "user@mail.example.com")
  check "plus addressing validates" (accepts "user+tag@example.com")
  check "missing @ rejected" (not (accepts "userexample.com"))
  check "empty string rejected" (not (accepts ""))
  check "leading dot in local part rejected" (not (accepts ".user@example.com"))
  check "consecutive dots in local part rejected" (not (accepts "a..b@example.com"))
  check "parsed parts are split at the @"
    (map getLocalPart (validateEmail "user@example.com") == Just "user" &&
     map getDomain (validateEmail "user@example.com") == Just "example.com")
  check "validated addresses satisfy both part validators (soundness corpus)"
    (all (\s => case validateEmail s of
                  Nothing => True
                  Just e => isValidLocalPart (getLocalPart e) &&
                            isValidDomain (getDomain e))
         [ "user@example.com", "a.b+c@sub.example.org", "x@y", "@example.com"
         , "user@", "user@-bad.com", "u@exa_mple.com", "a@b@c.com" ])
  check "normalize lower-cases the domain of a validated address"
    (map (getDomain . normalize) (validateEmail "User@Example.COM") == Just "example.com" &&
     map (getLocalPart . normalize) (validateEmail "User@Example.COM") == Just "User")
  -- Suspected library defect (reported, expectation kept): `parseLocalPart`
  -- tests `all isAtomChar` over the whole dot-atom, but `isAtomChar` excludes
  -- '.', so every dotted local part is rejected.
  check "dots in local part validate" (accepts "first.last@example.com")
  putStrLn ""
