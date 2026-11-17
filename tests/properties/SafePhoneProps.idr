-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafePhoneProps

import Proven.Core
import Proven.SafePhone
import Data.Maybe
import System

%default total

-- The real API is E.164-only: `parseE164 : String -> Maybe PhoneNumber`
-- (requires a leading '+', allows only digits and space/-/./()/ separators,
-- 7..15 digits, splits the country code by E.164 prefix match),
-- `formatE164 : PhoneNumber -> String`, and
-- `stripFormatting`. There is no `parsePhone`, no national-format parser and
-- no `Eq PhoneNumber`. Parsing goes through `unpack`/`substr`/
-- `parsePositive`, which do not reduce at the type level, so parse
-- properties are execution checks.

||| Property: country-code equality is reflexive, for every code.
||| (Replaces the orphan's `p == p` on a phone type with no `Eq`.)
prop_countryCodeEqReflexive : (n : Nat) -> (MkCountryCode n == MkCountryCode n) = True
prop_countryCodeEqReflexive Z = Refl
prop_countryCodeEqReflexive (S k) = prop_countryCodeEqReflexive k

||| OWED: parsing is a fixed point of format-then-parse: whatever
||| `parseE164` returns re-parses from its own E.164 rendering to the same
||| country code and subscriber. Blocked by: `parseE164`/`formatE164` go
||| through `unpack`, `substr`, `show` and `parsePositive` on String, none of
||| which reduce on abstract input. This names the proposition; it is not a
||| witness.
public export
FormatE164RoundTripObligation : Type
FormatE164RoundTripObligation =
  (s : String) -> (p : PhoneNumber) -> parseE164 s = Just p ->
  parseE164 (formatE164 p) = Just p

||| OWED: a parsed number's country code has 1 to 3 digits and its
||| subscriber part at least 4. Blocked by: same String non-reduction. (The
||| `LTE 1 cc` part was refuted by "+0123456789" parsing with country code 0
||| until parseE164 rejected a leading 0; checked at runtime below.) This
||| names the proposition; it is not a witness.
public export
ParsedPartsShapeObligation : Type
ParsedPartsShapeObligation =
  (s : String) -> (cc : Nat) -> (sub : String) ->
  parseE164 s = Just (MkPhoneNumber (MkCountryCode cc) sub) ->
  (LTE 1 cc, LTE cc 999, LTE 4 (length sub))

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
parsesAs : String -> Nat -> String -> Bool
parsesAs s cc sub = case parseE164 s of
  Just (MkPhoneNumber (MkCountryCode c) n) => c == cc && n == sub
  Nothing => False

private
roundTrips : String -> Bool
roundTrips s = case parseE164 s of
  Just p@(MkPhoneNumber (MkCountryCode c) n) => parsesAs (formatE164 p) c n
  Nothing => False

private
shapeOk : String -> Bool
shapeOk s = case parseE164 s of
  Just (MkPhoneNumber (MkCountryCode c) n) => c >= 1 && c <= 999 && length n >= 4
  Nothing => False

||| Test runner for phone properties
export
runPhoneProperties : IO ()
runPhoneProperties = do
  putStrLn "SafePhone Property Tests"
  putStrLn "========================"
  putStrLn "prop_countryCodeEqReflexive: PASS (proven by type, all codes)"
  check "E.164 +14155551234 parses as +1 / 4155551234"
    (parsesAs "+14155551234" 1 "4155551234")
  check "US national format without '+' is not E.164 (no national parser exists)"
    (isNothing (parseE164 "(415) 555-1234"))
  check "different formattings normalise to the same number"
    (stripFormatting "(415) 555-1234" == stripFormatting "4155551234" &&
     parsesAs "+1 (415) 555-1234" 1 "4155551234" &&
     map formatE164 (parseE164 "+1 (415) 555-1234") ==
       map formatE164 (parseE164 "+14155551234"))
  check "international number with spaces parses"
    (isJust (parseE164 "+44 20 7946 0958"))
  check "too short rejected"
    (isNothing (parseE164 "123") && isNothing (parseE164 "+123"))
  check "too long (over 15 digits) rejected"
    (isNothing (parseE164 "+12345678901234567890"))
  check "all-letter subscriber rejected"
    (isNothing (parseE164 "+1ABCDEFGHIJ"))
  check "format/parse round trip on corpus (instances of FormatE164RoundTripObligation)"
    (all roundTrips ["+14155551234", "+44 20 7946 0958", "+81 3 1234 5678",
                     "+1 (415) 555-1234"])
  check "parsed part shapes on corpus (instances of ParsedPartsShapeObligation)"
    (all shapeOk ["+14155551234", "+44 20 7946 0958", "+81 3 1234 5678"])
  -- Kept last: FAILs here are suspected library defects (see report).
  check "UK number +44 20 7946 0958 has country code 44"
    (parsesAs "+44 20 7946 0958" 44 "2079460958")
  check "letters inside an otherwise valid-length number are rejected"
    (isNothing (parseE164 "+1415ABC5551234"))
  check "country code 0 is rejected (no E.164 code starts with 0)"
    (isNothing (parseE164 "+0123456789"))
  -- Regression cases for the prefix-match fix (1-, 2- and 3-digit codes).
  check "country code split by prefix: 1, 7, 49, 86, 353, 420, 971, 880"
    (parsesAs "+1 268 555 0100" 1 "2685550100" &&
     parsesAs "+7 495 123 4567" 7 "4951234567" &&
     parsesAs "+49 30 1234567" 49 "301234567" &&
     parsesAs "+86 10 1234 5678" 86 "1012345678" &&
     parsesAs "+353 1 234 5678" 353 "12345678" &&
     parsesAs "+420 123 456 789" 420 "123456789" &&
     parsesAs "+971 4 123 4567" 971 "41234567" &&
     parsesAs "+880 2 1234 5678" 880 "212345678")
  check "format/parse round trip and shape on multi-digit-code corpus"
    (all (\s => roundTrips s && shapeOk s)
       ["+353 1 234 5678", "+420 123 456 789", "+971 4 123 4567", "+7 495 123 4567"])
  check "other non-separator characters rejected; separators still accepted"
    (isNothing (parseE164 "+44 20 7946 0958x") && isNothing (parseE164 "+44_2079460958") &&
     isNothing (parseE164 "+") && isNothing (parseE164 "+ ") &&
     parsesAs "+44-20-7946-0958" 44 "2079460958" &&
     parsesAs "+44.20.7946.0958" 44 "2079460958")
  check "isValidE164 agrees with parseE164 (letters, code 0, valid UK)"
    (not (isValidE164 "+1415ABC5551234") && not (isValidE164 "+0123456789") &&
     isValidE164 "+44 20 7946 0958" && isValidE164 "+1 (415) 555-1234")
  putStrLn ""
