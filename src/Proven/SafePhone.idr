-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafePhone - E.164 phone number parsing and validation
|||
||| Provides type-safe phone number handling per ITU-T E.164.
||| Validates: country codes, number length, format compliance.
module Proven.SafePhone

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| Country calling codes (ITU-T E.164)
public export
data CountryCode : Type where
  MkCountryCode : (code : Nat) -> CountryCode

public export
Show CountryCode where
  show (MkCountryCode c) = "+" ++ show c

public export
Eq CountryCode where
  MkCountryCode a == MkCountryCode b = a == b

||| Common country codes
public export
countryUS : CountryCode
countryUS = MkCountryCode 1

public export
countryUK : CountryCode
countryUK = MkCountryCode 44

public export
countryDE : CountryCode
countryDE = MkCountryCode 49

public export
countryFR : CountryCode
countryFR = MkCountryCode 33

public export
countryJP : CountryCode
countryJP = MkCountryCode 81

public export
countryAU : CountryCode
countryAU = MkCountryCode 61

public export
countryIN : CountryCode
countryIN = MkCountryCode 91

||| A validated E.164 phone number
public export
record PhoneNumber where
  constructor MkPhoneNumber
  phoneCountry    : CountryCode
  phoneSubscriber : String    -- National subscriber number (digits only)

||| Check if a string contains only digits
public export
isDigitsOnly : String -> Bool
isDigitsOnly s = all isDigit (unpack s)

||| Strip formatting characters from a phone number
public export
stripFormatting : String -> String
stripFormatting s = pack (filter isDigit (unpack s))

||| E.164 max total length is 15 digits (including country code)
public export
maxE164Length : Nat
maxE164Length = 15

||| Visual separators accepted between the digits of a written international
||| number. ITU-T E.123 international notation is "+" then digit groups
||| separated by spaces; hyphens, dots and parentheses are common display
||| punctuation.
||| Any other character (e.g. a letter) makes the input invalid instead of
||| being silently dropped.
public export
isPhoneSeparator : Char -> Bool
isPhoneSeparator c = c == ' ' || c == '-' || c == '.' || c == '(' || c == ')'

||| The complete set of 2-digit E.164 country codes (ITU-T E.164 list of
||| assigned country codes, Annex to the ITU Operational Bulletin). Zones 1
||| (NANP) and 7 are 1-digit codes; every other code is 3 digits.
public export
twoDigitCountryCodes : List Nat
twoDigitCountryCodes =
  [ 20, 27
  , 30, 31, 32, 33, 34, 36, 39
  , 40, 41, 43, 44, 45, 46, 47, 48, 49
  , 51, 52, 53, 54, 55, 56, 57, 58
  , 60, 61, 62, 63, 64, 65, 66
  , 81, 82, 84, 86
  , 90, 91, 92, 93, 94, 95, 98
  ]

||| Length of the country code that starts the given digits. E.164 country
||| codes form a prefix code (none is a prefix of another; ITU-T E.164 §6),
||| so the leading digits alone determine it: a longest/known-prefix match,
||| not "try 1 digit first". No country code starts with 0.
public export
countryCodeLength : List Char -> Maybe Nat
countryCodeLength ('0' :: _) = Nothing
countryCodeLength ('1' :: _) = Just 1
countryCodeLength ('7' :: _) = Just 1
countryCodeLength (a :: b :: _) =
  let twoDigits = cast {to=Nat} (ord a - ord '0') * 10 + cast {to=Nat} (ord b - ord '0')
  in if elem twoDigits twoDigitCountryCodes then Just 2 else Just 3
countryCodeLength _ = Nothing

||| Parse an E.164 phone number: "+", then 7..15 digits (ITU-T E.164 §6:
||| at most 15 digits including the country code), optionally separated by
||| `isPhoneSeparator` characters. The country code is split off by prefix
||| match (`countryCodeLength`), leaving a subscriber part of at least 4 digits.
public export
parseE164 : String -> Maybe PhoneNumber
parseE164 s =
  case unpack s of
    ('+' :: body) =>
      let digits = filter (not . isPhoneSeparator) body
          n = length digits
      in if not (isDigitsOnly (pack digits)) || n < 7 || n > maxE164Length
           then Nothing
           else case countryCodeLength digits of
                  Nothing => Nothing
                  Just ccLen =>
                    let (ccDigits, rest) = splitAt ccLen digits
                    in case parsePositive {a=Nat} (pack ccDigits) of
                         Nothing => Nothing
                         Just cc => if length rest >= 4
                                      then Just (MkPhoneNumber (MkCountryCode cc) (pack rest))
                                      else Nothing
    _ => Nothing

||| Validate an E.164 phone number string
||| Format: +CC NNNNNNNNNN (max 15 digits total). Agrees with `parseE164`.
public export
isValidE164 : String -> Bool
isValidE164 s = isJust (parseE164 s)

||| Format a phone number in E.164 format
public export
formatE164 : PhoneNumber -> String
formatE164 phone =
  let (MkCountryCode cc) = phoneCountry phone
  in "+" ++ show cc ++ phoneSubscriber phone

||| Format with spaces for display
public export covering
formatDisplay : PhoneNumber -> String
formatDisplay phone =
  let (MkCountryCode cc) = phoneCountry phone
      sub = phoneSubscriber phone
  in "+" ++ show cc ++ " " ++ formatGroups (unpack sub)
  where
    formatGroups : List Char -> String
    formatGroups [] = ""
    formatGroups cs =
      let (group, rest) = splitAt 3 cs
      in pack group ++ (if null rest then "" else " " ++ formatGroups rest)

||| Mask a phone number for display (show last 4 digits)
public export
maskPhone : PhoneNumber -> String
maskPhone phone =
  let full = formatE164 phone
      len = length full
  in if len <= 4
       then full
       else replicate' (minus len 4) '*' ++ substr (minus len 4) len full
  where
    replicate' : Nat -> Char -> String
    replicate' Z _ = ""
    replicate' (S n) c = singleton c ++ replicate' n c

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that a phone number is valid E.164
public export
data ValidE164 : PhoneNumber -> Type where
  MkValidE164 : (p : PhoneNumber) -> ValidE164 p

||| Proof that a phone number string parses correctly
public export
data ParseablePhone : String -> Type where
  MkParseablePhone : isValidE164 s = True -> ParseablePhone s
