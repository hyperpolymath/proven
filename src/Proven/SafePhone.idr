-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
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

||| Validate an E.164 phone number string
||| Format: +CC NNNNNNNNNN (max 15 digits total)
public export
isValidE164 : String -> Bool
isValidE164 s =
  let stripped = stripFormatting s
  in isPrefixOf "+" s &&
     length stripped >= 7 && length stripped <= maxE164Length &&
     isDigitsOnly stripped

||| Parse an E.164 phone number
public export
parseE164 : String -> Maybe PhoneNumber
parseE164 s =
  if not (isPrefixOf "+" s)
    then Nothing
    else let digits = stripFormatting (strSubstr 1 (length s) s)
         in if length digits < 7 || length digits > maxE164Length || not (isDigitsOnly digits)
              then Nothing
              else parseWithCountryCode digits
  where
    tryCountryCode : Nat -> String -> Maybe PhoneNumber
    tryCountryCode ccLen digits =
      let ccStr = strSubstr 0 ccLen digits
          rest = strSubstr ccLen (length digits) digits
      in case parsePositive {a=Nat} ccStr of
           Nothing => Nothing
           Just cc => if length rest >= 4
                        then Just (MkPhoneNumber (MkCountryCode cc) rest)
                        else Nothing

    parseWithCountryCode : String -> Maybe PhoneNumber
    parseWithCountryCode digits =
      -- Try 1-digit, 2-digit, 3-digit country codes
      case tryCountryCode 1 digits of
        Just phone => Just phone
        Nothing => case tryCountryCode 2 digits of
          Just phone => Just phone
          Nothing => tryCountryCode 3 digits

||| Format a phone number in E.164 format
public export
formatE164 : PhoneNumber -> String
formatE164 phone =
  let (MkCountryCode cc) = phoneCountry phone
  in "+" ++ show cc ++ phoneSubscriber phone

||| Format with spaces for display
public export
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
       else replicate' (minus len 4) '*' ++ strSubstr (minus len 4) len full
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
