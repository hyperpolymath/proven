-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe phone number validation and parsing.
-- |
-- | Validates phone numbers with support for international formats
-- | and provides safe parsing of phone number components.

module Proven.SafePhone
  ( SafePhone
  , PhoneNumber(..)
  , isValidPhone
  , parsePhone
  , formatE164
  , formatNational
  , getCountryCode
  , getNationalNumber
  , requireValidPhone
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, take, drop, length, trim)
import Data.String as S
import Proven.Result (Result(..), ProvenError(..))

-- | SafePhone namespace marker (not instantiated).
data SafePhone

-- | Parsed phone number with validated components.
newtype PhoneNumber = PhoneNumber
  { countryCode :: String
  , nationalNumber :: String
  , rawInput :: String
  }

derive instance eqPhoneNumber :: Eq PhoneNumber

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber p) = "+" <> p.countryCode <> p.nationalNumber

-- | Check if a string is a valid phone number.
-- | Accepts various formats including E.164.
isValidPhone :: String -> Boolean
isValidPhone phone = isValidPhoneImpl (normalizeInput phone)

foreign import isValidPhoneImpl :: String -> Boolean

-- | Normalize phone input by removing common formatting.
normalizeInput :: String -> String
normalizeInput = normalizePhoneImpl

foreign import normalizePhoneImpl :: String -> String

-- | Parse a phone number string.
parsePhone :: String -> Result PhoneNumber ProvenError
parsePhone phone =
  let parsed = parsePhoneImpl (normalizeInput phone)
  in if parsed.valid
     then Ok (PhoneNumber
       { countryCode: parsed.countryCode
       , nationalNumber: parsed.nationalNumber
       , rawInput: phone
       })
     else Err InvalidPhone

foreign import parsePhoneImpl :: String ->
  { valid :: Boolean
  , countryCode :: String
  , nationalNumber :: String
  }

-- | Format phone number in E.164 format (+1234567890).
formatE164 :: PhoneNumber -> String
formatE164 (PhoneNumber p) = "+" <> p.countryCode <> p.nationalNumber

-- | Format phone number in national format (varies by country).
formatNational :: PhoneNumber -> String
formatNational (PhoneNumber p) = formatNationalImpl p.countryCode p.nationalNumber

foreign import formatNationalImpl :: String -> String -> String

-- | Get the country code from a phone number.
getCountryCode :: PhoneNumber -> String
getCountryCode (PhoneNumber p) = p.countryCode

-- | Get the national number (without country code).
getNationalNumber :: PhoneNumber -> String
getNationalNumber (PhoneNumber p) = p.nationalNumber

-- | Require a valid phone number or return error.
requireValidPhone :: String -> Result PhoneNumber ProvenError
requireValidPhone = parsePhone
