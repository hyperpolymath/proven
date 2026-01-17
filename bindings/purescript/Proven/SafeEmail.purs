-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe email address validation and parsing.
-- |
-- | Validates email addresses according to RFC 5321/5322 and provides
-- | safe parsing of email components.

module Proven.SafeEmail
  ( SafeEmail
  , Email(..)
  , isValidEmail
  , parseEmail
  , requireValidEmail
  , getLocalPart
  , getDomain
  , normalize
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, take, drop, toLower, trim)
import Proven.Result (Result(..), ProvenError(..))

-- | SafeEmail namespace marker (not instantiated).
data SafeEmail

-- | Validated email address with parsed components.
newtype Email = Email
  { local :: String
  , domain :: String
  }

derive instance eqEmail :: Eq Email

instance showEmail :: Show Email where
  show (Email e) = e.local <> "@" <> e.domain

-- | Check if a string is a valid email address.
-- | Uses RFC 5321/5322 compliant validation.
isValidEmail :: String -> Boolean
isValidEmail email = isValidEmailImpl (trim email)

foreign import isValidEmailImpl :: String -> Boolean

-- | Parse an email address into its components.
-- | Returns the local part and domain separately.
parseEmail :: String -> Result Email ProvenError
parseEmail email =
  let trimmed = trim email
  in case indexOf (Pattern "@") trimmed of
    Nothing -> Err InvalidEmail
    Just idx ->
      let
        localPart = take idx trimmed
        domainPart = drop (idx + 1) trimmed
      in
        if isValidEmail trimmed
          then Ok (Email { local: localPart, domain: domainPart })
          else Err InvalidEmail

-- | Require a valid email or return error.
requireValidEmail :: String -> Result String ProvenError
requireValidEmail email
  | isValidEmail email = Ok (trim email)
  | otherwise = Err InvalidEmail

-- | Get the local part (before @) of an email.
getLocalPart :: Email -> String
getLocalPart (Email e) = e.local

-- | Get the domain (after @) of an email.
getDomain :: Email -> String
getDomain (Email e) = e.domain

-- | Normalize an email address.
-- | Lowercases the domain part (local part case is preserved per RFC).
normalize :: Email -> Email
normalize (Email e) = Email
  { local: e.local
  , domain: toLower e.domain
  }
