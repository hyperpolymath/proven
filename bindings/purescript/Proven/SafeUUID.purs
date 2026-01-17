-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe UUID generation and validation following RFC 4122.
-- |
-- | Provides UUID parsing, validation, and generation with proper
-- | version and variant handling.

module Proven.SafeUUID
  ( SafeUUID
  , Uuid(..)
  , UuidVersion(..)
  , isValidUuid
  , parseUuid
  , nilUuid
  , getVersion
  , isNil
  , toUrn
  , requireValidUuid
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length, toUpper)
import Data.String as S
import Proven.Result (Result(..), ProvenError(..))

-- | SafeUUID namespace marker (not instantiated).
data SafeUUID

-- | UUID version types per RFC 4122.
data UuidVersion
  = V1  -- Time-based
  | V2  -- DCE Security
  | V3  -- Name-based (MD5)
  | V4  -- Random
  | V5  -- Name-based (SHA-1)
  | Nil -- Nil UUID

derive instance eqUuidVersion :: Eq UuidVersion

instance showUuidVersion :: Show UuidVersion where
  show V1 = "V1"
  show V2 = "V2"
  show V3 = "V3"
  show V4 = "V4"
  show V5 = "V5"
  show Nil = "Nil"

-- | Validated UUID (128 bits represented as canonical string).
newtype Uuid = Uuid String

derive instance eqUuid :: Eq Uuid
derive instance ordUuid :: Ord Uuid

instance showUuid :: Show Uuid where
  show (Uuid s) = s

-- | The nil UUID (all zeros).
nilUuid :: Uuid
nilUuid = Uuid "00000000-0000-0000-0000-000000000000"

-- | Check if a string is a valid UUID format.
-- | Accepts canonical format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
isValidUuid :: String -> Boolean
isValidUuid s =
  length s == 36 && isValidUuidImpl s

foreign import isValidUuidImpl :: String -> Boolean

-- | Parse a UUID from string.
parseUuid :: String -> Result Uuid ProvenError
parseUuid s
  | isValidUuid s = Ok (Uuid (S.toLower s))
  | otherwise = Err InvalidUuid

-- | Get the version of a UUID.
getVersion :: Uuid -> UuidVersion
getVersion (Uuid s) =
  let versionChar = S.charAt 14 s
  in case versionChar of
    Just '1' -> V1
    Just '2' -> V2
    Just '3' -> V3
    Just '4' -> V4
    Just '5' -> V5
    Just '0' -> Nil
    _ -> Nil

-- | Check if a UUID is the nil UUID.
isNil :: Uuid -> Boolean
isNil (Uuid s) = s == "00000000-0000-0000-0000-000000000000"

-- | Convert UUID to URN format.
toUrn :: Uuid -> String
toUrn (Uuid s) = "urn:uuid:" <> s

-- | Require a valid UUID or return error.
requireValidUuid :: String -> Result Uuid ProvenError
requireValidUuid = parseUuid
