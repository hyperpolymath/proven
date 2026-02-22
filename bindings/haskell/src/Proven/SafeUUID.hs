{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe UUID operations via libproven FFI.
--
-- UUID generation, parsing, and formatting are performed by the
-- Idris 2 verified core.
module Proven.SafeUUID
  ( uuidV4
  , parseUUID
  , uuidToString
  , isNilUUID
  , uuidVersion
  ) where

import Data.Word (Word8)
import Proven.FFI (c_proven_uuid_v4, c_proven_uuid_parse, c_proven_uuid_to_string,
                   c_proven_uuid_is_nil, c_proven_uuid_version, c_proven_free_string)
import Proven.FFI.Types (FFI_UUID(..), UUIDResult(..))
import Proven.Core (withCStringLen', stringResultToMaybe)

-- | Generate a random UUID v4.
-- Delegates to @proven_uuid_v4@ in libproven.
uuidV4 :: IO (Maybe [Word8])
uuidV4 = do
  result <- c_proven_uuid_v4
  if uuidrStatusRaw result == 0
    then return (Just (uuidBytes (uuidrUUID result)))
    else return Nothing

-- | Parse a UUID from its canonical string form (8-4-4-4-12).
-- Delegates to @proven_uuid_parse@ in libproven.
parseUUID :: String -> IO (Maybe [Word8])
parseUUID str = withCStringLen' str $ \ptr len -> do
  result <- c_proven_uuid_parse ptr len
  if uuidrStatusRaw result == 0
    then return (Just (uuidBytes (uuidrUUID result)))
    else return Nothing

-- | Format a UUID as its canonical 36-character string.
-- Delegates to @proven_uuid_to_string@ in libproven.
uuidToString :: [Word8] -> IO (Maybe String)
uuidToString bs = do
  sr <- c_proven_uuid_to_string (FFI_UUID bs)
  stringResultToMaybe c_proven_free_string sr

-- | Check if a UUID is the nil UUID (all zeros).
-- Delegates to @proven_uuid_is_nil@ in libproven.
isNilUUID :: [Word8] -> IO Bool
isNilUUID bs = do
  result <- c_proven_uuid_is_nil (FFI_UUID bs)
  return (result /= 0)

-- | Get the version of a UUID.
-- Delegates to @proven_uuid_version@ in libproven.
uuidVersion :: [Word8] -> IO Word8
uuidVersion bs = c_proven_uuid_version (FFI_UUID bs)
