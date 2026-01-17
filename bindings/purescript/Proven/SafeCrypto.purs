-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe cryptographic operations.
-- |
-- | Provides hashing, HMAC, and comparison operations with
-- | constant-time implementations where security-relevant.

module Proven.SafeCrypto
  ( SafeCrypto
  , sha256
  , sha512
  , blake3
  , hmacSha256
  , constantTimeCompare
  , toHex
  , fromHex
  , randomBytes
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (length)
import Proven.Result (Result(..), ProvenError(..))

-- | SafeCrypto namespace marker (not instantiated).
data SafeCrypto

-- | Compute SHA-256 hash and return as hex string.
-- | Returns 64 character hex string (32 bytes).
sha256 :: String -> String
sha256 s = sha256Impl s

foreign import sha256Impl :: String -> String

-- | Compute SHA-512 hash and return as hex string.
-- | Returns 128 character hex string (64 bytes).
sha512 :: String -> String
sha512 s = sha512Impl s

foreign import sha512Impl :: String -> String

-- | Compute BLAKE3 hash and return as hex string.
-- | BLAKE3 is the primary hash per Hyperpolymath Crypto Standard.
-- | Returns 64 character hex string (32 bytes).
blake3 :: String -> String
blake3 s = blake3Impl s

foreign import blake3Impl :: String -> String

-- | Compute HMAC-SHA256 and return as hex string.
hmacSha256 :: String -> String -> String
hmacSha256 key message = hmacSha256Impl key message

foreign import hmacSha256Impl :: String -> String -> String

-- | Constant-time comparison to prevent timing attacks.
-- | Returns true only if both strings are equal and have the same length.
constantTimeCompare :: String -> String -> Boolean
constantTimeCompare a b = constantTimeCompareImpl a b

foreign import constantTimeCompareImpl :: String -> String -> Boolean

-- | Convert bytes (as array of ints 0-255) to hex string.
toHex :: Array Int -> String
toHex bytes = toHexImpl bytes

foreign import toHexImpl :: Array Int -> String

-- | Convert hex string to bytes (array of ints 0-255).
-- | Returns Nothing for invalid hex strings.
fromHex :: String -> Maybe (Array Int)
fromHex hex =
  let result = fromHexImpl hex
  in if result.valid
     then Just result.bytes
     else Nothing

foreign import fromHexImpl :: String -> { valid :: Boolean, bytes :: Array Int }

-- | Generate cryptographically secure random bytes.
-- | Returns an array of random values 0-255.
randomBytes :: Int -> Array Int
randomBytes len = randomBytesImpl len

foreign import randomBytesImpl :: Int -> Array Int
