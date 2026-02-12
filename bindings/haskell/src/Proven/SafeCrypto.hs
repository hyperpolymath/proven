{- SPDX-License-Identifier: Apache-2.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Cryptographic safety operations with constant-time guarantees.
module Proven.SafeCrypto
  ( constantTimeCompare
  , secureZero
  , randomBytes
  , randomHex
  ) where

import Data.Bits (xor)
import Data.Char (intToDigit)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import System.Entropy (getEntropy)

-- | Compare two strings in constant time to prevent timing attacks.
constantTimeCompare :: String -> String -> Bool
constantTimeCompare a b
  | length a /= length b = False
  | null a = True
  | otherwise = foldl xor 0 (zipWith xorChars a b) == 0
  where
    xorChars c1 c2 = fromEnum c1 `xor` fromEnum c2

-- | Create a zeroed string of the specified length.
secureZero :: Int -> String
secureZero n = replicate n '\0'

-- | Generate cryptographically secure random bytes.
randomBytes :: Int -> IO BS.ByteString
randomBytes = getEntropy

-- | Generate a cryptographically secure random hex string.
randomHex :: Int -> IO String
randomHex n = do
  bytes <- getEntropy n
  return $ concatMap byteToHex (BS.unpack bytes)
  where
    byteToHex :: Word8 -> String
    byteToHex b = [intToDigit (fromIntegral (b `div` 16)), intToDigit (fromIntegral (b `mod` 16))]
