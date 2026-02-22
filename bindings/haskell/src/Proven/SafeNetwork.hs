{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe network operations via libproven FFI.
--
-- IPv4 parsing and classification is performed by the Idris 2 verified core.
module Proven.SafeNetwork
  ( IPv4(..)
  , parseIPv4
  , isPrivate
  , isLoopback
  ) where

import Data.Word (Word8)
import Proven.FFI (c_proven_network_parse_ipv4, c_proven_network_ipv4_is_private,
                   c_proven_network_ipv4_is_loopback)
import Proven.FFI.Types (IPv4Address(..), IPv4Result(..))
import Proven.Core (withCStringLen')

-- | Parsed IPv4 address.
data IPv4 = IPv4
  { octetA :: !Word8
  , octetB :: !Word8
  , octetC :: !Word8
  , octetD :: !Word8
  } deriving (Eq, Show)

-- | Parse an IPv4 address string.
-- Delegates to @proven_network_parse_ipv4@ in libproven.
parseIPv4 :: String -> IO (Maybe IPv4)
parseIPv4 addr = withCStringLen' addr $ \ptr len -> do
  result <- c_proven_network_parse_ipv4 ptr len
  if ipv4rStatusRaw result == 0
    then let a = ipv4rAddress result
         in return (Just (IPv4 (ipv4A a) (ipv4B a) (ipv4C a) (ipv4D a)))
    else return Nothing

-- | Check if an IPv4 address is in a private range (RFC 1918).
-- Delegates to @proven_network_ipv4_is_private@ in libproven.
isPrivate :: IPv4 -> IO Bool
isPrivate (IPv4 a b c d) = do
  result <- c_proven_network_ipv4_is_private (IPv4Address a b c d)
  return (result /= 0)

-- | Check if an IPv4 address is a loopback address (127.0.0.0/8).
-- Delegates to @proven_network_ipv4_is_loopback@ in libproven.
isLoopback :: IPv4 -> IO Bool
isLoopback (IPv4 a b c d) = do
  result <- c_proven_network_ipv4_is_loopback (IPv4Address a b c d)
  return (result /= 0)
