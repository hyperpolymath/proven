-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe network address validation.
-- |
-- | Validates IP addresses, ports, hostnames, and other network
-- | identifiers with proper bounds checking.

module Proven.SafeNetwork
  ( SafeNetwork
  , isValidPort
  , isValidIpv4
  , isValidIpv6
  , isValidHostname
  , isPrivateIp
  , isLoopback
  , requireValidPort
  , requireValidIpv4
  , requireValidHostname
  , parseIpv4
  ) where

import Prelude

import Data.Array (length, all)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, take)
import Data.String as S
import Proven.Result (Result(..), ProvenError(..))

-- | SafeNetwork namespace marker (not instantiated).
data SafeNetwork

-- | Check if a port number is valid (1-65535).
isValidPort :: Int -> Boolean
isValidPort port = port >= 1 && port <= 65535

-- | Check if a string is a valid IPv4 address.
isValidIpv4 :: String -> Boolean
isValidIpv4 ip =
  let
    parts = split (Pattern ".") ip
    parseOctet s = fromMaybe (-1) (fromString s)
    isValidOctet n = n >= 0 && n <= 255
  in
    length parts == 4 && all (\p -> isValidOctet (parseOctet p)) parts

-- | Check if a string is a valid IPv6 address.
-- | Uses simplified validation for common formats.
isValidIpv6 :: String -> Boolean
isValidIpv6 ip = isValidIpv6Impl ip

foreign import isValidIpv6Impl :: String -> Boolean

-- | Check if a string is a valid hostname.
-- | Validates according to RFC 1123.
isValidHostname :: String -> Boolean
isValidHostname hostname = isValidHostnameImpl hostname

foreign import isValidHostnameImpl :: String -> Boolean

-- | Check if an IPv4 address is in a private range.
-- | Private ranges: 10.x.x.x, 172.16-31.x.x, 192.168.x.x
isPrivateIp :: String -> Boolean
isPrivateIp ip =
  let
    parts = split (Pattern ".") ip
    getOctet idx = fromMaybe 0 (do
      p <- parts !! idx
      fromString p)
    o1 = getOctet 0
    o2 = getOctet 1
  in
    o1 == 10 ||
    (o1 == 172 && o2 >= 16 && o2 <= 31) ||
    (o1 == 192 && o2 == 168)
  where
    infixl 8 !!
    arr !! idx = arrIndex arr idx

foreign import arrIndex :: forall a. Array a -> Int -> Maybe a

-- | Check if an IP address is a loopback address.
isLoopback :: String -> Boolean
isLoopback ip =
  take 4 ip == "127." ||
  ip == "::1" ||
  ip == "0:0:0:0:0:0:0:1"

-- | Require a valid port or return error.
requireValidPort :: Int -> Result Int ProvenError
requireValidPort port
  | isValidPort port = Ok port
  | otherwise = Err InvalidPort

-- | Require a valid IPv4 address or return error.
requireValidIpv4 :: String -> Result String ProvenError
requireValidIpv4 ip
  | isValidIpv4 ip = Ok ip
  | otherwise = Err (InvalidFormat "Invalid IPv4 address")

-- | Require a valid hostname or return error.
requireValidHostname :: String -> Result String ProvenError
requireValidHostname hostname
  | isValidHostname hostname = Ok hostname
  | otherwise = Err (InvalidFormat "Invalid hostname")

-- | Parse an IPv4 address into octets.
parseIpv4 :: String -> Result (Array Int) ProvenError
parseIpv4 ip
  | isValidIpv4 ip =
      let
        parts = split (Pattern ".") ip
        octets = map (\p -> fromMaybe 0 (fromString p)) parts
      in Ok octets
  | otherwise = Err (InvalidFormat "Invalid IPv4 address")
