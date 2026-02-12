{- SPDX-License-Identifier: Apache-2.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe network operations for IP address validation and classification.
module Proven.SafeNetwork
  ( IPv4(..)
  , parseIPv4
  , isValidIPv4
  , isPrivate
  , isLoopback
  , isPublic
  , formatIPv4
  ) where

import Text.Read (readMaybe)
import Data.List (intercalate)
import Data.Word (Word8)

-- | Represents an IPv4 address.
data IPv4 = IPv4
  { octetA :: Word8
  , octetB :: Word8
  , octetC :: Word8
  , octetD :: Word8
  } deriving (Show, Eq)

-- | Parse an IPv4 address string.
parseIPv4 :: String -> Maybe IPv4
parseIPv4 address = case splitOn '.' address of
  [a, b, c, d] -> do
    oa <- parseOctet a
    ob <- parseOctet b
    oc <- parseOctet c
    od <- parseOctet d
    return IPv4 { octetA = oa, octetB = ob, octetC = oc, octetD = od }
  _ -> Nothing
  where
    parseOctet s = do
      n <- readMaybe s :: Maybe Int
      if n >= 0 && n <= 255
        then Just (fromIntegral n)
        else Nothing
    splitOn _ [] = [""]
    splitOn c (x:xs)
      | x == c    = "" : splitOn c xs
      | otherwise = let (h:t) = splitOn c xs in (x:h) : t

-- | Check if a string is a valid IPv4 address.
isValidIPv4 :: String -> Bool
isValidIPv4 address = case parseIPv4 address of
  Just _  -> True
  Nothing -> False

-- | Check if an IPv4 address is in a private range.
isPrivate :: String -> Bool
isPrivate address = case parseIPv4 address of
  Nothing -> False
  Just ip ->
    -- 10.0.0.0/8
    octetA ip == 10 ||
    -- 172.16.0.0/12
    (octetA ip == 172 && octetB ip >= 16 && octetB ip <= 31) ||
    -- 192.168.0.0/16
    (octetA ip == 192 && octetB ip == 168)

-- | Check if an IPv4 address is a loopback address (127.0.0.0/8).
isLoopback :: String -> Bool
isLoopback address = case parseIPv4 address of
  Nothing -> False
  Just ip -> octetA ip == 127

-- | Check if an IPv4 address is public (not private or loopback).
isPublic :: String -> Bool
isPublic address = isValidIPv4 address && not (isPrivate address) && not (isLoopback address)

-- | Format an IPv4 address as a string.
formatIPv4 :: IPv4 -> String
formatIPv4 ip = intercalate "." $ map show [octetA ip, octetB ip, octetC ip, octetD ip]
