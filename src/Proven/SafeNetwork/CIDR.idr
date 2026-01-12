-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| CIDR Notation Handling
|||
||| Type-safe CIDR (Classless Inter-Domain Routing) notation with validation.
module Proven.SafeNetwork.CIDR

import Proven.Core
import Proven.SafeNetwork.IPv4
import Proven.SafeNetwork.IPv6
import Data.List
import Data.String
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- CIDR Types
--------------------------------------------------------------------------------

||| IPv4 CIDR notation
public export
record CIDR4 where
  constructor MkCIDR4
  network : IPv4
  prefix : Nat
  {auto validPrefix : prefix <= 32}

||| IPv6 CIDR notation
public export
record CIDR6 where
  constructor MkCIDR6
  network : IPv6
  prefix : Nat
  {auto validPrefix : prefix <= 128}

||| Generic CIDR (either IPv4 or IPv6)
public export
data CIDR
  = CIDR4Block CIDR4
  | CIDR6Block CIDR6

public export
Show CIDR4 where
  show (MkCIDR4 net pre) = show net ++ "/" ++ show pre

public export
Show CIDR6 where
  show (MkCIDR6 net pre) = show net ++ "/" ++ show pre

public export
Show CIDR where
  show (CIDR4Block c) = show c
  show (CIDR6Block c) = show c

public export
Eq CIDR4 where
  (MkCIDR4 n1 p1) == (MkCIDR4 n2 p2) = n1 == n2 && p1 == p2

public export
Eq CIDR6 where
  (MkCIDR6 n1 p1) == (MkCIDR6 n2 p2) = n1 == n2 && p1 == p2

--------------------------------------------------------------------------------
-- CIDR Parsing
--------------------------------------------------------------------------------

||| Parse IPv4 CIDR notation
public export
parseCIDR4 : String -> Maybe CIDR4
parseCIDR4 s =
  case split (== '/') s of
    [ipStr, prefixStr] => do
      ip <- parseIPv4 ipStr
      prefix <- parsePositive prefixStr
      if prefix <= 32
        then Just (MkCIDR4 (normalizeNetwork4 ip prefix) prefix)
        else Nothing
    _ => Nothing
  where
    split : (Char -> Bool) -> String -> List String
    split p str = go (unpack str) [] []
      where
        go : List Char -> List Char -> List String -> List String
        go [] current acc = reverse (pack (reverse current) :: acc)
        go (c :: cs) current acc =
          if p c
            then go cs [] (pack (reverse current) :: acc)
            else go cs (c :: current) acc

    normalizeNetwork4 : IPv4 -> Nat -> IPv4
    normalizeNetwork4 ip prefix = applyNetmask ip (prefixToMask prefix)

||| Parse IPv6 CIDR notation
public export
parseCIDR6 : String -> Maybe CIDR6
parseCIDR6 s =
  case split (== '/') s of
    [ipStr, prefixStr] => do
      ip <- parseIPv6 ipStr
      prefix <- parsePositive prefixStr
      if prefix <= 128
        then Just (MkCIDR6 ip prefix)  -- Should normalize
        else Nothing
    _ => Nothing
  where
    split : (Char -> Bool) -> String -> List String
    split p str = go (unpack str) [] []
      where
        go : List Char -> List Char -> List String -> List String
        go [] current acc = reverse (pack (reverse current) :: acc)
        go (c :: cs) current acc =
          if p c
            then go cs [] (pack (reverse current) :: acc)
            else go cs (c :: current) acc

||| Parse any CIDR notation
public export
parseCIDR : String -> Maybe CIDR
parseCIDR s =
  case parseCIDR4 s of
    Just c => Just (CIDR4Block c)
    Nothing => map CIDR6Block (parseCIDR6 s)

--------------------------------------------------------------------------------
-- CIDR Operations
--------------------------------------------------------------------------------

||| Get netmask from CIDR4
public export
cidr4Netmask : CIDR4 -> IPv4
cidr4Netmask (MkCIDR4 _ prefix) = prefixToMask prefix

||| Get broadcast address from CIDR4
public export
cidr4Broadcast : CIDR4 -> IPv4
cidr4Broadcast (MkCIDR4 net prefix) =
  broadcastAddress net (prefixToMask prefix)

||| Get first host address in CIDR4 block
public export
cidr4FirstHost : CIDR4 -> IPv4
cidr4FirstHost (MkCIDR4 (MkIPv4 a b c d) prefix) =
  if prefix == 32 then MkIPv4 a b c d
  else if prefix == 31 then MkIPv4 a b c d  -- Point-to-point
  else MkIPv4 a b c (d + 1)

||| Get last host address in CIDR4 block
public export
cidr4LastHost : CIDR4 -> IPv4
cidr4LastHost cidr =
  let bc = cidr4Broadcast cidr
  in if cidr.prefix >= 31
       then bc
       else case bc of
              MkIPv4 a b c d => MkIPv4 a b c (d - 1)

||| Count addresses in CIDR4 block
public export
cidr4Size : CIDR4 -> Nat
cidr4Size (MkCIDR4 _ prefix) = power 2 (32 `minus` prefix)
  where
    power : Nat -> Nat -> Nat
    power _ 0 = 1
    power b (S n) = b * power b n

||| Count usable host addresses in CIDR4 block
public export
cidr4HostCount : CIDR4 -> Nat
cidr4HostCount (MkCIDR4 _ 32) = 1
cidr4HostCount (MkCIDR4 _ 31) = 2  -- Point-to-point link
cidr4HostCount cidr = cidr4Size cidr `minus` 2  -- Exclude network and broadcast

--------------------------------------------------------------------------------
-- Address Containment
--------------------------------------------------------------------------------

||| Check if IPv4 address is within CIDR4 block
public export
cidr4Contains : CIDR4 -> IPv4 -> Bool
cidr4Contains (MkCIDR4 net prefix) ip =
  let mask = prefixToMask prefix
  in applyNetmask ip mask == applyNetmask net mask

||| Check if CIDR4 block contains another block (supernet)
public export
cidr4ContainsBlock : CIDR4 -> CIDR4 -> Bool
cidr4ContainsBlock outer inner =
  outer.prefix <= inner.prefix &&
  cidr4Contains outer inner.network

||| Check if two CIDR4 blocks overlap
public export
cidr4Overlaps : CIDR4 -> CIDR4 -> Bool
cidr4Overlaps c1 c2 =
  cidr4ContainsBlock c1 c2 || cidr4ContainsBlock c2 c1

--------------------------------------------------------------------------------
-- Common CIDR Blocks
--------------------------------------------------------------------------------

||| Private network: 10.0.0.0/8
public export
privateA : CIDR4
privateA = MkCIDR4 (MkIPv4 10 0 0 0) 8

||| Private network: 172.16.0.0/12
public export
privateB : CIDR4
privateB = MkCIDR4 (MkIPv4 172 16 0 0) 12

||| Private network: 192.168.0.0/16
public export
privateC : CIDR4
privateC = MkCIDR4 (MkIPv4 192 168 0 0) 16

||| Loopback: 127.0.0.0/8
public export
loopbackBlock : CIDR4
loopbackBlock = MkCIDR4 (MkIPv4 127 0 0 0) 8

||| Link-local: 169.254.0.0/16
public export
linkLocalBlock : CIDR4
linkLocalBlock = MkCIDR4 (MkIPv4 169 254 0 0) 16

||| Multicast: 224.0.0.0/4
public export
multicastBlock : CIDR4
multicastBlock = MkCIDR4 (MkIPv4 224 0 0 0) 4

||| Documentation: 192.0.2.0/24 (TEST-NET-1)
public export
testNet1 : CIDR4
testNet1 = MkCIDR4 (MkIPv4 192 0 2 0) 24

||| Documentation: 198.51.100.0/24 (TEST-NET-2)
public export
testNet2 : CIDR4
testNet2 = MkCIDR4 (MkIPv4 198 51 100 0) 24

||| Documentation: 203.0.113.0/24 (TEST-NET-3)
public export
testNet3 : CIDR4
testNet3 = MkCIDR4 (MkIPv4 203 0 113 0) 24

--------------------------------------------------------------------------------
-- Subnet Calculations
--------------------------------------------------------------------------------

||| Split CIDR4 block into two equal subnets
public export
cidr4Split : CIDR4 -> Maybe (CIDR4, CIDR4)
cidr4Split (MkCIDR4 net prefix) =
  if prefix >= 32
    then Nothing
    else
      let newPrefix = prefix + 1
          first = MkCIDR4 net newPrefix
          secondNet = integerToIPv4 (ipv4ToInteger net + cast (power 2 (32 `minus` newPrefix)))
          second = MkCIDR4 secondNet newPrefix
      in Just (first, second)
  where
    power : Nat -> Nat -> Nat
    power _ 0 = 1
    power b (S n) = b * power b n

||| Merge two adjacent CIDR4 blocks into a supernet
public export
cidr4Merge : CIDR4 -> CIDR4 -> Maybe CIDR4
cidr4Merge c1 c2 =
  if c1.prefix /= c2.prefix || c1.prefix == 0
    then Nothing
    else
      let newPrefix = c1.prefix `minus` 1
          mask = prefixToMask newPrefix
          net1 = applyNetmask c1.network mask
          net2 = applyNetmask c2.network mask
      in if net1 == net2
           then Just (MkCIDR4 net1 newPrefix)
           else Nothing

||| Get all subnets of a given size from a CIDR block
public export
cidr4Subnets : CIDR4 -> Nat -> List CIDR4
cidr4Subnets cidr targetPrefix =
  if targetPrefix < cidr.prefix || targetPrefix > 32
    then []
    else if targetPrefix == cidr.prefix
           then [cidr]
           else case cidr4Split cidr of
                  Nothing => []
                  Just (a, b) => cidr4Subnets a targetPrefix ++ cidr4Subnets b targetPrefix

--------------------------------------------------------------------------------
-- Address Iteration (Limited)
--------------------------------------------------------------------------------

||| Get list of addresses in CIDR4 block (for small blocks only)
||| Returns Nothing if block has more than maxCount addresses
public export
cidr4Addresses : CIDR4 -> (maxCount : Nat) -> Maybe (List IPv4)
cidr4Addresses cidr maxCount =
  let size = cidr4Size cidr
  in if size > maxCount
       then Nothing
       else Just (generateAddresses cidr.network size)
  where
    generateAddresses : IPv4 -> Nat -> List IPv4
    generateAddresses _ 0 = []
    generateAddresses ip (S n) =
      ip :: generateAddresses (integerToIPv4 (ipv4ToInteger ip + 1)) n

||| Get list of host addresses in CIDR4 block (for small blocks only)
public export
cidr4Hosts : CIDR4 -> (maxCount : Nat) -> Maybe (List IPv4)
cidr4Hosts cidr maxCount =
  if cidr.prefix >= 31
    then cidr4Addresses cidr maxCount
    else do
      addrs <- cidr4Addresses cidr (maxCount + 2)
      case addrs of
        [] => Just []
        [x] => Just []
        (_ :: xs) => Just (init xs)  -- Remove first (network) and last (broadcast)
  where
    init : List a -> List a
    init [] = []
    init [x] = []
    init (x :: xs) = x :: init xs

--------------------------------------------------------------------------------
-- CIDR Validation
--------------------------------------------------------------------------------

||| Check if CIDR4 network address is properly normalized
public export
isNormalizedCIDR4 : CIDR4 -> Bool
isNormalizedCIDR4 (MkCIDR4 net prefix) =
  applyNetmask net (prefixToMask prefix) == net

||| Normalize a CIDR4 block (set host bits to zero)
public export
normalizeCIDR4 : CIDR4 -> CIDR4
normalizeCIDR4 (MkCIDR4 net prefix) =
  MkCIDR4 (applyNetmask net (prefixToMask prefix)) prefix

--------------------------------------------------------------------------------
-- CIDR Aggregation
--------------------------------------------------------------------------------

||| Aggregate a list of CIDR4 blocks (simplify where possible)
||| This is a simplified version - full aggregation is complex
public export
aggregateCIDR4 : List CIDR4 -> List CIDR4
aggregateCIDR4 cidrs =
  -- Sort by network address, then try to merge adjacent blocks
  sortAndMerge (map normalizeCIDR4 cidrs)
  where
    sortAndMerge : List CIDR4 -> List CIDR4
    sortAndMerge [] = []
    sortAndMerge [x] = [x]
    sortAndMerge (x :: y :: rest) =
      case cidr4Merge x y of
        Just merged => sortAndMerge (merged :: rest)
        Nothing => x :: sortAndMerge (y :: rest)

