-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| IPv4 Address Types and Operations
|||
||| Type-safe IPv4 address handling with validation and utilities.
module Proven.SafeNetwork.IPv4

import Proven.Core
import Data.List
import Data.String
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- IPv4 Address Type
--------------------------------------------------------------------------------

||| IPv4 address as 4 octets
public export
record IPv4 where
  constructor MkIPv4
  octet1 : Bits8
  octet2 : Bits8
  octet3 : Bits8
  octet4 : Bits8

public export
Show IPv4 where
  show (MkIPv4 a b c d) =
    show (cast {to=Nat} a) ++ "." ++
    show (cast {to=Nat} b) ++ "." ++
    show (cast {to=Nat} c) ++ "." ++
    show (cast {to=Nat} d)

public export
Eq IPv4 where
  (MkIPv4 a1 b1 c1 d1) == (MkIPv4 a2 b2 c2 d2) =
    a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2

public export
Ord IPv4 where
  compare ip1 ip2 = compare (toInteger ip1) (toInteger ip2)
    where
      toInteger : IPv4 -> Integer
      toInteger (MkIPv4 a b c d) =
        cast a * 16777216 + cast b * 65536 + cast c * 256 + cast d

--------------------------------------------------------------------------------
-- IPv4 Parsing
--------------------------------------------------------------------------------

||| Parse IPv4 address from dotted decimal string
public export
parseIPv4 : String -> Maybe IPv4
parseIPv4 s =
  case split (== '.') s of
    [a, b, c, d] => do
      o1 <- parseOctet a
      o2 <- parseOctet b
      o3 <- parseOctet c
      o4 <- parseOctet d
      Just (MkIPv4 o1 o2 o3 o4)
    _ => Nothing
  where
    parseOctet : String -> Maybe Bits8
    parseOctet str =
      case parsePositive str of
        Just n => if n <= 255 then Just (cast n) else Nothing
        Nothing => Nothing

    split : (Char -> Bool) -> String -> List String
    split p str = go (unpack str) [] []
      where
        go : List Char -> List Char -> List String -> List String
        go [] current acc = reverse (pack (reverse current) :: acc)
        go (c :: cs) current acc =
          if p c
            then go cs [] (pack (reverse current) :: acc)
            else go cs (c :: current) acc

--------------------------------------------------------------------------------
-- IPv4 Conversion
--------------------------------------------------------------------------------

||| Convert IPv4 to 32-bit integer
public export
ipv4ToInteger : IPv4 -> Bits32
ipv4ToInteger (MkIPv4 a b c d) =
  (cast a `shiftL` 24) .|.
  (cast b `shiftL` 16) .|.
  (cast c `shiftL` 8) .|.
  cast d

||| Convert 32-bit integer to IPv4
public export
integerToIPv4 : Bits32 -> IPv4
integerToIPv4 n = MkIPv4
  (cast (n `shiftR` 24))
  (cast ((n `shiftR` 16) .&. 0xFF))
  (cast ((n `shiftR` 8) .&. 0xFF))
  (cast (n .&. 0xFF))

||| Convert IPv4 to list of octets
public export
ipv4ToOctets : IPv4 -> List Bits8
ipv4ToOctets (MkIPv4 a b c d) = [a, b, c, d]

--------------------------------------------------------------------------------
-- Special Addresses
--------------------------------------------------------------------------------

||| Localhost (127.0.0.1)
public export
localhost : IPv4
localhost = MkIPv4 127 0 0 1

||| Broadcast (255.255.255.255)
public export
broadcast : IPv4
broadcast = MkIPv4 255 255 255 255

||| Any address (0.0.0.0)
public export
anyAddress : IPv4
anyAddress = MkIPv4 0 0 0 0

--------------------------------------------------------------------------------
-- Address Classification
--------------------------------------------------------------------------------

||| Check if IPv4 is in private range (RFC 1918)
public export
isPrivateIPv4 : IPv4 -> Bool
isPrivateIPv4 (MkIPv4 10 _ _ _) = True                          -- 10.0.0.0/8
isPrivateIPv4 (MkIPv4 172 b _ _) = b >= 16 && b <= 31           -- 172.16.0.0/12
isPrivateIPv4 (MkIPv4 192 168 _ _) = True                       -- 192.168.0.0/16
isPrivateIPv4 _ = False

||| Check if IPv4 is loopback (127.0.0.0/8)
public export
isLoopbackIPv4 : IPv4 -> Bool
isLoopbackIPv4 (MkIPv4 127 _ _ _) = True
isLoopbackIPv4 _ = False

||| Check if IPv4 is link-local (169.254.0.0/16)
public export
isLinkLocalIPv4 : IPv4 -> Bool
isLinkLocalIPv4 (MkIPv4 169 254 _ _) = True
isLinkLocalIPv4 _ = False

||| Check if IPv4 is multicast (224.0.0.0/4)
public export
isMulticastIPv4 : IPv4 -> Bool
isMulticastIPv4 (MkIPv4 a _ _ _) = a >= 224 && a <= 239

||| Check if IPv4 is broadcast
public export
isBroadcastIPv4 : IPv4 -> Bool
isBroadcastIPv4 (MkIPv4 255 255 255 255) = True
isBroadcastIPv4 _ = False

||| Check if IPv4 is reserved (0.0.0.0/8)
public export
isReservedIPv4 : IPv4 -> Bool
isReservedIPv4 (MkIPv4 0 _ _ _) = True
isReservedIPv4 _ = False

||| Check if IPv4 is documentation range (192.0.2.0/24, 198.51.100.0/24, 203.0.113.0/24)
public export
isDocumentationIPv4 : IPv4 -> Bool
isDocumentationIPv4 (MkIPv4 192 0 2 _) = True
isDocumentationIPv4 (MkIPv4 198 51 100 _) = True
isDocumentationIPv4 (MkIPv4 203 0 113 _) = True
isDocumentationIPv4 _ = False

||| Check if IPv4 is globally routable
public export
isGlobalIPv4 : IPv4 -> Bool
isGlobalIPv4 ip =
  not (isPrivateIPv4 ip) &&
  not (isLoopbackIPv4 ip) &&
  not (isLinkLocalIPv4 ip) &&
  not (isMulticastIPv4 ip) &&
  not (isBroadcastIPv4 ip) &&
  not (isReservedIPv4 ip) &&
  not (isDocumentationIPv4 ip)

--------------------------------------------------------------------------------
-- Network Classes (Historical)
--------------------------------------------------------------------------------

||| IPv4 address class (historical classification)
public export
data IPv4Class = ClassA | ClassB | ClassC | ClassD | ClassE

public export
Show IPv4Class where
  show ClassA = "Class A"
  show ClassB = "Class B"
  show ClassC = "Class C"
  show ClassD = "Class D (Multicast)"
  show ClassE = "Class E (Reserved)"

||| Get address class (historical)
public export
ipv4Class : IPv4 -> IPv4Class
ipv4Class (MkIPv4 a _ _ _) =
  if a < 128 then ClassA
  else if a < 192 then ClassB
  else if a < 224 then ClassC
  else if a < 240 then ClassD
  else ClassE

--------------------------------------------------------------------------------
-- Network Operations
--------------------------------------------------------------------------------

||| Apply netmask to get network address
public export
applyNetmask : IPv4 -> IPv4 -> IPv4
applyNetmask ip mask =
  integerToIPv4 (ipv4ToInteger ip .&. ipv4ToInteger mask)

||| Calculate broadcast address for network
public export
broadcastAddress : IPv4 -> IPv4 -> IPv4
broadcastAddress ip mask =
  let netAddr = ipv4ToInteger ip .&. ipv4ToInteger mask
      invMask = complement (ipv4ToInteger mask)
  in integerToIPv4 (netAddr .|. invMask)

||| Check if two IPs are on the same network
public export
sameNetwork : IPv4 -> IPv4 -> IPv4 -> Bool
sameNetwork ip1 ip2 mask =
  applyNetmask ip1 mask == applyNetmask ip2 mask

||| Count hosts in subnet (excluding network and broadcast)
public export
hostCount : IPv4 -> Nat
hostCount mask =
  let bits = ipv4ToInteger mask
      hostBits = countZeros bits
  in if hostBits <= 2 then 0 else cast (power 2 hostBits) `minus` 2
  where
    countZeros : Bits32 -> Nat
    countZeros 0xFFFFFFFF = 0
    countZeros n = 32 `minus` popCount n
      where
        popCount : Bits32 -> Nat
        popCount 0 = 0
        popCount x = cast (x .&. 1) + popCount (assert_smaller x (x `shiftR` 1))

    power : Nat -> Nat -> Nat
    power _ 0 = 1
    power b (S n) = b * power b n

--------------------------------------------------------------------------------
-- Netmask Construction
--------------------------------------------------------------------------------

||| Create netmask from prefix length (0-32)
public export
prefixToMask : Nat -> IPv4
prefixToMask pre =
  if pre >= 32
    then MkIPv4 255 255 255 255
    else integerToIPv4 (complement (cast ((power 2 (32 `minus` pre)) `minus` 1)))
  where
    power : Nat -> Nat -> Nat
    power _ 0 = 1
    power b (S n) = b * power b n

||| Get prefix length from netmask
public export
maskToPrefix : IPv4 -> Maybe Nat
maskToPrefix mask =
  let bits = ipv4ToInteger mask
  in if isValidMask bits then Just (countOnes bits) else Nothing
  where
    isValidMask : Bits32 -> Bool
    isValidMask n =
      let inverted = complement n
      in (inverted .&. (inverted + 1)) == 0

    countOnes : Bits32 -> Nat
    countOnes 0 = 0
    countOnes n = cast (n .&. 1) + countOnes (assert_smaller n (n `shiftR` 1))

--------------------------------------------------------------------------------
-- Common Netmasks
--------------------------------------------------------------------------------

public export
mask8 : IPv4
mask8 = MkIPv4 255 0 0 0

public export
mask16 : IPv4
mask16 = MkIPv4 255 255 0 0

public export
mask24 : IPv4
mask24 = MkIPv4 255 255 255 0

public export
mask32 : IPv4
mask32 = MkIPv4 255 255 255 255
