-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| CIDR Notation Handling
|||
||| Type-safe CIDR (Classless Inter-Domain Routing) notation with validation.
||| Provides network address calculation, containment checks, and subnet operations.
module Proven.SafeNetwork.CIDR

import Data.Nat
import Data.List
import Data.String
import Data.Maybe
import Proven.SafeNetwork.IPv4

%default total

||| Valid prefix lengths for IPv4 (0-32)
public export
data PrefixLength : Type where
  MkPrefix : (n : Nat) -> {auto ok : LTE n 32} -> PrefixLength

||| Extract the numeric value
public export
prefixValue : PrefixLength -> Nat
prefixValue (MkPrefix n) = n

||| Smart constructor for prefix length
public export
mkPrefix : Nat -> Maybe PrefixLength
mkPrefix n = case isLTE n 32 of
  Yes prf => Just (MkPrefix n)
  No _ => Nothing

||| CIDR block representation
public export
record CIDRBlock where
  constructor MkCIDR
  network : (Nat, Nat, Nat, Nat)
  prefix  : PrefixLength

||| Show instance for PrefixLength
public export
Show PrefixLength where
  show (MkPrefix n) = show n

||| Show instance for CIDRBlock
public export
Show CIDRBlock where
  show cidr =
    let (a, b, c, d) = network cidr
    in show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
       ++ "/" ++ show (prefix cidr)

||| Eq instance for PrefixLength
public export
Eq PrefixLength where
  MkPrefix n == MkPrefix m = n == m

||| Eq instance for CIDRBlock
public export
Eq CIDRBlock where
  c1 == c2 = network c1 == network c2 && prefix c1 == prefix c2

||| Calculate the subnet mask from prefix length
public export
subnetMask : PrefixLength -> (Nat, Nat, Nat, Nat)
subnetMask (MkPrefix 0)  = (0, 0, 0, 0)
subnetMask (MkPrefix 8)  = (255, 0, 0, 0)
subnetMask (MkPrefix 16) = (255, 255, 0, 0)
subnetMask (MkPrefix 24) = (255, 255, 255, 0)
subnetMask (MkPrefix 32) = (255, 255, 255, 255)
subnetMask (MkPrefix n)  =
  let totalBits = 32
      hostBits = minus totalBits n
      maskVal = minus (power 2 totalBits) (power 2 hostBits)
      o1 = div maskVal (power 2 24)
      o2 = div (mod maskVal (power 2 24)) (power 2 16)
      o3 = div (mod maskVal (power 2 16)) (power 2 8)
      o4 = mod maskVal (power 2 8)
  in (o1, o2, o3, o4)

||| Number of host addresses in a CIDR block
public export
hostCount : PrefixLength -> Nat
hostCount (MkPrefix n) =
  if n >= 31
    then power 2 (minus 32 n)
    else minus (power 2 (minus 32 n)) 2

||| Calculate network address (apply mask)
public export
networkAddress : CIDRBlock -> (Nat, Nat, Nat, Nat)
networkAddress cidr =
  let (a, b, c, d) = network cidr
      (m1, m2, m3, m4) = subnetMask (prefix cidr)
  in (min a m1, min b m2, min c m3, min d m4)

||| Calculate broadcast address
public export
broadcastAddress : CIDRBlock -> (Nat, Nat, Nat, Nat)
broadcastAddress cidr =
  let (a, b, c, d) = networkAddress cidr
      (m1, m2, m3, m4) = subnetMask (prefix cidr)
      inv1 = minus 255 m1
      inv2 = minus 255 m2
      inv3 = minus 255 m3
      inv4 = minus 255 m4
  in (a + inv1, b + inv2, c + inv3, d + inv4)

||| Convert IP tuple to a single Nat for comparison
public export
ipToNat : (Nat, Nat, Nat, Nat) -> Nat
ipToNat (a, b, c, d) = a * 16777216 + b * 65536 + c * 256 + d

||| Check if an IP address is contained in a CIDR block
public export
contains : CIDRBlock -> (Nat, Nat, Nat, Nat) -> Bool
contains cidr ip =
  let netAddr = ipToNat (networkAddress cidr)
      bcastAddr = ipToNat (broadcastAddress cidr)
      ipAddr = ipToNat ip
  in ipAddr >= netAddr && ipAddr <= bcastAddr

||| Check if one CIDR block is a subset of another
public export
isSubsetOf : CIDRBlock -> CIDRBlock -> Bool
isSubsetOf sub super =
  contains super (networkAddress sub) &&
  contains super (broadcastAddress sub)

||| Parse a CIDR string like "192.168.1.0/24"
public export
parseCIDR : String -> Maybe CIDRBlock
parseCIDR s =
  case break (== '/') (unpack s) of
    (ipChars, []) => Nothing
    (ipChars, '/' :: prefixChars) =>
      case parseIPv4 (pack ipChars) of
        Nothing => Nothing
        Just ip =>
          case parseNat (pack prefixChars) of
            Nothing => Nothing
            Just n => case mkPrefix n of
              Nothing => Nothing
              Just pfx => Just (MkCIDR ip pfx)
    _ => Nothing
  where
    parseIPv4 : String -> Maybe (Nat, Nat, Nat, Nat)
    parseIPv4 s =
      case split (== '.') s of
        [a, b, c, d] =>
          case (parsePositive a, parsePositive b, parsePositive c, parsePositive d) of
            (Just a', Just b', Just c', Just d') =>
              if a' <= 255 && b' <= 255 && c' <= 255 && d' <= 255
                then Just (a', b', c', d')
                else Nothing
            _ => Nothing
        _ => Nothing

    parseNat : String -> Maybe Nat
    parseNat s = parsePositive s

||| First usable host address
public export
firstHost : CIDRBlock -> Maybe (Nat, Nat, Nat, Nat)
firstHost cidr =
  if prefixValue (prefix cidr) >= 31
    then Nothing
    else let (a, b, c, d) = networkAddress cidr
         in Just (a, b, c, if d < 255 then S d else d)

||| Last usable host address
public export
lastHost : CIDRBlock -> Maybe (Nat, Nat, Nat, Nat)
lastHost cidr =
  if prefixValue (prefix cidr) >= 31
    then Nothing
    else let (a, b, c, d) = broadcastAddress cidr
         in Just (a, b, c, minus d 1)

||| Check if CIDR represents a private network (RFC 1918)
public export
isPrivate : CIDRBlock -> Bool
isPrivate cidr =
  let (a, b, _, _) = networkAddress cidr
  in a == 10 ||
     (a == 172 && b >= 16 && b <= 31) ||
     (a == 192 && b == 168)

||| Check if CIDR represents a loopback network
public export
isLoopback : CIDRBlock -> Bool
isLoopback cidr =
  let (a, _, _, _) = networkAddress cidr
  in a == 127

||| Check if CIDR represents a link-local network
public export
isLinkLocal : CIDRBlock -> Bool
isLinkLocal cidr =
  let (a, b, _, _) = networkAddress cidr
  in a == 169 && b == 254

||| Proof that a valid CIDR has prefix <= 32
public export
data ValidCIDR : CIDRBlock -> Type where
  MkValidCIDR : (cidr : CIDRBlock) -> ValidCIDR cidr

||| Proof that an IP is within a CIDR block
public export
data IPInCIDR : CIDRBlock -> (Nat, Nat, Nat, Nat) -> Type where
  MkIPInCIDR : contains cidr ip = True -> IPInCIDR cidr ip
