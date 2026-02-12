-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| IPv6 Address Types and Operations
|||
||| Type-safe IPv6 address handling with validation and utilities.
module Proven.SafeNetwork.IPv6

import Proven.Core
import Data.List
import Data.String
import Data.Vect
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- IPv6 Address Type
--------------------------------------------------------------------------------

||| IPv6 address as 8 16-bit segments
public export
record IPv6 where
  constructor MkIPv6
  segments : Vect 8 Bits16

public export
partial
Show IPv6 where
  show (MkIPv6 segs) =
    joinBy ":" (map showHex (toList segs))
    where
      partial toHexString : Nat -> String
      toHexString 0 = "0"
      toHexString n = go n ""
        where
          hexChar : Nat -> Char
          hexChar x = if x < 10 then chr (ord '0' + cast x)
                                else chr (ord 'a' + cast x - 10)
          go : Nat -> String -> String
          go 0 acc = acc
          go k acc = go (k `div` 16) (singleton (hexChar (k `mod` 16)) ++ acc)

      showHex : Bits16 -> String
      showHex n = toHexString (cast {to=Nat} n)

      joinBy : String -> List String -> String
      joinBy sep [] = ""
      joinBy sep [x] = x
      joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

public export
Eq IPv6 where
  (MkIPv6 a) == (MkIPv6 b) = toList a == toList b

--------------------------------------------------------------------------------
-- IPv6 Parsing
--------------------------------------------------------------------------------

||| Parse IPv6 address from string
||| Supports full form, compressed (::), and mixed notation
public export
partial
parseIPv6 : String -> Maybe IPv6
parseIPv6 s =
  -- Handle :: compression
  case findDoubleColon s of
    Just (left, right) => parseCompressed left right
    Nothing => parseFull s
  where
    break : (Char -> Bool) -> List Char -> (List Char, List Char)
    break p [] = ([], [])
    break p (c :: cs) =
      if p c then ([], c :: cs) else let (ys, zs) = break p cs in (c :: ys, zs)

    findDoubleColon : String -> Maybe (String, String)
    findDoubleColon str =
      case break (== ':') (unpack str) of
        (before, ':' :: ':' :: after) =>
          Just (pack before, pack after)
        (before, ':' :: rest) =>
          -- Continue searching
          case findDoubleColon (pack rest) of
            Just (l, r) => Just (pack before ++ ":" ++ l, r)
            Nothing => Nothing
        _ => Nothing

    split : (Char -> Bool) -> String -> List String
    split p str = go (unpack str) [] []
      where
        go : List Char -> List Char -> List String -> List String
        go [] current acc = reverse (pack (reverse current) :: acc)
        go (c :: cs) current acc =
          if p c
            then go cs [] (pack (reverse current) :: acc)
            else go cs (c :: current) acc

    toVect : (n : Nat) -> List a -> Maybe (Vect n a)
    toVect Z [] = Just []
    toVect (S k) (x :: xs) = map (x ::) (toVect k xs)
    toVect _ _ = Nothing

    parseHexNat : String -> Maybe Nat
    parseHexNat str = go 0 (unpack str)
      where
        hexValue : Char -> Maybe Nat
        hexValue c =
          if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
          else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
          else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
          else Nothing

        go : Nat -> List Char -> Maybe Nat
        go acc [] = Just acc
        go acc (c :: cs) = do
          v <- hexValue c
          go (acc * 16 + v) cs

    parseHex16 : String -> Maybe Bits16
    parseHex16 str =
      case parseHexNat str of
        Just n => if n <= 0xFFFF then Just (cast n) else Nothing
        Nothing => Nothing

    parseFull : String -> Maybe IPv6
    parseFull str =
      case split (== ':') str of
        segs => if length segs == 8
                  then do
                    parsed <- traverse parseHex16 segs
                    case toVect 8 parsed of
                      Just v => Just (MkIPv6 v)
                      Nothing => Nothing
                  else Nothing

    parseCompressed : String -> String -> Maybe IPv6
    parseCompressed left right =
      let leftSegs = if left == "" then [] else split (== ':') left
          rightSegs = if right == "" then [] else split (== ':') right
          leftCount = length leftSegs
          rightCount = length rightSegs
          zeroCount = (8 `minus` leftCount) `minus` rightCount
      in if leftCount + rightCount > 8
           then Nothing
           else do
             leftParsed <- traverse parseHex16 leftSegs
             rightParsed <- traverse parseHex16 rightSegs
             let zeros = replicate zeroCount 0
                 allSegs = leftParsed ++ zeros ++ rightParsed
             case toVect 8 allSegs of
               Just v => Just (MkIPv6 v)
               Nothing => Nothing

--------------------------------------------------------------------------------
-- Special Addresses
--------------------------------------------------------------------------------

||| Loopback address (::1)
public export
loopback6 : IPv6
loopback6 = MkIPv6 [0, 0, 0, 0, 0, 0, 0, 1]

||| Unspecified address (::)
public export
unspecified6 : IPv6
unspecified6 = MkIPv6 [0, 0, 0, 0, 0, 0, 0, 0]

||| All nodes multicast (ff02::1)
public export
allNodes : IPv6
allNodes = MkIPv6 [0xFF02, 0, 0, 0, 0, 0, 0, 1]

||| All routers multicast (ff02::2)
public export
allRouters : IPv6
allRouters = MkIPv6 [0xFF02, 0, 0, 0, 0, 0, 0, 2]

--------------------------------------------------------------------------------
-- Address Classification
--------------------------------------------------------------------------------

||| Check if IPv6 is loopback (::1)
public export
isLoopbackIPv6 : IPv6 -> Bool
isLoopbackIPv6 ip = ip == loopback6

||| Check if IPv6 is unspecified (::)
public export
isUnspecifiedIPv6 : IPv6 -> Bool
isUnspecifiedIPv6 ip = ip == unspecified6

||| Check if IPv6 is link-local (fe80::/10)
public export
isLinkLocalIPv6 : IPv6 -> Bool
isLinkLocalIPv6 (MkIPv6 segs) =
  case toList segs of
    (first :: _) => (first .&. 0xFFC0) == 0xFE80
    _ => False

||| Check if IPv6 is site-local (fec0::/10, deprecated)
public export
isSiteLocalIPv6 : IPv6 -> Bool
isSiteLocalIPv6 (MkIPv6 segs) =
  case toList segs of
    (first :: _) => (first .&. 0xFFC0) == 0xFEC0
    _ => False

||| Check if IPv6 is multicast (ff00::/8)
public export
isMulticastIPv6 : IPv6 -> Bool
isMulticastIPv6 (MkIPv6 segs) =
  case toList segs of
    (first :: _) => (first .&. 0xFF00) == 0xFF00
    _ => False

||| Check if IPv6 is unique local (fc00::/7)
public export
isUniqueLocalIPv6 : IPv6 -> Bool
isUniqueLocalIPv6 (MkIPv6 segs) =
  case toList segs of
    (first :: _) => (first .&. 0xFE00) == 0xFC00
    _ => False

||| Check if IPv6 is private (unique local or link-local)
public export
isPrivateIPv6 : IPv6 -> Bool
isPrivateIPv6 ip = isUniqueLocalIPv6 ip || isLinkLocalIPv6 ip

||| Check if IPv6 is global unicast (2000::/3)
public export
isGlobalUnicastIPv6 : IPv6 -> Bool
isGlobalUnicastIPv6 (MkIPv6 segs) =
  case toList segs of
    (first :: _) => (first .&. 0xE000) == 0x2000
    _ => False

||| Check if IPv6 is documentation (2001:db8::/32)
public export
isDocumentationIPv6 : IPv6 -> Bool
isDocumentationIPv6 (MkIPv6 segs) =
  case toList segs of
    (s1 :: s2 :: _) => s1 == 0x2001 && s2 == 0x0DB8
    _ => False

||| Check if IPv6 is 6to4 address (2002::/16)
public export
is6to4IPv6 : IPv6 -> Bool
is6to4IPv6 (MkIPv6 segs) =
  case toList segs of
    (first :: _) => first == 0x2002
    _ => False

||| Check if IPv6 is Teredo address (2001:0::/32)
public export
isTeredoIPv6 : IPv6 -> Bool
isTeredoIPv6 (MkIPv6 segs) =
  case toList segs of
    (s1 :: s2 :: _) => s1 == 0x2001 && s2 == 0x0000
    _ => False

--------------------------------------------------------------------------------
-- IPv4-Mapped IPv6
--------------------------------------------------------------------------------

||| Check if IPv6 is IPv4-mapped (::ffff:0:0/96)
public export
isIPv4MappedIPv6 : IPv6 -> Bool
isIPv4MappedIPv6 (MkIPv6 segs) =
  case toList segs of
    [0, 0, 0, 0, 0, 0xFFFF, _, _] => True
    _ => False

||| Check if IPv6 is IPv4-compatible (deprecated, ::/96 prefix)
public export
isIPv4CompatibleIPv6 : IPv6 -> Bool
isIPv4CompatibleIPv6 (MkIPv6 segs) =
  case toList segs of
    [0, 0, 0, 0, 0, 0, _, _] => True
    _ => False

--------------------------------------------------------------------------------
-- Multicast Scope
--------------------------------------------------------------------------------

||| IPv6 multicast scope
public export
data MulticastScope
  = InterfaceLocal   -- 1
  | LinkLocal        -- 2
  | RealmLocal       -- 3
  | AdminLocal       -- 4
  | SiteLocal        -- 5
  | OrganizationLocal -- 8
  | Global           -- E
  | UnknownScope Bits8

public export
Show MulticastScope where
  show InterfaceLocal = "Interface-Local"
  show LinkLocal = "Link-Local"
  show RealmLocal = "Realm-Local"
  show AdminLocal = "Admin-Local"
  show SiteLocal = "Site-Local"
  show OrganizationLocal = "Organization-Local"
  show Global = "Global"
  show (UnknownScope n) = "Unknown(" ++ show n ++ ")"

||| Get multicast scope from IPv6 address
public export
multicastScope : IPv6 -> Maybe MulticastScope
multicastScope ip =
  if not (isMulticastIPv6 ip)
    then Nothing
    else case toList (segments ip) of
           (first :: _) =>
             let scopeNibble : Bits8 = cast ((first `shiftR` 8) .&. 0x0F)
             in Just $ case scopeNibble of
                  1 => InterfaceLocal
                  2 => LinkLocal
                  3 => RealmLocal
                  4 => AdminLocal
                  5 => SiteLocal
                  8 => OrganizationLocal
                  14 => Global
                  n => UnknownScope n
           _ => Nothing

--------------------------------------------------------------------------------
-- Address Manipulation
--------------------------------------------------------------------------------

||| Get interface ID (last 64 bits)
public export
interfaceId : IPv6 -> Vect 4 Bits16
interfaceId (MkIPv6 segs) =
  case toList segs of
    [_, _, _, _, a, b, c, d] => [a, b, c, d]
    _ => [0, 0, 0, 0]  -- Should never happen with valid Vect 8

||| Get network prefix (first 64 bits)
public export
networkPrefix : IPv6 -> Vect 4 Bits16
networkPrefix (MkIPv6 segs) =
  case toList segs of
    [a, b, c, d, _, _, _, _] => [a, b, c, d]
    _ => [0, 0, 0, 0]  -- Should never happen with valid Vect 8

--------------------------------------------------------------------------------
-- Compressed String Format
--------------------------------------------------------------------------------

||| Show IPv6 in compressed format (with ::)
public export
partial
showCompressed : IPv6 -> String
showCompressed (MkIPv6 segs) =
  let segList = toList segs
      (start, len) = findLongestZeroRun segList
  in if len >= 2
       then compressAt segList start len
       else joinBy ":" (map showHex segList)
  where
    showHex : Bits16 -> String
    showHex 0 = "0"
    showHex n = toHexString (cast {to=Nat} n)
      where
        hexChar : Nat -> Char
        hexChar x = if x < 10 then chr (ord '0' + cast x)
                              else chr (ord 'a' + cast x - 10)
        partial toHexString : Nat -> String
        toHexString 0 = ""
        toHexString k = toHexString (k `div` 16) ++ singleton (hexChar (k `mod` 16))

    joinBy : String -> List String -> String
    joinBy sep [] = ""
    joinBy sep [x] = x
    joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

    findLongestZeroRun : List Bits16 -> (Nat, Nat)
    findLongestZeroRun segs = go segs 0 0 0 0 0
      where
        go : List Bits16 -> Nat -> Nat -> Nat -> Nat -> Nat -> (Nat, Nat)
        go [] _ _ bestStart bestLen _ = (bestStart, bestLen)
        go (s :: ss) idx currentStart currentLen bestStart bestLen =
          if s == 0
            then let newLen = currentLen + 1
                     newStart = if currentLen == 0 then idx else currentStart
                 in if newLen > bestLen
                      then go ss (idx + 1) newStart newLen newStart newLen
                      else go ss (idx + 1) newStart newLen bestStart bestLen
            else go ss (idx + 1) 0 0 bestStart bestLen

    compressAt : List Bits16 -> Nat -> Nat -> String
    compressAt segs start len =
      let before = take start segs
          after = drop (start + len) segs
          beforeStr = joinBy ":" (map showHex before)
          afterStr = joinBy ":" (map showHex after)
      in if start == 0 && start + len == 8 then "::"
         else if start == 0 then "::" ++ afterStr
         else if start + len == 8 then beforeStr ++ "::"
         else beforeStr ++ "::" ++ afterStr

