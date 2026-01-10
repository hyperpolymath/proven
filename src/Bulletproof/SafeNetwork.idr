-- SPDX-License-Identifier: Palimpsest-MPL
||| SafeNetwork - Type-safe network primitives
|||
||| This module provides:
||| - Type-safe IP address handling (IPv4, IPv6)
||| - CIDR notation with validation
||| - Port numbers with range checking
||| - MAC addresses
||| - Safe hostname validation
|||
||| Note: Network operations require FFI
module Bulletproof.SafeNetwork

import public Bulletproof.Core
import public Bulletproof.SafeNetwork.IPv4
import public Bulletproof.SafeNetwork.IPv6
import public Bulletproof.SafeNetwork.CIDR
import public Bulletproof.SafeNetwork.Port

import Data.List
import Data.String
import Data.Vect
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- IP Address Union Type
--------------------------------------------------------------------------------

||| Either IPv4 or IPv6 address
public export
data IPAddress
  = IPv4Addr IPv4
  | IPv6Addr IPv6

public export
Show IPAddress where
  show (IPv4Addr ip) = show ip
  show (IPv6Addr ip) = show ip

public export
Eq IPAddress where
  (IPv4Addr a) == (IPv4Addr b) = a == b
  (IPv6Addr a) == (IPv6Addr b) = a == b
  _ == _ = False

||| Parse any IP address
public export
parseIPAddress : String -> Maybe IPAddress
parseIPAddress s =
  case parseIPv4 s of
    Just ip => Just (IPv4Addr ip)
    Nothing => map IPv6Addr (parseIPv6 s)

||| Check if address is IPv4
public export
isIPv4 : IPAddress -> Bool
isIPv4 (IPv4Addr _) = True
isIPv4 _ = False

||| Check if address is IPv6
public export
isIPv6 : IPAddress -> Bool
isIPv6 (IPv6Addr _) = True
isIPv6 _ = False

--------------------------------------------------------------------------------
-- MAC Address Type
--------------------------------------------------------------------------------

||| 48-bit MAC address
public export
record MACAddress where
  constructor MkMAC
  octets : Vect 6 Bits8

public export
Show MACAddress where
  show (MkMAC octets) =
    let hexStrings = map toHex (toList octets)
    in joinBy ":" hexStrings
    where
      toHex : Bits8 -> String
      toHex b =
        let n = cast {to=Nat} b
            hi = n `div` 16
            lo = n `mod` 16
            hexChar : Nat -> Char
            hexChar x = if x < 10 then chr (ord '0' + cast x)
                                  else chr (ord 'a' + cast x - 10)
        in pack [hexChar hi, hexChar lo]

      joinBy : String -> List String -> String
      joinBy sep [] = ""
      joinBy sep [x] = x
      joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

public export
Eq MACAddress where
  (MkMAC a) == (MkMAC b) = toList a == toList b

||| Parse MAC address from string
public export
parseMAC : String -> Maybe MACAddress
parseMAC s =
  let parts = split (== ':') s ++ split (== '-') s
  in if length parts == 6
       then do
         octets <- traverse parseHexByte (take 6 parts)
         Just (MkMAC (fromList octets))
       else Nothing
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

    parseHexByte : String -> Maybe Bits8
    parseHexByte str =
      case unpack str of
        [h1, h2] => do
          n1 <- hexDigitValue h1
          n2 <- hexDigitValue h2
          Just (cast (n1 * 16 + n2))
        _ => Nothing

    hexDigitValue : Char -> Maybe Nat
    hexDigitValue c =
      if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
      else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
      else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
      else Nothing

||| Check if MAC is broadcast address
public export
isBroadcast : MACAddress -> Bool
isBroadcast (MkMAC octets) = all (== 0xFF) (toList octets)

||| Check if MAC is multicast
public export
isMulticast : MACAddress -> Bool
isMulticast (MkMAC octets) =
  case toList octets of
    (first :: _) => (first `and` 0x01) /= 0
    _ => False

||| Check if MAC is locally administered
public export
isLocallyAdministered : MACAddress -> Bool
isLocallyAdministered (MkMAC octets) =
  case toList octets of
    (first :: _) => (first `and` 0x02) /= 0
    _ => False

--------------------------------------------------------------------------------
-- Hostname Type
--------------------------------------------------------------------------------

||| A validated hostname (RFC 1123)
public export
data Hostname : Type where
  MkHostname : (name : String) -> {auto valid : ValidHostname name} -> Hostname

||| Proof that a string is a valid hostname
public export
data ValidHostname : String -> Type where
  IsValidHostname : ValidHostname name

||| Parse and validate hostname
public export
parseHostname : String -> Maybe Hostname
parseHostname s =
  if isValidHostnameString s
    then Just (MkHostname s)
    else Nothing
  where
    isValidHostnameString : String -> Bool
    isValidHostnameString str =
      let chars = unpack str
          len = length chars
      in len >= 1 && len <= 253 &&
         all isValidChar chars &&
         not (isPrefixOf "-" str) &&
         not (isSuffixOf "-" str) &&
         all validLabel (split (== '.') str)

    isValidChar : Char -> Bool
    isValidChar c = isAlphaNum c || c == '-' || c == '.'

    validLabel : String -> Bool
    validLabel label =
      let len = length label
      in len >= 1 && len <= 63 &&
         not (isPrefixOf "-" label) &&
         not (isSuffixOf "-" label)

    split : (Char -> Bool) -> String -> List String
    split p str = go (unpack str) [] []
      where
        go : List Char -> List Char -> List String -> List String
        go [] current acc = reverse (pack (reverse current) :: acc)
        go (c :: cs) current acc =
          if p c
            then go cs [] (pack (reverse current) :: acc)
            else go cs (c :: current) acc

public export
Show Hostname where
  show (MkHostname name) = name

public export
Eq Hostname where
  (MkHostname a) == (MkHostname b) = toLower a == toLower b

--------------------------------------------------------------------------------
-- Socket Address Type
--------------------------------------------------------------------------------

||| Complete socket address (IP + port)
public export
record SocketAddress where
  constructor MkSocketAddr
  address : IPAddress
  port : Port

public export
Show SocketAddress where
  show (MkSocketAddr addr port) =
    case addr of
      IPv4Addr ip => show ip ++ ":" ++ show port
      IPv6Addr ip => "[" ++ show ip ++ "]:" ++ show port

||| Parse socket address from string
public export
parseSocketAddress : String -> Maybe SocketAddress
parseSocketAddress s =
  -- Try IPv4:port format
  case parseIPv4WithPort s of
    Just sa => Just sa
    Nothing => parseIPv6WithPort s
  where
    parseIPv4WithPort : String -> Maybe SocketAddress
    parseIPv4WithPort str =
      case break (== ':') (unpack str) of
        (ipChars, ':' :: portChars) => do
          ip <- parseIPv4 (pack ipChars)
          port <- parsePort (pack portChars)
          Just (MkSocketAddr (IPv4Addr ip) port)
        _ => Nothing

    parseIPv6WithPort : String -> Maybe SocketAddress
    parseIPv6WithPort str =
      case unpack str of
        ('[' :: rest) =>
          case break (== ']') rest of
            (ipChars, ']' :: ':' :: portChars) => do
              ip <- parseIPv6 (pack ipChars)
              port <- parsePort (pack portChars)
              Just (MkSocketAddr (IPv6Addr ip) port)
            _ => Nothing
        _ => Nothing

    break : (Char -> Bool) -> List Char -> (List Char, List Char)
    break p [] = ([], [])
    break p (c :: cs) =
      if p c then ([], c :: cs) else let (ys, zs) = break p cs in (c :: ys, zs)

--------------------------------------------------------------------------------
-- Network Utilities
--------------------------------------------------------------------------------

||| Check if IP is in private range
public export
isPrivateIP : IPAddress -> Bool
isPrivateIP (IPv4Addr ip) = isPrivateIPv4 ip
isPrivateIP (IPv6Addr ip) = isPrivateIPv6 ip

||| Check if IP is loopback
public export
isLoopback : IPAddress -> Bool
isLoopback (IPv4Addr (MkIPv4 127 _ _ _)) = True
isLoopback (IPv6Addr ip) = isLoopbackIPv6 ip
isLoopback _ = False

||| Check if IP is link-local
public export
isLinkLocal : IPAddress -> Bool
isLinkLocal (IPv4Addr ip) = isLinkLocalIPv4 ip
isLinkLocal (IPv6Addr ip) = isLinkLocalIPv6 ip

||| Get IP version
public export
ipVersion : IPAddress -> Nat
ipVersion (IPv4Addr _) = 4
ipVersion (IPv6Addr _) = 6

--------------------------------------------------------------------------------
-- DNS Record Types
--------------------------------------------------------------------------------

||| DNS record types
public export
data DNSRecordType
  = A           -- IPv4 address
  | AAAA        -- IPv6 address
  | CNAME       -- Canonical name
  | MX          -- Mail exchange
  | TXT         -- Text record
  | NS          -- Name server
  | SOA         -- Start of authority
  | PTR         -- Pointer (reverse DNS)
  | SRV         -- Service locator

public export
Show DNSRecordType where
  show A = "A"
  show AAAA = "AAAA"
  show CNAME = "CNAME"
  show MX = "MX"
  show TXT = "TXT"
  show NS = "NS"
  show SOA = "SOA"
  show PTR = "PTR"
  show SRV = "SRV"

--------------------------------------------------------------------------------
-- Protocol Types
--------------------------------------------------------------------------------

||| Transport layer protocols
public export
data Protocol = TCP | UDP | SCTP | ICMP

public export
Show Protocol where
  show TCP = "TCP"
  show UDP = "UDP"
  show SCTP = "SCTP"
  show ICMP = "ICMP"

public export
Eq Protocol where
  TCP == TCP = True
  UDP == UDP = True
  SCTP == SCTP = True
  ICMP == ICMP = True
  _ == _ = False

||| Protocol number
public export
protocolNumber : Protocol -> Nat
protocolNumber TCP = 6
protocolNumber UDP = 17
protocolNumber SCTP = 132
protocolNumber ICMP = 1

--------------------------------------------------------------------------------
-- Network Endpoint
--------------------------------------------------------------------------------

||| Complete network endpoint specification
public export
record Endpoint where
  constructor MkEndpoint
  host : Either Hostname IPAddress
  port : Port
  protocol : Protocol

public export
Show Endpoint where
  show (MkEndpoint host port proto) =
    let hostStr = case host of
                    Left h => show h
                    Right ip => show ip
    in show proto ++ "://" ++ hostStr ++ ":" ++ show port
