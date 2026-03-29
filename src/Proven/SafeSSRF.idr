-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeSSRF - Server-Side Request Forgery prevention
|||
||| Validates that outbound request targets are not internal/private
||| network addresses. Blocks RFC 1918, loopback, link-local, multicast,
||| and other non-routable addresses to prevent SSRF (CWE-918).
module Proven.SafeSSRF

import Data.String
import Data.List
import Data.List1
import Data.Nat

%default total

-- ============================================================================
-- IP ADDRESS CLASSIFICATION
-- ============================================================================

||| Network address class for SSRF analysis
public export
data AddressClass =
    Loopback          -- 127.0.0.0/8, ::1
  | PrivateA          -- 10.0.0.0/8
  | PrivateB          -- 172.16.0.0/12
  | PrivateC          -- 192.168.0.0/16
  | LinkLocal         -- 169.254.0.0/16, fe80::/10
  | Multicast         -- 224.0.0.0/4, ff00::/8
  | Documentation     -- 192.0.2.0/24, 198.51.100.0/24, 203.0.113.0/24
  | BroadcastAddr     -- 255.255.255.255
  | CloudMetadata     -- 169.254.169.254 (AWS/GCP/Azure metadata)
  | IPv6Mapped        -- ::ffff:0:0/96
  | Routable          -- Public internet address

public export
Show AddressClass where
  show Loopback      = "loopback"
  show PrivateA      = "private-10/8"
  show PrivateB      = "private-172.16/12"
  show PrivateC      = "private-192.168/16"
  show LinkLocal     = "link-local"
  show Multicast     = "multicast"
  show Documentation = "documentation"
  show BroadcastAddr = "broadcast"
  show CloudMetadata = "cloud-metadata"
  show IPv6Mapped    = "ipv6-mapped"
  show Routable      = "routable"

public export
Eq AddressClass where
  Loopback == Loopback = True
  PrivateA == PrivateA = True
  PrivateB == PrivateB = True
  PrivateC == PrivateC = True
  LinkLocal == LinkLocal = True
  Multicast == Multicast = True
  Documentation == Documentation = True
  BroadcastAddr == BroadcastAddr = True
  CloudMetadata == CloudMetadata = True
  IPv6Mapped == IPv6Mapped = True
  Routable == Routable = True
  _ == _ = False

||| Whether an address class is considered internal/dangerous for SSRF
public export
isInternal : AddressClass -> Bool
isInternal Routable = False
isInternal _        = True

-- ============================================================================
-- IPv4 CLASSIFICATION
-- ============================================================================

||| Parse dotted-quad IPv4 into 4 octets
public export
parseIPv4Octets : String -> Maybe (Nat, Nat, Nat, Nat)
parseIPv4Octets s =
  case map (parsePositive {a=Nat}) (forget (split (== '.') s)) of
    [Just a, Just b, Just c, Just d] =>
      if a <= 255 && b <= 255 && c <= 255 && d <= 255
        then Just (a, b, c, d)
        else Nothing
    _ => Nothing

||| Classify an IPv4 address
public export
classifyIPv4 : (Nat, Nat, Nat, Nat) -> AddressClass
classifyIPv4 (127, _, _, _)     = Loopback
classifyIPv4 (10, _, _, _)      = PrivateA
classifyIPv4 (172, b, _, _)     = if b >= 16 && b <= 31 then PrivateB else Routable
classifyIPv4 (192, 168, _, _)   = PrivateC
classifyIPv4 (169, 254, 169, 254) = CloudMetadata
classifyIPv4 (169, 254, _, _)   = LinkLocal
classifyIPv4 (192, 0, 2, _)     = Documentation
classifyIPv4 (198, 51, 100, _)  = Documentation
classifyIPv4 (203, 0, 113, _)   = Documentation
classifyIPv4 (255, 255, 255, 255) = BroadcastAddr
classifyIPv4 (a, _, _, _)       = if a >= 224 && a <= 239 then Multicast else Routable

||| Classify an IPv4 string
public export
classifyIPv4String : String -> Maybe AddressClass
classifyIPv4String s = map classifyIPv4 (parseIPv4Octets s)

-- ============================================================================
-- SSRF VALIDATION
-- ============================================================================

||| SSRF check result
public export
data SSRFResult =
    Allowed String          -- URL is safe to request
  | Blocked AddressClass    -- Blocked due to internal address
  | InvalidAddress          -- Could not parse address

public export
Show SSRFResult where
  show (Allowed url) = "Allowed: " ++ url
  show (Blocked cls) = "Blocked: " ++ show cls
  show InvalidAddress = "Invalid address"

||| Extract host from a URL (simple extraction — scheme://host[:port]/...)
public export
extractHost : String -> Maybe String
extractHost url =
  if isPrefixOf "http://" url then extractAfterScheme 7 url
  else if isPrefixOf "https://" url then extractAfterScheme 8 url
  else Nothing
  where
    extractAfterScheme : Nat -> String -> Maybe String
    extractAfterScheme skip u =
      let rest = substr skip (length u) u
          host = pack (takeWhile (\c => c /= '/' && c /= ':' && c /= '?') (unpack rest))
      in if length host > 0 then Just host else Nothing

||| Validate a URL is safe from SSRF
public export
validateSSRF : String -> SSRFResult
validateSSRF url =
  case extractHost url of
    Nothing => InvalidAddress
    Just host =>
      case classifyIPv4String host of
        Just cls => if isInternal cls then Blocked cls else Allowed url
        Nothing  =>
          -- Not a bare IP — check for known dangerous hostnames
          if host == "localhost" || host == "localhost.localdomain"
            then Blocked Loopback
          else if isPrefixOf "0" host  -- 0.0.0.0, 0x7f...
            then Blocked Loopback
          else Allowed url  -- DNS name, allow (DNS rebinding is separate concern)

||| Quick check: is the URL SSRF-safe?
public export
isSSRFSafe : String -> Bool
isSSRFSafe url = case validateSSRF url of
  Allowed _ => True
  _         => False

-- ============================================================================
-- SSRF POLICY
-- ============================================================================

||| Configurable SSRF policy
public export
record SSRFPolicy where
  constructor MkSSRFPolicy
  blockPrivate     : Bool   -- Block RFC 1918
  blockLoopback    : Bool   -- Block 127.0.0.0/8
  blockLinkLocal   : Bool   -- Block 169.254.0.0/16
  blockMetadata    : Bool   -- Block cloud metadata endpoints
  allowedHosts     : List String  -- Explicit allowlist (bypasses IP checks)

||| Default strict policy (block everything internal)
public export
strictPolicy : SSRFPolicy
strictPolicy = MkSSRFPolicy True True True True []

||| Validate against a policy
public export
validateWithPolicy : SSRFPolicy -> String -> SSRFResult
validateWithPolicy policy url =
  case extractHost url of
    Nothing => InvalidAddress
    Just host =>
      -- Check allowlist first
      if any (== host) policy.allowedHosts
        then Allowed url
        else case classifyIPv4String host of
          Nothing =>
            if host == "localhost" && policy.blockLoopback
              then Blocked Loopback
              else Allowed url
          Just cls => case cls of
            Loopback      => if policy.blockLoopback then Blocked cls else Allowed url
            PrivateA      => if policy.blockPrivate then Blocked cls else Allowed url
            PrivateB      => if policy.blockPrivate then Blocked cls else Allowed url
            PrivateC      => if policy.blockPrivate then Blocked cls else Allowed url
            LinkLocal     => if policy.blockLinkLocal then Blocked cls else Allowed url
            CloudMetadata => if policy.blockMetadata then Blocked cls else Allowed url
            Routable      => Allowed url
            other         => Blocked other
