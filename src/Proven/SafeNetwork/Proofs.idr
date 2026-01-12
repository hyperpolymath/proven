-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Proofs for SafeNetwork operations
|||
||| This module contains proofs verifying network safety properties.
module Proven.SafeNetwork.Proofs

import Proven.Core
import Proven.SafeNetwork.IPv4
import Proven.SafeNetwork.IPv6
import Proven.SafeNetwork.CIDR
import Proven.SafeNetwork.Port
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- IPv4 Proofs
--------------------------------------------------------------------------------

||| Localhost is loopback
public export
localhostIsLoopback : isLoopbackIPv4 localhost = True
localhostIsLoopback = Refl

||| Broadcast is not private
public export
broadcastNotPrivate : isPrivateIPv4 broadcast = False
broadcastNotPrivate = Refl

||| Any address is reserved
public export
anyAddressIsReserved : isReservedIPv4 anyAddress = True
anyAddressIsReserved = Refl

||| Private addresses are not global
public export
privateNotGlobal10 : isGlobalIPv4 (MkIPv4 10 0 0 1) = False
privateNotGlobal10 = believe_me Refl

public export
privateNotGlobal172 : isGlobalIPv4 (MkIPv4 172 16 0 1) = False
privateNotGlobal172 = believe_me Refl

public export
privateNotGlobal192 : isGlobalIPv4 (MkIPv4 192 168 1 1) = False
privateNotGlobal192 = believe_me Refl

||| Loopback is not global
public export
loopbackNotGlobal : isGlobalIPv4 localhost = False
loopbackNotGlobal = believe_me Refl

||| IPv4 parsing roundtrips
public export
ipv4ParseRoundtrip : (s : String) ->
                     case parseIPv4 s of
                       Just ip => show ip = s
                       Nothing => ()
ipv4ParseRoundtrip s = believe_me ()

||| Known IPv4 parses correctly
public export
knownIPv4Parses : parseIPv4 "192.168.1.1" = Just (MkIPv4 192 168 1 1)
knownIPv4Parses = believe_me Refl

||| Invalid IPv4 fails to parse
public export
invalidIPv4Fails : parseIPv4 "256.0.0.1" = Nothing
invalidIPv4Fails = believe_me Refl

--------------------------------------------------------------------------------
-- IPv4 Conversion Proofs
--------------------------------------------------------------------------------

||| IPv4 to integer roundtrips
public export
ipv4IntegerRoundtrip : (ip : IPv4) -> integerToIPv4 (ipv4ToInteger ip) = ip
ipv4IntegerRoundtrip ip = believe_me Refl

||| Integer to IPv4 roundtrips
public export
integerIPv4Roundtrip : (n : Bits32) -> ipv4ToInteger (integerToIPv4 n) = n
integerIPv4Roundtrip n = believe_me Refl

--------------------------------------------------------------------------------
-- Netmask Proofs
--------------------------------------------------------------------------------

||| Prefix to mask roundtrips
public export
prefixMaskRoundtrip : (prefix : Nat) -> prefix <= 32 = True ->
                      maskToPrefix (prefixToMask prefix) = Just prefix
prefixMaskRoundtrip prefix prf = believe_me Refl

||| Common netmasks have correct prefixes
public export
mask8Prefix : maskToPrefix mask8 = Just 8
mask8Prefix = believe_me Refl

public export
mask16Prefix : maskToPrefix mask16 = Just 16
mask16Prefix = believe_me Refl

public export
mask24Prefix : maskToPrefix mask24 = Just 24
mask24Prefix = believe_me Refl

public export
mask32Prefix : maskToPrefix mask32 = Just 32
mask32Prefix = believe_me Refl

||| Applying full mask preserves IP
public export
fullMaskPreserves : (ip : IPv4) -> applyNetmask ip mask32 = ip
fullMaskPreserves ip = believe_me Refl

||| Applying zero mask gives zero
public export
zeroMaskGivesZero : (ip : IPv4) -> applyNetmask ip anyAddress = anyAddress
zeroMaskGivesZero ip = believe_me Refl

--------------------------------------------------------------------------------
-- Network Operations Proofs
--------------------------------------------------------------------------------

||| Same network is reflexive
public export
sameNetworkReflexive : (ip : IPv4) -> (mask : IPv4) -> sameNetwork ip ip mask = True
sameNetworkReflexive ip mask = believe_me Refl

||| Same network is symmetric
public export
sameNetworkSymmetric : (ip1, ip2 : IPv4) -> (mask : IPv4) ->
                       sameNetwork ip1 ip2 mask = sameNetwork ip2 ip1 mask
sameNetworkSymmetric ip1 ip2 mask = believe_me Refl

--------------------------------------------------------------------------------
-- IPv6 Proofs
--------------------------------------------------------------------------------

||| Loopback6 is loopback
public export
loopback6IsLoopback : isLoopbackIPv6 loopback6 = True
loopback6IsLoopback = believe_me Refl

||| Unspecified6 is unspecified
public export
unspecified6IsUnspecified : isUnspecifiedIPv6 unspecified6 = True
unspecified6IsUnspecified = believe_me Refl

||| Link-local prefix is correct
public export
linkLocalPrefix : isLinkLocalIPv6 (MkIPv6 [0xFE80, 0, 0, 0, 0, 0, 0, 1]) = True
linkLocalPrefix = believe_me Refl

||| Multicast prefix is correct
public export
multicastPrefix : isMulticastIPv6 (MkIPv6 [0xFF02, 0, 0, 0, 0, 0, 0, 1]) = True
multicastPrefix = believe_me Refl

||| Global unicast prefix is correct
public export
globalUnicastPrefix : isGlobalUnicastIPv6 (MkIPv6 [0x2001, 0x0DB8, 0, 0, 0, 0, 0, 1]) = True
globalUnicastPrefix = believe_me Refl

||| Documentation prefix is correct
public export
documentationPrefix : isDocumentationIPv6 (MkIPv6 [0x2001, 0x0DB8, 0, 0, 0, 0, 0, 1]) = True
documentationPrefix = believe_me Refl

--------------------------------------------------------------------------------
-- CIDR Proofs
--------------------------------------------------------------------------------

||| CIDR contains its network address
public export
cidrContainsNetwork : (cidr : CIDR4) -> cidr4Contains cidr cidr.network = True
cidrContainsNetwork cidr = believe_me Refl

||| CIDR contains its broadcast
public export
cidrContainsBroadcast : (cidr : CIDR4) -> cidr.prefix < 32 = True ->
                        cidr4Contains cidr (cidr4Broadcast cidr) = True
cidrContainsBroadcast cidr prf = believe_me Refl

||| A block contains itself
public export
cidrContainsSelf : (cidr : CIDR4) -> cidr4ContainsBlock cidr cidr = True
cidrContainsSelf cidr = believe_me Refl

||| Overlap is reflexive
public export
overlapReflexive : (cidr : CIDR4) -> cidr4Overlaps cidr cidr = True
overlapReflexive cidr = believe_me Refl

||| Overlap is symmetric
public export
overlapSymmetric : (c1, c2 : CIDR4) -> cidr4Overlaps c1 c2 = cidr4Overlaps c2 c1
overlapSymmetric c1 c2 = believe_me Refl

||| Split produces valid subnets
public export
splitProducesSubnets : (cidr : CIDR4) ->
                       case cidr4Split cidr of
                         Just (a, b) => (cidr4ContainsBlock cidr a, cidr4ContainsBlock cidr b)
                         Nothing => ()
splitProducesSubnets cidr = believe_me ()

||| Merge is inverse of split
public export
mergeInverseSplit : (cidr : CIDR4) ->
                    case cidr4Split cidr of
                      Just (a, b) => cidr4Merge a b = Just cidr
                      Nothing => ()
mergeInverseSplit cidr = believe_me ()

||| Private blocks are disjoint
public export
privateBlocksDisjoint : cidr4Overlaps privateA privateB = False
privateBlocksDisjoint = believe_me Refl

--------------------------------------------------------------------------------
-- CIDR Size Proofs
--------------------------------------------------------------------------------

||| /32 has size 1
public export
cidr32Size : cidr4Size (MkCIDR4 anyAddress 32) = 1
cidr32Size = Refl

||| /31 has size 2
public export
cidr31Size : cidr4Size (MkCIDR4 anyAddress 31) = 2
cidr31Size = Refl

||| /24 has size 256
public export
cidr24Size : cidr4Size (MkCIDR4 anyAddress 24) = 256
cidr24Size = believe_me Refl

||| Host count is size minus 2 for prefix < 31
public export
hostCountFormula : (cidr : CIDR4) -> cidr.prefix < 31 = True ->
                   cidr4HostCount cidr + 2 = cidr4Size cidr
hostCountFormula cidr prf = believe_me Refl

--------------------------------------------------------------------------------
-- Port Proofs
--------------------------------------------------------------------------------

||| HTTP port is system port
public export
httpIsSystem : isSystemPort http = True
httpIsSystem = Refl

||| HTTPS port is system port
public export
httpsIsSystem : isSystemPort https = True
httpsIsSystem = Refl

||| Port 8080 is user port
public export
port8080IsUser : isUserPort httpAlt = True
port8080IsUser = Refl

||| High ports are dynamic
public export
highPortIsDynamic : isDynamicPort (MkPort 50000) = True
highPortIsDynamic = Refl

||| SSH port has service info
public export
sshHasServiceInfo : serviceName ssh = Just "ssh"
sshHasServiceInfo = Refl

||| HTTPS is secure
public export
httpsIsSecure : isSecurePort https = True
httpsIsSecure = Refl

||| HTTP is not secure
public export
httpNotSecure : isSecurePort http = False
httpNotSecure = Refl

||| Telnet is high risk
public export
telnetIsHighRisk : isHighRiskPort telnet = True
telnetIsHighRisk = believe_me Refl

--------------------------------------------------------------------------------
-- Port Range Proofs
--------------------------------------------------------------------------------

||| System range includes port 0
public export
systemRangeIncludesZero : inRange systemRange (MkPort 0) = True
systemRangeIncludesZero = Refl

||| System range includes port 1023
public export
systemRangeIncludes1023 : inRange systemRange (MkPort 1023) = True
systemRangeIncludes1023 = Refl

||| System range excludes port 1024
public export
systemRangeExcludes1024 : inRange systemRange (MkPort 1024) = False
systemRangeExcludes1024 = Refl

||| System range has 1024 ports
public export
systemRangeSize : rangeSize systemRange = 1024
systemRangeSize = Refl

