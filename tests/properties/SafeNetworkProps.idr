-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeNetworkProps

import Proven.Core
import Proven.SafeNetwork
import Data.Maybe
import System

%default total

-- The real API is Maybe-returning: `parseIPv4`, `parseIPv6`, `parseCIDR`
-- (prefix validated by `mkPrefix`) and `mkPort : Nat -> Maybe Port`.
-- `isPrivateIP` takes the `IPAddress` union, so IPv4 values are wrapped
-- with `IPv4Addr`. The parsers go through `split`/`unpack`/`parsePositive`
-- on String, which do not reduce at the type level, so the parse
-- properties are execution checks in `runNetworkProperties`.

||| Property: a port in range is accepted
prop_validPort : map (\p => portValue p) (mkPort 8080) = Just 8080
prop_validPort = Refl

||| Property: port zero is accepted
prop_portZero : map (\p => portValue p) (mkPort 0) = Just 0
prop_portZero = Refl

||| Property: the maximum port 65535 is accepted
prop_portMax : map (\p => portValue p) (mkPort 65535) = Just 65535
prop_portMax = Refl

||| Property: 65536 is rejected
prop_portOverMax : isJust (mkPort 65536) = False
prop_portOverMax = Refl

||| Property: private IP detection (RFC 1918, 192.168.0.0/16)
prop_privateIP : isPrivateIP (IPv4Addr (MkIPv4 192 168 1 1)) = True
prop_privateIP = Refl

||| Property: 8.8.8.8 is not private
prop_publicIP : isPrivateIP (IPv4Addr (MkIPv4 8 8 8 8)) = False
prop_publicIP = Refl

||| Property: every address in 10.0.0.0/8 is private
prop_tenSlashEightPrivate : (b, c, d : Bits8) ->
                            isPrivateIP (IPv4Addr (MkIPv4 10 b c d)) = True
prop_tenSlashEightPrivate b c d = Refl

||| Property: every address in 192.168.0.0/16 is private
prop_192168Private : (c, d : Bits8) ->
                     isPrivateIP (IPv4Addr (MkIPv4 192 168 c d)) = True
prop_192168Private c d = Refl

||| Property: mkPrefix rejects 33 (CIDR prefix bound)
prop_prefix33Rejected : isJust (mkPrefix 33) = False
prop_prefix33Rejected = Refl

||| Property: mkPrefix accepts 24
prop_prefix24Accepted : map (\p => prefixValue p) (mkPrefix 24) = Just 24
prop_prefix24Accepted = Refl

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

||| Test runner for network properties
export
runNetworkProperties : IO ()
runNetworkProperties = do
  putStrLn "SafeNetwork Property Tests"
  putStrLn "=========================="
  putStrLn "prop_validPort: PASS (proven by type)"
  putStrLn "prop_portZero: PASS (proven by type)"
  putStrLn "prop_portMax: PASS (proven by type)"
  putStrLn "prop_portOverMax: PASS (proven by type)"
  putStrLn "prop_privateIP: PASS (proven by type)"
  putStrLn "prop_publicIP: PASS (proven by type)"
  putStrLn "prop_tenSlashEightPrivate: PASS (proven by type, all addresses)"
  putStrLn "prop_192168Private: PASS (proven by type, all addresses)"
  putStrLn "prop_prefix33Rejected: PASS (proven by type)"
  putStrLn "prop_prefix24Accepted: PASS (proven by type)"
  check "valid IPv4 192.168.1.1 parses to its octets"
    (parseIPv4 "192.168.1.1" == Just (MkIPv4 192 168 1 1))
  check "invalid IPv4 256.1.1.1 rejected"
    (isNothing (parseIPv4 "256.1.1.1"))
  check "malformed IPv4 rejected (too few/many octets, empty, letters)"
    (isNothing (parseIPv4 "1.2.3") && isNothing (parseIPv4 "1.2.3.4.5") &&
     isNothing (parseIPv4 "") && isNothing (parseIPv4 "a.b.c.d"))
  check "localhost 127.0.0.1 parses to localhost"
    (parseIPv4 "127.0.0.1" == Just localhost)
  check "IPv6 ::1 parses to the loopback address"
    (map isLoopbackIPv6 (parseIPv6 "::1") == Just True)
  check "valid CIDR 192.168.0.0/24 parses with prefix 24"
    (map (prefixValue . pfxLen) (parseCIDR "192.168.0.0/24") == Just 24)
  check "CIDR prefix /33 rejected"
    (isNothing (parseCIDR "192.168.0.0/33"))
  check "parsePort agrees with mkPort at the boundaries"
    (map portValue (parsePort "8080") == Just 8080 &&
     map portValue (parsePort "65535") == Just 65535 &&
     isNothing (parsePort "65536"))
  putStrLn ""
