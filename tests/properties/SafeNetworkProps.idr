-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeNetworkProps

import Proven.Core
import Proven.SafeNetwork

%default total

||| Property: Valid IPv4 parses
prop_validIPv4 : isOk (parseIPv4 "192.168.1.1") = True
prop_validIPv4 = Refl

||| Property: Invalid IPv4 fails
prop_invalidIPv4 : isErr (parseIPv4 "256.1.1.1") = True
prop_invalidIPv4 = Refl

||| Property: Localhost IPv4 parses
prop_localhost : isOk (parseIPv4 "127.0.0.1") = True
prop_localhost = Refl

||| Property: Valid IPv6 parses
prop_validIPv6 : isOk (parseIPv6 "::1") = True
prop_validIPv6 = Refl

||| Property: Valid CIDR parses
prop_validCIDR : isOk (parseCIDR "192.168.0.0/24") = True
prop_validCIDR = Refl

||| Property: Invalid CIDR prefix fails
prop_invalidCIDRPrefix : isErr (parseCIDR "192.168.0.0/33") = True
prop_invalidCIDRPrefix = Refl

||| Property: Valid port in range
prop_validPort : isOk (validatePort 8080) = True
prop_validPort = Refl

||| Property: Port zero is valid
prop_portZero : isOk (validatePort 0) = True
prop_portZero = Refl

||| Property: Port max is valid (65535)
prop_portMax : isOk (validatePort 65535) = True
prop_portMax = Refl

||| Property: Port over max fails
prop_portOverMax : isErr (validatePort 65536) = True
prop_portOverMax = Refl

||| Property: Private IP detection
prop_privateIP : isPrivateIP (MkIPv4 192 168 1 1) = True
prop_privateIP = Refl

||| Property: Public IP detection
prop_publicIP : isPrivateIP (MkIPv4 8 8 8 8) = False
prop_publicIP = Refl

||| Test runner for network properties
export
runNetworkProperties : IO ()
runNetworkProperties = do
  putStrLn "SafeNetwork Property Tests"
  putStrLn "=========================="
  putStrLn "prop_validIPv4: PASS (proven by type)"
  putStrLn "prop_invalidIPv4: PASS (proven by type)"
  putStrLn "prop_localhost: PASS (proven by type)"
  putStrLn "prop_validIPv6: PASS (proven by type)"
  putStrLn "prop_validCIDR: PASS (proven by type)"
  putStrLn "prop_invalidCIDRPrefix: PASS (proven by type)"
  putStrLn "prop_validPort: PASS (proven by type)"
  putStrLn "prop_portZero: PASS (proven by type)"
  putStrLn "prop_portMax: PASS (proven by type)"
  putStrLn "prop_portOverMax: PASS (proven by type)"
  putStrLn "prop_privateIP: PASS (proven by type)"
  putStrLn "prop_publicIP: PASS (proven by type)"
  putStrLn ""
