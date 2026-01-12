-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeNetworkUnit

import Proven.Core
import Proven.SafeNetwork

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

export
runNetworkUnitTests : IO ()
runNetworkUnitTests = do
  putStrLn "SafeNetwork Unit Tests"
  putStrLn "======================"

  -- IPv4 parsing tests
  putStrLn "\n[IPv4 Parsing]"
  assertOk "parseIPv4 \"192.168.1.1\"" (parseIPv4 "192.168.1.1")
  assertOk "parseIPv4 \"0.0.0.0\"" (parseIPv4 "0.0.0.0")
  assertOk "parseIPv4 \"255.255.255.255\"" (parseIPv4 "255.255.255.255")
  assertOk "parseIPv4 \"127.0.0.1\"" (parseIPv4 "127.0.0.1")
  assertErr "parseIPv4 \"256.1.1.1\" out of range" (parseIPv4 "256.1.1.1")
  assertErr "parseIPv4 \"192.168.1\" incomplete" (parseIPv4 "192.168.1")
  assertErr "parseIPv4 \"not.an.ip.addr\"" (parseIPv4 "not.an.ip.addr")
  assertErr "parseIPv4 \"\"" (parseIPv4 "")

  -- IPv4 classification tests
  putStrLn "\n[IPv4 Classification]"
  case parseIPv4 "127.0.0.1" of
    Ok ip => assertTrue "127.0.0.1 is loopback" (isLoopback ip)
    Err _ => putStrLn "  ✗ Failed to parse"
  case parseIPv4 "192.168.1.1" of
    Ok ip => assertTrue "192.168.1.1 is private" (isPrivate ip)
    Err _ => putStrLn "  ✗ Failed to parse"
  case parseIPv4 "10.0.0.1" of
    Ok ip => assertTrue "10.0.0.1 is private" (isPrivate ip)
    Err _ => putStrLn "  ✗ Failed to parse"
  case parseIPv4 "8.8.8.8" of
    Ok ip => assertTrue "8.8.8.8 is public" (isPublic ip)
    Err _ => putStrLn "  ✗ Failed to parse"

  -- IPv6 parsing tests
  putStrLn "\n[IPv6 Parsing]"
  assertOk "parseIPv6 \"::1\"" (parseIPv6 "::1")
  assertOk "parseIPv6 \"::\"" (parseIPv6 "::")
  assertOk "parseIPv6 \"2001:db8::1\"" (parseIPv6 "2001:db8::1")
  assertOk "parseIPv6 full form" (parseIPv6 "2001:0db8:85a3:0000:0000:8a2e:0370:7334")
  assertErr "parseIPv6 invalid" (parseIPv6 "not:valid:ipv6")
  assertErr "parseIPv6 too many groups" (parseIPv6 "1:2:3:4:5:6:7:8:9")

  -- CIDR parsing tests
  putStrLn "\n[CIDR Notation]"
  assertOk "parseCIDR \"192.168.1.0/24\"" (parseCIDR "192.168.1.0/24")
  assertOk "parseCIDR \"10.0.0.0/8\"" (parseCIDR "10.0.0.0/8")
  assertOk "parseCIDR \"0.0.0.0/0\"" (parseCIDR "0.0.0.0/0")
  assertErr "parseCIDR \"192.168.1.0/33\" invalid prefix" (parseCIDR "192.168.1.0/33")
  assertErr "parseCIDR \"192.168.1.0\" no prefix" (parseCIDR "192.168.1.0")

  -- CIDR containment tests
  putStrLn "\n[CIDR Containment]"
  case (parseCIDR "192.168.1.0/24", parseIPv4 "192.168.1.100") of
    (Ok cidr, Ok ip) => assertTrue "192.168.1.100 in 192.168.1.0/24" (contains cidr ip)
    _ => putStrLn "  ✗ Failed to parse"
  case (parseCIDR "192.168.1.0/24", parseIPv4 "192.168.2.1") of
    (Ok cidr, Ok ip) => assertTrue "192.168.2.1 not in 192.168.1.0/24" (not $ contains cidr ip)
    _ => putStrLn "  ✗ Failed to parse"

  -- Port validation tests
  putStrLn "\n[Port Validation]"
  assertOk "parsePort 80" (parsePort 80)
  assertOk "parsePort 443" (parsePort 443)
  assertOk "parsePort 8080" (parsePort 8080)
  assertOk "parsePort 1" (parsePort 1)
  assertOk "parsePort 65535" (parsePort 65535)
  assertErr "parsePort 0" (parsePort 0)
  assertErr "parsePort 65536" (parsePort 65536)
  assertErr "parsePort -1" (parsePort (-1))

  -- Port classification tests
  putStrLn "\n[Port Classification]"
  case parsePort 22 of
    Ok p => assertTrue "port 22 is well-known" (isWellKnown p)
    Err _ => putStrLn "  ✗ Failed to parse"
  case parsePort 8080 of
    Ok p => assertTrue "port 8080 is registered" (isRegistered p)
    Err _ => putStrLn "  ✗ Failed to parse"
  case parsePort 50000 of
    Ok p => assertTrue "port 50000 is dynamic" (isDynamic p)
    Err _ => putStrLn "  ✗ Failed to parse"

  -- Well-known services tests
  putStrLn "\n[Well-Known Services]"
  assertEq "getService 80 = \"http\"" (Just "http") (getService 80)
  assertEq "getService 443 = \"https\"" (Just "https") (getService 443)
  assertEq "getService 22 = \"ssh\"" (Just "ssh") (getService 22)
  assertEq "getService 12345 = Nothing" Nothing (getService 12345)

  putStrLn "\n✓ SafeNetwork unit tests complete"
