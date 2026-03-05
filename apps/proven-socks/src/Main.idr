-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-socks skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import SOCKS

%default total

||| All SOCKS5 authentication method constructors for demonstration.
allAuthMethods : List AuthMethod
allAuthMethods = [NoAuth, GSSAPI, UsernamePassword, NoAcceptable]

||| All SOCKS5 command constructors for demonstration.
allCommands : List Command
allCommands = [Connect, Bind, UDPAssociate]

||| All SOCKS5 address type constructors for demonstration.
allAddressTypes : List AddressType
allAddressTypes = [IPv4, DomainName, IPv6]

||| All SOCKS5 reply code constructors for demonstration.
allReplies : List Reply
allReplies =
  [ Succeeded, GeneralFailure, NotAllowed, NetworkUnreachable
  , HostUnreachable, ConnectionRefused, TTLExpired
  , CommandNotSupported, AddressTypeNotSupported ]

||| All SOCKS5 connection state constructors for demonstration.
allStates : List State
allStates = [Initial, Authenticating, Authenticated, Connecting, Established, Closed]

main : IO ()
main = do
  putStrLn "proven-socks: RFC 1928 SOCKS5 Proxy"
  putStrLn $ "  SOCKS port:     " ++ show socksPort
  putStrLn $ "  Auth methods:   " ++ show allAuthMethods
  putStrLn $ "  Commands:       " ++ show allCommands
  putStrLn $ "  Address types:  " ++ show allAddressTypes
  putStrLn $ "  Reply codes:    " ++ show allReplies
  putStrLn $ "  States:         " ++ show allStates
