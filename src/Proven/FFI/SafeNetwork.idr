-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeNetwork operations
|||
||| This module exports network primitives to the C ABI via Idris2's RefC backend.
||| All functions are proven total and validate network addresses.
|||
||| Return conventions:
||| - Address parsing → (status: Int, address: String)
|||   - status = 0: Valid, address is normalized
|||   - status = 1: Invalid, address is empty
||| - Classification → Int (0 = false, 1 = true)
||| - Port validation → (status: Int, error: String)
|||
||| CRITICAL: Always validate IP addresses and ports from untrusted sources.
module Proven.FFI.SafeNetwork

import Proven.SafeNetwork
import Proven.SafeNetwork.IPv4
import Proven.SafeNetwork.IPv6
import Proven.SafeNetwork.CIDR
import Proven.SafeNetwork.Port
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode Maybe IPv4 as (status, address)
encodeMaybeIPv4 : Maybe IPv4 -> (Int, String)
encodeMaybeIPv4 Nothing = (1, "")
encodeMaybeIPv4 (Just ip) = (0, show ip)

||| Encode Maybe IPv6 as (status, address)
encodeMaybeIPv6 : Maybe IPv6 -> (Int, String)
encodeMaybeIPv6 Nothing = (1, "")
encodeMaybeIPv6 (Just ip) = (0, show ip)

||| Encode Maybe Port as (status, error)
encodeMaybePort : Maybe Port -> (Int, String)
encodeMaybePort Nothing = (1, "Invalid port number")
encodeMaybePort (Just port) = (0, show (portToNat port))

||| Encode Maybe MACAddress as (status, address)
encodeMaybeMAC : Maybe MACAddress -> (Int, String)
encodeMaybeMAC Nothing = (1, "")
encodeMaybeMAC (Just mac) = (0, show mac)

||| Encode Maybe Hostname as (status, name)
encodeMaybeHostname : Maybe Hostname -> (Int, String)
encodeMaybeHostname Nothing = (1, "Invalid hostname")
encodeMaybeHostname (Just (MkHostname name)) = (0, name)

--------------------------------------------------------------------------------
-- IPv4 Operations
--------------------------------------------------------------------------------

export
proven_idris_ipv4_parse : String -> (Int, String)
proven_idris_ipv4_parse s = encodeMaybeIPv4 (parseIPv4 s)

export
proven_idris_ipv4_is_private : String -> Int
proven_idris_ipv4_is_private s =
  case parseIPv4 s of
    Nothing => 0
    Just ip => encodeBool (isPrivateIPv4 ip)

export
proven_idris_ipv4_is_loopback : String -> Int
proven_idris_ipv4_is_loopback s =
  case parseIPv4 s of
    Nothing => 0
    Just ip => encodeBool (isLoopbackIPv4 ip)

export
proven_idris_ipv4_is_link_local : String -> Int
proven_idris_ipv4_is_link_local s =
  case parseIPv4 s of
    Nothing => 0
    Just ip => encodeBool (isLinkLocalIPv4 ip)

export
proven_idris_ipv4_is_multicast : String -> Int
proven_idris_ipv4_is_multicast s =
  case parseIPv4 s of
    Nothing => 0
    Just ip => encodeBool (isMulticastIPv4 ip)

export
proven_idris_ipv4_is_broadcast : String -> Int
proven_idris_ipv4_is_broadcast s =
  case parseIPv4 s of
    Nothing => 0
    Just ip => ip == broadcast

export
proven_idris_ipv4_is_global : String -> Int
proven_idris_ipv4_is_global s =
  case parseIPv4 s of
    Nothing => 0
    Just ip => encodeBool (isGlobalIPv4 ip)

export
proven_idris_ipv4_is_documentation : String -> Int
proven_idris_ipv4_is_documentation s =
  case parseIPv4 s of
    Nothing => 0
    Just ip => encodeBool (isDocumentationIPv4 ip)

export
proven_idris_ipv4_is_benchmarking : String -> Int
proven_idris_ipv4_is_benchmarking s =
  case parseIPv4 s of
    Nothing => 0
    Just ip => encodeBool (isBenchmarkingIPv4 ip)

--------------------------------------------------------------------------------
-- IPv6 Operations
--------------------------------------------------------------------------------

export
proven_idris_ipv6_parse : String -> (Int, String)
proven_idris_ipv6_parse s = encodeMaybeIPv6 (parseIPv6 s)

export
proven_idris_ipv6_is_loopback : String -> Int
proven_idris_ipv6_is_loopback s =
  case parseIPv6 s of
    Nothing => 0
    Just ip => encodeBool (isLoopbackIPv6 ip)

export
proven_idris_ipv6_is_link_local : String -> Int
proven_idris_ipv6_is_link_local s =
  case parseIPv6 s of
    Nothing => 0
    Just ip => encodeBool (isLinkLocalIPv6 ip)

export
proven_idris_ipv6_is_unique_local : String -> Int
proven_idris_ipv6_is_unique_local s =
  case parseIPv6 s of
    Nothing => 0
    Just ip => encodeBool (isUniqueLocalIPv6 ip)

export
proven_idris_ipv6_is_global : String -> Int
proven_idris_ipv6_is_global s =
  case parseIPv6 s of
    Nothing => 0
    Just ip => encodeBool (isGlobalIPv6 ip)

export
proven_idris_ipv6_is_multicast : String -> Int
proven_idris_ipv6_is_multicast s =
  case parseIPv6 s of
    Nothing => 0
    Just ip => encodeBool (isMulticastIPv6 ip)

export
proven_idris_ipv6_is_documentation : String -> Int
proven_idris_ipv6_is_documentation s =
  case parseIPv6 s of
    Nothing => 0
    Just ip => encodeBool (isDocumentationIPv6 ip)

--------------------------------------------------------------------------------
-- IP Address (Either IPv4 or IPv6)
--------------------------------------------------------------------------------

export
proven_idris_ip_parse : String -> (Int, String)
proven_idris_ip_parse s =
  case parseIPAddress s of
    Nothing => (1, "")
    Just addr => (0, show addr)

export
proven_idris_ip_is_ipv4 : String -> Int
proven_idris_ip_is_ipv4 s =
  case parseIPAddress s of
    Nothing => 0
    Just addr => encodeBool (isIPv4 addr)

export
proven_idris_ip_is_ipv6 : String -> Int
proven_idris_ip_is_ipv6 s =
  case parseIPAddress s of
    Nothing => 0
    Just addr => encodeBool (isIPv6 addr)

--------------------------------------------------------------------------------
-- Port Operations
--------------------------------------------------------------------------------

export
proven_idris_port_parse : String -> (Int, String)
proven_idris_port_parse s =
  case parsePositive s of
    Just n => encodeMaybePort (mkPort n)
    Nothing => (1, "Invalid port number")

export
proven_idris_port_is_well_known : Int -> Int
proven_idris_port_is_well_known n =
  case mkPort (cast n) of
    Nothing => 0
    Just port => encodeBool (isWellKnownPort port)

export
proven_idris_port_is_registered : Int -> Int
proven_idris_port_is_registered n =
  case mkPort (cast n) of
    Nothing => 0
    Just port => encodeBool (isRegisteredPort port)

export
proven_idris_port_is_ephemeral : Int -> Int
proven_idris_port_is_ephemeral n =
  case mkPort (cast n) of
    Nothing => 0
    Just port => encodeBool (isEphemeralPort port)

export
proven_idris_port_is_privileged : Int -> Int
proven_idris_port_is_privileged n =
  case mkPort (cast n) of
    Nothing => 0
    Just port => encodeBool (isPrivilegedPort port)

--------------------------------------------------------------------------------
-- MAC Address Operations
--------------------------------------------------------------------------------

export
proven_idris_mac_parse : String -> (Int, String)
proven_idris_mac_parse s = encodeMaybeMAC (parseMAC s)

export
proven_idris_mac_is_broadcast : String -> Int
proven_idris_mac_is_broadcast s =
  case parseMAC s of
    Nothing => 0
    Just mac => encodeBool (isBroadcast mac)

export
proven_idris_mac_is_multicast : String -> Int
proven_idris_mac_is_multicast s =
  case parseMAC s of
    Nothing => 0
    Just mac => encodeBool (isMulticast mac)

export
proven_idris_mac_is_locally_administered : String -> Int
proven_idris_mac_is_locally_administered s =
  case parseMAC s of
    Nothing => 0
    Just mac => encodeBool (isLocallyAdministered mac)

--------------------------------------------------------------------------------
-- Hostname Operations
--------------------------------------------------------------------------------

export
proven_idris_hostname_parse : String -> (Int, String)
proven_idris_hostname_parse s = encodeMaybeHostname (parseHostname s)

--------------------------------------------------------------------------------
-- Port Range Constants
--------------------------------------------------------------------------------

export
proven_idris_port_min : Int
proven_idris_port_min = 1

export
proven_idris_port_max : Int
proven_idris_port_max = 65535

export
proven_idris_port_well_known_max : Int
proven_idris_port_well_known_max = 1023

export
proven_idris_port_registered_max : Int
proven_idris_port_registered_max = 49151

export
proven_idris_port_ephemeral_min : Int
proven_idris_port_ephemeral_min = 49152

--------------------------------------------------------------------------------
-- Special IPv4 Addresses
--------------------------------------------------------------------------------

export
proven_idris_ipv4_localhost : String
proven_idris_ipv4_localhost = show localhost

export
proven_idris_ipv4_broadcast : String
proven_idris_ipv4_broadcast = show broadcast

export
proven_idris_ipv4_any : String
proven_idris_ipv4_any = show anyAddress

--------------------------------------------------------------------------------
-- Error Checking
--------------------------------------------------------------------------------

export
proven_idris_network_is_parse_error : String -> Int
proven_idris_network_is_parse_error errorMsg =
  if isInfixOf "invalid" (toLower errorMsg) || isInfixOf "parse" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_network_is_range_error : String -> Int
proven_idris_network_is_range_error errorMsg =
  if isInfixOf "range" (toLower errorMsg) || isInfixOf "out of" (toLower errorMsg)
    then 1
    else 0
