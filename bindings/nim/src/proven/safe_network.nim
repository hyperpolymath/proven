# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe network operations for IP address validation and classification.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  IPv4* = object
    ## Represents an IPv4 address.
    a*, b*, c*, d*: uint8

proc `$`*(ip: IPv4): string =
  ## Convert IPv4 to string representation.
  $ip.a & "." & $ip.b & "." & $ip.c & "." & $ip.d

proc parseIPv4*(address: string): Option[IPv4] =
  ## Parse an IPv4 address string.
  if address.len == 0:
    return none(IPv4)
  let res = provenNetworkParseIpv4(unsafeAddr address[0], csize_t(address.len))
  if res.status == PROVEN_OK:
    return some(IPv4(
      a: res.address.octets[0],
      b: res.address.octets[1],
      c: res.address.octets[2],
      d: res.address.octets[3]
    ))
  none(IPv4)

proc isValidIPv4*(address: string): bool =
  ## Check if a string is a valid IPv4 address.
  parseIPv4(address).isSome

proc isPrivate*(address: string): bool =
  ## Check if an IPv4 address is in a private range.
  if address.len == 0:
    return false
  let res = provenNetworkParseIpv4(unsafeAddr address[0], csize_t(address.len))
  if res.status != PROVEN_OK:
    return false
  provenNetworkIpv4IsPrivate(res.address)

proc isLoopback*(address: string): bool =
  ## Check if an IPv4 address is a loopback address (127.0.0.0/8).
  if address.len == 0:
    return false
  let res = provenNetworkParseIpv4(unsafeAddr address[0], csize_t(address.len))
  if res.status != PROVEN_OK:
    return false
  provenNetworkIpv4IsLoopback(res.address)

proc isPublic*(address: string): bool =
  ## Check if an IPv4 address is public (not private or loopback).
  isValidIPv4(address) and not isPrivate(address) and not isLoopback(address)

proc formatIPv4*(ip: IPv4): string =
  ## Format an IPv4 address as a string.
  $ip
