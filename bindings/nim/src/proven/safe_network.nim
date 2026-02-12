# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe network operations for IP address validation and classification.

import std/[strutils, options]

type
  IPv4* = object
    ## Represents an IPv4 address.
    a*, b*, c*, d*: uint8

proc `$`*(ip: IPv4): string =
  ## Convert IPv4 to string representation.
  result = $ip.a & "." & $ip.b & "." & $ip.c & "." & $ip.d

proc parseIPv4*(address: string): Option[IPv4] =
  ## Parse an IPv4 address string.
  let parts = address.split('.')
  if parts.len != 4:
    return none(IPv4)

  var octets: array[4, uint8]
  for i, part in parts:
    try:
      let num = parseInt(part)
      if num < 0 or num > 255:
        return none(IPv4)
      octets[i] = uint8(num)
    except ValueError:
      return none(IPv4)

  result = some(IPv4(a: octets[0], b: octets[1], c: octets[2], d: octets[3]))

proc isValidIPv4*(address: string): bool =
  ## Check if a string is a valid IPv4 address.
  result = parseIPv4(address).isSome

proc isPrivate*(address: string): bool =
  ## Check if an IPv4 address is in a private range.
  let ip = parseIPv4(address)
  if ip.isNone:
    return false

  let ipv4 = ip.get

  # 10.0.0.0/8
  if ipv4.a == 10:
    return true

  # 172.16.0.0/12
  if ipv4.a == 172 and ipv4.b >= 16 and ipv4.b <= 31:
    return true

  # 192.168.0.0/16
  if ipv4.a == 192 and ipv4.b == 168:
    return true

  result = false

proc isLoopback*(address: string): bool =
  ## Check if an IPv4 address is a loopback address (127.0.0.0/8).
  let ip = parseIPv4(address)
  if ip.isNone:
    return false

  result = ip.get.a == 127

proc isPublic*(address: string): bool =
  ## Check if an IPv4 address is public (not private or loopback).
  result = isValidIPv4(address) and not isPrivate(address) and not isLoopback(address)

proc formatIPv4*(ip: IPv4): string =
  ## Format an IPv4 address as a string.
  result = $ip
