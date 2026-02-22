// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeNetwork - Typed wrapper for network operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

/** JavaScript bindings to the SafeNetwork FFI wrapper. */
module SafeNetworkJs = {
  @module("../../javascript/src/safe_network.js") @scope("SafeNetwork")
  external parseIpv4: string => Nullable.t<{..}> = "parseIpv4"

  @module("../../javascript/src/safe_network.js") @scope("SafeNetwork")
  external isValidIpv4: string => bool = "isValidIpv4"

  @module("../../javascript/src/safe_network.js") @scope("SafeNetwork")
  external isPrivate: string => bool = "isPrivate"

  @module("../../javascript/src/safe_network.js") @scope("SafeNetwork")
  external isLoopback: string => bool = "isLoopback"
}

/** An IPv4 address with its four octets. */
type ipv4 = {octets: (int, int, int, int)}

/**
 * Parse an IPv4 address string.
 * Delegates to proven_network_parse_ipv4 via FFI.
 *
 * @param address IPv4 address string (e.g. "192.168.1.1").
 * @returns Some(ipv4) or None on failure.
 */
let parseIpv4 = (address: string): option<{..}> => {
  SafeNetworkJs.parseIpv4(address)->Nullable.toOption
}

/**
 * Check if a string is a valid IPv4 address.
 *
 * @param address Address string to validate.
 * @returns true if the address is valid.
 */
let isValidIpv4 = SafeNetworkJs.isValidIpv4

/**
 * Check if an IPv4 address is in a private range (RFC 1918).
 * Delegates to proven_network_ipv4_is_private via FFI.
 *
 * @param address IPv4 address string.
 * @returns true if the address is private.
 */
let isPrivate = SafeNetworkJs.isPrivate

/**
 * Check if an IPv4 address is a loopback address (127.0.0.0/8).
 * Delegates to proven_network_ipv4_is_loopback via FFI.
 *
 * @param address IPv4 address string.
 * @returns true if the address is loopback.
 */
let isLoopback = SafeNetworkJs.isLoopback
