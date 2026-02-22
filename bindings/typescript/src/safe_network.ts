// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeNetwork - Typed wrapper for network operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeNetwork as JsSafeNetwork } from '../../javascript/src/safe_network.js';

/** An IPv4 address with its four octets. */
export interface IPv4 {
  octets: [number, number, number, number];
}

/** Result type for network operations. */
export type NetworkResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Safe network operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export class SafeNetwork {
  /**
   * Parse an IPv4 address string.
   * Delegates to proven_network_parse_ipv4 via FFI.
   *
   * @param address - IPv4 address string (e.g. "192.168.1.1").
   * @returns Parsed IPv4 address or null on failure.
   */
  static parseIpv4(address: string): IPv4 | null {
    return JsSafeNetwork.parseIpv4(address) as IPv4 | null;
  }

  /**
   * Check if a string is a valid IPv4 address.
   *
   * @param address - Address string to validate.
   * @returns true if the address is valid.
   */
  static isValidIpv4(address: string): boolean {
    return JsSafeNetwork.isValidIpv4(address);
  }

  /**
   * Check if an IPv4 address is in a private range (RFC 1918).
   * Delegates to proven_network_ipv4_is_private via FFI.
   *
   * @param address - IPv4 address string.
   * @returns true if the address is private.
   */
  static isPrivate(address: string): boolean {
    return JsSafeNetwork.isPrivate(address);
  }

  /**
   * Check if an IPv4 address is a loopback address (127.0.0.0/8).
   * Delegates to proven_network_ipv4_is_loopback via FFI.
   *
   * @param address - IPv4 address string.
   * @returns true if the address is loopback.
   */
  static isLoopback(address: string): boolean {
    return JsSafeNetwork.isLoopback(address);
  }
}
