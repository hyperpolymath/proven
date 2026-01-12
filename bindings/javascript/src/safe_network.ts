// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeNetwork - Network operations that cannot crash.
 *
 * Provides safe IP address parsing and validation.
 */

import { getExports, encodeString, freePtr } from './wasm.js';
import { statusFromCode } from './error.js';

/**
 * An IPv4 address.
 */
export interface IPv4 {
  octets: [number, number, number, number];
}

/**
 * Safe network operations with proven correctness guarantees.
 */
export class SafeNetwork {
  /**
   * Parse an IPv4 address.
   *
   * @example
   * SafeNetwork.parseIpv4("192.168.1.1")
   * // { octets: [192, 168, 1, 1] }
   */
  static parseIpv4(address: string): IPv4 | null {
    const exports = getExports();
    const fn = exports['proven_network_parse_ipv4'] as (ptr: number, len: number) => number;

    const { ptr, len } = encodeString(address);
    const result = fn(ptr, len);
    freePtr(ptr);

    const status = (result >> 24) & 0xff;
    if (statusFromCode(status) !== 'ok') {
      return null;
    }

    return {
      octets: [
        (result >> 16) & 0xff,
        (result >> 8) & 0xff,
        result & 0xff,
        (result >> 0) & 0xff,
      ] as [number, number, number, number],
    };
  }

  /**
   * Check if a string is a valid IPv4 address.
   */
  static isValidIpv4(address: string): boolean {
    return SafeNetwork.parseIpv4(address) !== null;
  }

  /**
   * Check if an IPv4 address is in a private range.
   *
   * Private ranges (RFC 1918):
   * - 10.0.0.0/8
   * - 172.16.0.0/12
   * - 192.168.0.0/16
   *
   * @example
   * SafeNetwork.isPrivate("192.168.1.1")  // true
   * SafeNetwork.isPrivate("8.8.8.8")      // false
   */
  static isPrivate(address: string): boolean {
    const ip = SafeNetwork.parseIpv4(address);
    if (!ip) return false;

    const [a, b] = ip.octets;

    // 10.0.0.0/8
    if (a === 10) return true;

    // 172.16.0.0/12
    if (a === 172 && b >= 16 && b <= 31) return true;

    // 192.168.0.0/16
    if (a === 192 && b === 168) return true;

    return false;
  }

  /**
   * Check if an IPv4 address is a loopback address.
   *
   * Loopback range: 127.0.0.0/8
   *
   * @example
   * SafeNetwork.isLoopback("127.0.0.1")    // true
   * SafeNetwork.isLoopback("192.168.1.1")  // false
   */
  static isLoopback(address: string): boolean {
    const ip = SafeNetwork.parseIpv4(address);
    if (!ip) return false;

    return ip.octets[0] === 127;
  }

  /**
   * Check if an IPv4 address is a public (routable) address.
   */
  static isPublic(address: string): boolean {
    if (!SafeNetwork.isValidIpv4(address)) return false;
    return !SafeNetwork.isPrivate(address) && !SafeNetwork.isLoopback(address);
  }

  /**
   * Format an IPv4 address as a string.
   */
  static formatIpv4(ip: IPv4): string {
    return ip.octets.join('.');
  }
}
