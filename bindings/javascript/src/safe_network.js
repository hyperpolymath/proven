// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeNetwork - IP address and CIDR validation that cannot crash.
 *
 * Provides safe network operations with bounds checking.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Parsed IPv4 address.
 * @typedef {Object} IPv4Address
 * @property {number[]} octets - Four octets as numbers
 * @property {string} normalized - Normalized string representation
 */

/**
 * Parsed CIDR block.
 * @typedef {Object} CidrBlock
 * @property {IPv4Address} network - Network address
 * @property {number} prefix - Prefix length (0-32)
 * @property {string} normalized - Normalized CIDR notation
 */

/**
 * Safe network operations.
 */
export class SafeNetwork {
  /**
   * Parse an IPv4 address.
   *
   * @param {string} ip - IP address string
   * @returns {{ ok: true, value: IPv4Address } | { ok: false, error: string }}
   *
   * @example
   * SafeNetwork.parseIpv4("192.168.1.1")
   */
  static parseIpv4(ip) {
    if (typeof ip !== 'string') {
      return err('IP must be a string');
    }

    const parts = ip.trim().split('.');
    if (parts.length !== 4) {
      return err('IPv4 must have exactly 4 octets');
    }

    const octets = [];
    for (const part of parts) {
      if (!/^\d+$/.test(part)) {
        return err('Invalid octet: must be numeric');
      }
      const num = parseInt(part, 10);
      if (num < 0 || num > 255) {
        return err('Octet must be 0-255');
      }
      // Check for leading zeros (not allowed in strict mode)
      if (part.length > 1 && part.startsWith('0')) {
        return err('Leading zeros not allowed');
      }
      octets.push(num);
    }

    return ok({
      octets,
      normalized: octets.join('.'),
    });
  }

  /**
   * Check if string is a valid IPv4 address.
   *
   * @param {string} ip - IP address string
   * @returns {boolean}
   */
  static isValidIpv4(ip) {
    return SafeNetwork.parseIpv4(ip).ok;
  }

  /**
   * Parse a CIDR block.
   *
   * @param {string} cidr - CIDR notation (e.g., "192.168.0.0/24")
   * @returns {{ ok: true, value: CidrBlock } | { ok: false, error: string }}
   */
  static parseCidr(cidr) {
    if (typeof cidr !== 'string') {
      return err('CIDR must be a string');
    }

    const parts = cidr.trim().split('/');
    if (parts.length !== 2) {
      return err('CIDR must contain exactly one /');
    }

    const ipResult = SafeNetwork.parseIpv4(parts[0]);
    if (!ipResult.ok) {
      return ipResult;
    }

    if (!/^\d+$/.test(parts[1])) {
      return err('Prefix must be numeric');
    }

    const prefix = parseInt(parts[1], 10);
    if (prefix < 0 || prefix > 32) {
      return err('Prefix must be 0-32');
    }

    return ok({
      network: ipResult.value,
      prefix,
      normalized: `${ipResult.value.normalized}/${prefix}`,
    });
  }

  /**
   * Check if an IP is within a CIDR block.
   *
   * @param {string} ip - IP address
   * @param {string} cidr - CIDR block
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isInCidr(ip, cidr) {
    const ipResult = SafeNetwork.parseIpv4(ip);
    if (!ipResult.ok) {
      return ipResult;
    }

    const cidrResult = SafeNetwork.parseCidr(cidr);
    if (!cidrResult.ok) {
      return cidrResult;
    }

    const ipNum = SafeNetwork.ipToNumber(ipResult.value.octets);
    const networkNum = SafeNetwork.ipToNumber(cidrResult.value.network.octets);
    const mask = SafeNetwork.prefixToMask(cidrResult.value.prefix);

    return ok((ipNum & mask) === (networkNum & mask));
  }

  /**
   * Convert IP octets to 32-bit number.
   *
   * @param {number[]} octets - Four octets
   * @returns {number}
   */
  static ipToNumber(octets) {
    return ((octets[0] << 24) | (octets[1] << 16) | (octets[2] << 8) | octets[3]) >>> 0;
  }

  /**
   * Convert 32-bit number to IP octets.
   *
   * @param {number} num - 32-bit number
   * @returns {number[]}
   */
  static numberToIp(num) {
    return [(num >>> 24) & 0xff, (num >>> 16) & 0xff, (num >>> 8) & 0xff, num & 0xff];
  }

  /**
   * Convert prefix length to subnet mask.
   *
   * @param {number} prefix - Prefix length (0-32)
   * @returns {number}
   */
  static prefixToMask(prefix) {
    if (prefix === 0) return 0;
    return (~0 << (32 - prefix)) >>> 0;
  }

  /**
   * Get subnet mask as IP string.
   *
   * @param {number} prefix - Prefix length (0-32)
   * @returns {string}
   */
  static prefixToMaskString(prefix) {
    const mask = SafeNetwork.prefixToMask(prefix);
    return SafeNetwork.numberToIp(mask).join('.');
  }

  /**
   * Check if IP is private (RFC 1918).
   *
   * @param {string} ip - IP address
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isPrivate(ip) {
    const result = SafeNetwork.parseIpv4(ip);
    if (!result.ok) {
      return result;
    }

    const octets = result.value.octets;
    // 10.0.0.0/8
    if (octets[0] === 10) {
      return ok(true);
    }
    // 172.16.0.0/12
    if (octets[0] === 172 && octets[1] >= 16 && octets[1] <= 31) {
      return ok(true);
    }
    // 192.168.0.0/16
    if (octets[0] === 192 && octets[1] === 168) {
      return ok(true);
    }
    return ok(false);
  }

  /**
   * Check if IP is loopback (127.0.0.0/8).
   *
   * @param {string} ip - IP address
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isLoopback(ip) {
    const result = SafeNetwork.parseIpv4(ip);
    if (!result.ok) {
      return result;
    }
    return ok(result.value.octets[0] === 127);
  }

  /**
   * Check if IP is link-local (169.254.0.0/16).
   *
   * @param {string} ip - IP address
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isLinkLocal(ip) {
    const result = SafeNetwork.parseIpv4(ip);
    if (!result.ok) {
      return result;
    }
    const octets = result.value.octets;
    return ok(octets[0] === 169 && octets[1] === 254);
  }

  /**
   * Get network address from IP and prefix.
   *
   * @param {string} ip - IP address
   * @param {number} prefix - Prefix length
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static getNetworkAddress(ip, prefix) {
    const result = SafeNetwork.parseIpv4(ip);
    if (!result.ok) {
      return result;
    }

    if (prefix < 0 || prefix > 32) {
      return err('Prefix must be 0-32');
    }

    const ipNum = SafeNetwork.ipToNumber(result.value.octets);
    const mask = SafeNetwork.prefixToMask(prefix);
    const networkNum = (ipNum & mask) >>> 0;

    return ok(SafeNetwork.numberToIp(networkNum).join('.'));
  }

  /**
   * Get broadcast address from IP and prefix.
   *
   * @param {string} ip - IP address
   * @param {number} prefix - Prefix length
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static getBroadcastAddress(ip, prefix) {
    const result = SafeNetwork.parseIpv4(ip);
    if (!result.ok) {
      return result;
    }

    if (prefix < 0 || prefix > 32) {
      return err('Prefix must be 0-32');
    }

    const ipNum = SafeNetwork.ipToNumber(result.value.octets);
    const mask = SafeNetwork.prefixToMask(prefix);
    const broadcastNum = (ipNum | ~mask) >>> 0;

    return ok(SafeNetwork.numberToIp(broadcastNum).join('.'));
  }
}
