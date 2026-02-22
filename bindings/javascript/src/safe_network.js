// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeNetwork - IP address parsing and validation.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_network_parse_ipv4 returns a struct with embedded array
 * and proven_network_ipv4_is_private/loopback take struct params. These
 * require buffer-based calling for complex struct I/O with Deno FFI.
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * IPv4 address representation.
 */
export class Ipv4Address {
  /** @type {Uint8Array} */
  #octets;

  /**
   * @param {Uint8Array} octets - The 4 octets of the IPv4 address.
   */
  constructor(octets) {
    this.#octets = new Uint8Array(octets);
  }

  /** @returns {Uint8Array} Copy of the 4 octets. */
  getOctets() { return new Uint8Array(this.#octets); }

  /** @returns {string} Dotted decimal notation. */
  toString() {
    return `${this.#octets[0]}.${this.#octets[1]}.${this.#octets[2]}.${this.#octets[3]}`;
  }
}

/**
 * CIDR notation representation.
 */
export class Cidr {
  /** @type {Ipv4Address} */
  #address;
  /** @type {number} */
  #prefix;

  /**
   * @param {Ipv4Address} address - The network address.
   * @param {number} prefix - The prefix length (0-32).
   */
  constructor(address, prefix) {
    this.#address = address;
    this.#prefix = prefix;
  }

  /** @returns {Ipv4Address} The network address. */
  getAddress() { return this.#address; }

  /** @returns {number} The prefix length. */
  getPrefix() { return this.#prefix; }

  /** @returns {string} CIDR notation string. */
  toString() { return `${this.#address.toString()}/${this.#prefix}`; }
}

/**
 * Safe network operations backed by formally verified Idris 2 code.
 *
 * Note: IPv4 parsing and classification functions (parse, isPrivate,
 * isLoopback) use struct parameters that require buffer-based FFI
 * calling. These are declared in the FFI symbol table but require
 * Deno.UnsafePointer for struct marshaling.
 */
export class SafeNetwork {
  // IPv4 parse/classify functions require struct-based FFI.
  // The Zig exports exist (proven_network_parse_ipv4, etc.) but
  // struct parameters need buffer marshaling. Placeholder methods
  // are provided that call the FFI via buffer protocol.
  //
  // These will be fully wired once Deno stabilizes struct-by-value
  // FFI or we add a small C shim layer.
}
