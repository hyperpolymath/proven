// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe network address operations.
 */

import { err, ok, type Result } from './result.ts';

export enum Ipv4Class {
  Loopback = 'loopback',
  Private = 'private',
  LinkLocal = 'link-local',
  Multicast = 'multicast',
  Public = 'public',
}

export interface Cidr {
  address: string;
  prefixLen: number;
}

/**
 * Safe network operations.
 */
export class SafeNetwork {
  /** Parse and validate an IPv4 address. */
  static parseIpv4(s: string): Result<[number, number, number, number]> {
    const parts = s.split('.');
    if (parts.length !== 4) {
      return err('Invalid IPv4 address');
    }

    const octets: number[] = [];
    for (const part of parts) {
      const n = parseInt(part, 10);
      if (isNaN(n) || n < 0 || n > 255 || part !== String(n)) {
        return err('Invalid IPv4 octet');
      }
      octets.push(n);
    }

    return ok([octets[0], octets[1], octets[2], octets[3]]);
  }

  /** Classify an IPv4 address. */
  static classifyIpv4(octets: [number, number, number, number]): Ipv4Class {
    const [a, b] = octets;

    if (a === 127) return Ipv4Class.Loopback;
    if (a === 10) return Ipv4Class.Private;
    if (a === 172 && b >= 16 && b <= 31) return Ipv4Class.Private;
    if (a === 192 && b === 168) return Ipv4Class.Private;
    if (a === 169 && b === 254) return Ipv4Class.LinkLocal;
    if (a >= 224 && a <= 239) return Ipv4Class.Multicast;

    return Ipv4Class.Public;
  }

  /** Parse CIDR notation. */
  static parseCidr(s: string): Result<Cidr> {
    const [addr, prefix] = s.split('/');
    if (!addr || !prefix) {
      return err('Invalid CIDR notation');
    }

    const prefixLen = parseInt(prefix, 10);
    if (isNaN(prefixLen) || prefixLen < 0 || prefixLen > 32) {
      return err('Invalid prefix length');
    }

    // Validate the address
    const addrResult = this.parseIpv4(addr);
    if (!addrResult.ok) {
      return err(addrResult.error);
    }

    return ok({ address: addr, prefixLen });
  }

  /** Check if an IP is within a CIDR range. */
  static isInCidr(ip: string, cidr: Cidr): boolean {
    const ipResult = this.parseIpv4(ip);
    const cidrAddrResult = this.parseIpv4(cidr.address);

    if (!ipResult.ok || !cidrAddrResult.ok) return false;

    const ipNum = (ipResult.value[0] << 24) |
      (ipResult.value[1] << 16) |
      (ipResult.value[2] << 8) |
      ipResult.value[3];
    const cidrNum = (cidrAddrResult.value[0] << 24) |
      (cidrAddrResult.value[1] << 16) |
      (cidrAddrResult.value[2] << 8) |
      cidrAddrResult.value[3];

    const mask = cidr.prefixLen === 0 ? 0 : ~0 << (32 - cidr.prefixLen);

    return (ipNum & mask) === (cidrNum & mask);
  }

  /** Check if port is well-known (0-1023). */
  static isWellKnownPort(port: number): boolean {
    return port >= 0 && port <= 1023;
  }

  /** Check if port is registered (1024-49151). */
  static isRegisteredPort(port: number): boolean {
    return port >= 1024 && port <= 49151;
  }

  /** Check if port is dynamic/private (49152-65535). */
  static isDynamicPort(port: number): boolean {
    return port >= 49152 && port <= 65535;
  }
}
