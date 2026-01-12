// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Safe UUID parsing, generation, and validation.
 */

import { err, ok, type Result } from './result.ts';

/** UUID version as defined in RFC 4122. */
export enum UuidVersion {
  /** Time-based UUID (RFC 4122). */
  V1 = 1,
  /** DCE Security UUID. */
  V2 = 2,
  /** Name-based UUID using MD5 hashing. */
  V3 = 3,
  /** Random UUID. */
  V4 = 4,
  /** Name-based UUID using SHA-1 hashing. */
  V5 = 5,
  /** Reordered time-based UUID (RFC 9562). */
  V6 = 6,
  /** Unix epoch time-based UUID (RFC 9562). */
  V7 = 7,
  /** Custom UUID (RFC 9562). */
  V8 = 8,
  /** Unknown or nil UUID. */
  Unknown = 0,
}

/** UUID variant as defined in RFC 4122. */
export enum UuidVariant {
  /** Reserved for NCS backward compatibility. */
  Ncs = 'ncs',
  /** RFC 4122 variant. */
  Rfc4122 = 'rfc4122',
  /** Reserved for Microsoft backward compatibility. */
  Microsoft = 'microsoft',
  /** Reserved for future definition. */
  Future = 'future',
  /** Nil UUID (all zeros). */
  Nil = 'nil',
  /** Max UUID (all ones). */
  Max = 'max',
}

/** Parsed UUID structure. */
export interface Uuid {
  /** Original string representation. */
  readonly value: string;
  /** UUID version. */
  readonly version: UuidVersion;
  /** UUID variant. */
  readonly variant: UuidVariant;
  /** Raw bytes as Uint8Array. */
  readonly bytes: Uint8Array;
}

/** Nil UUID constant (all zeros). */
const NIL_UUID = '00000000-0000-0000-0000-000000000000';

/** Max UUID constant (all ones). */
const MAX_UUID = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

/** Valid UUID regex pattern. */
const UUID_REGEX = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

/**
 * Safe UUID operations.
 */
export class SafeUuid {
  /**
   * Parse and validate a UUID string.
   *
   * @example
   * ```ts
   * const result = SafeUuid.parse("550e8400-e29b-41d4-a716-446655440000");
   * if (result.ok) {
   *   console.log(result.value.version); // UuidVersion.V4
   * }
   * ```
   */
  static parse(uuidString: string): Result<Uuid> {
    const trimmed = uuidString.trim().toLowerCase();

    if (!trimmed) {
      return err('Empty UUID string');
    }

    if (!UUID_REGEX.test(trimmed)) {
      return err('Invalid UUID format');
    }

    const bytes = this.toBytes(trimmed);
    const version = this.extractVersion(bytes);
    const variant = this.extractVariant(bytes, trimmed);

    return ok({
      value: trimmed,
      version,
      variant,
      bytes,
    });
  }

  /** Check if a string is a valid UUID. */
  static isValid(uuidString: string): boolean {
    return this.parse(uuidString).ok;
  }

  /** Format a UUID to canonical lowercase form. */
  static format(uuid: string | Uuid): Result<string> {
    if (typeof uuid === 'string') {
      const parsed = this.parse(uuid);
      if (!parsed.ok) return parsed;
      return ok(parsed.value.value);
    }
    return ok(uuid.value);
  }

  /** Generate a random v4 UUID. */
  static generateV4(): string {
    const bytes = new Uint8Array(16);
    crypto.getRandomValues(bytes);

    // Set version to 4
    bytes[6] = (bytes[6] & 0x0f) | 0x40;
    // Set variant to RFC 4122
    bytes[8] = (bytes[8] & 0x3f) | 0x80;

    return this.bytesToString(bytes);
  }

  /**
   * Generate a v7 UUID (time-ordered, RFC 9562).
   *
   * @example
   * ```ts
   * const uuid = SafeUuid.generateV7();
   * // Time-ordered UUID suitable for database primary keys
   * ```
   */
  static generateV7(): string {
    const bytes = new Uint8Array(16);
    const timestamp = BigInt(Date.now());

    // 48-bit timestamp
    bytes[0] = Number((timestamp >> 40n) & 0xffn);
    bytes[1] = Number((timestamp >> 32n) & 0xffn);
    bytes[2] = Number((timestamp >> 24n) & 0xffn);
    bytes[3] = Number((timestamp >> 16n) & 0xffn);
    bytes[4] = Number((timestamp >> 8n) & 0xffn);
    bytes[5] = Number(timestamp & 0xffn);

    // Random bits for the rest
    crypto.getRandomValues(bytes.subarray(6));

    // Set version to 7
    bytes[6] = (bytes[6] & 0x0f) | 0x70;
    // Set variant to RFC 4122
    bytes[8] = (bytes[8] & 0x3f) | 0x80;

    return this.bytesToString(bytes);
  }

  /** Get the nil UUID (all zeros). */
  static nil(): string {
    return NIL_UUID;
  }

  /** Get the max UUID (all ones). */
  static max(): string {
    return MAX_UUID;
  }

  /** Check if a UUID is nil. */
  static isNil(uuid: string | Uuid): boolean {
    const value = typeof uuid === 'string' ? uuid.toLowerCase() : uuid.value;
    return value === NIL_UUID;
  }

  /** Check if a UUID is max. */
  static isMax(uuid: string | Uuid): boolean {
    const value = typeof uuid === 'string' ? uuid.toLowerCase() : uuid.value;
    return value === MAX_UUID;
  }

  /** Get the version of a UUID. */
  static getVersion(uuid: string | Uuid): Result<UuidVersion> {
    if (typeof uuid === 'string') {
      const parsed = this.parse(uuid);
      if (!parsed.ok) return parsed;
      return ok(parsed.value.version);
    }
    return ok(uuid.version);
  }

  /** Get the variant of a UUID. */
  static getVariant(uuid: string | Uuid): Result<UuidVariant> {
    if (typeof uuid === 'string') {
      const parsed = this.parse(uuid);
      if (!parsed.ok) return parsed;
      return ok(parsed.value.variant);
    }
    return ok(uuid.variant);
  }

  /** Compare two UUIDs for equality. */
  static equals(uuidA: string | Uuid, uuidB: string | Uuid): boolean {
    const valueA = typeof uuidA === 'string' ? uuidA.toLowerCase() : uuidA.value;
    const valueB = typeof uuidB === 'string' ? uuidB.toLowerCase() : uuidB.value;
    return valueA === valueB;
  }

  /** Convert UUID bytes to string representation. */
  private static bytesToString(bytes: Uint8Array): string {
    const hex = Array.from(bytes, (b) => b.toString(16).padStart(2, '0')).join('');
    return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${
      hex.slice(20)
    }`;
  }

  /** Convert UUID string to bytes. */
  private static toBytes(uuid: string): Uint8Array {
    const hex = uuid.replace(/-/g, '');
    const bytes = new Uint8Array(16);
    for (let i = 0; i < 16; i++) {
      bytes[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);
    }
    return bytes;
  }

  /** Extract version from UUID bytes. */
  private static extractVersion(bytes: Uint8Array): UuidVersion {
    const versionNibble = bytes[6] >> 4;
    switch (versionNibble) {
      case 1:
        return UuidVersion.V1;
      case 2:
        return UuidVersion.V2;
      case 3:
        return UuidVersion.V3;
      case 4:
        return UuidVersion.V4;
      case 5:
        return UuidVersion.V5;
      case 6:
        return UuidVersion.V6;
      case 7:
        return UuidVersion.V7;
      case 8:
        return UuidVersion.V8;
      default:
        return UuidVersion.Unknown;
    }
  }

  /** Extract variant from UUID bytes and string. */
  private static extractVariant(bytes: Uint8Array, uuid: string): UuidVariant {
    if (uuid === NIL_UUID) return UuidVariant.Nil;
    if (uuid === MAX_UUID) return UuidVariant.Max;

    const variantByte = bytes[8];
    if ((variantByte & 0x80) === 0) {
      return UuidVariant.Ncs;
    }
    if ((variantByte & 0xc0) === 0x80) {
      return UuidVariant.Rfc4122;
    }
    if ((variantByte & 0xe0) === 0xc0) {
      return UuidVariant.Microsoft;
    }
    return UuidVariant.Future;
  }
}
