// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Proven - Code that cannot crash
 *
 * A verified safety library providing:
 * - SafeUUID: UUID parsing, validation, and generation
 * - SafeCurrency: Money arithmetic without floating-point errors
 * - SafePhone: Phone number parsing and formatting
 * - SafeHex: Hexadecimal encoding/decoding with constant-time comparison
 *
 * JavaScript bindings for the proven library.
 * @module
 */

// Result utilities
export { ok, err, isOk, isErr, unwrap, unwrapOr } from './result.js';

// UUID module
export { SafeUUID, UuidVersion, UuidVariant } from './safe_uuid.js';

// Currency module
export { Money, CurrencyCode } from './safe_currency.js';

// Phone module
export { PhoneNumber, CountryCode, PhoneNumberType } from './safe_phone.js';

// Hex module
export { SafeHex } from './safe_hex.js';

/**
 * Library version.
 * @type {string}
 */
export const VERSION = '0.3.0';
