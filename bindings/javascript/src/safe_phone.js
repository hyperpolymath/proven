// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafePhone - E.164 phone number handling.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_phone_parse returns PhoneResult struct and
 * proven_phone_format_e164 takes u16 + u64 which need special
 * marshaling for Deno FFI struct returns.
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Country code constants.
 * @readonly
 * @enum {number}
 */
export const CountryCode = Object.freeze({
  US: 1,
  GB: 44,
  DE: 49,
  FR: 33,
  JP: 81,
  CN: 86,
  IN: 91,
  AU: 61,
  BR: 55,
  CA: 1,
});

/**
 * Phone number type enumeration.
 * @readonly
 * @enum {string}
 */
export const PhoneNumberType = Object.freeze({
  FIXED_LINE: 'fixed_line',
  MOBILE: 'mobile',
  TOLL_FREE: 'toll_free',
  PREMIUM_RATE: 'premium_rate',
  UNKNOWN: 'unknown',
});

/**
 * Phone number representation.
 */
export class PhoneNumber {
  /** @type {number} */
  #countryCode;
  /** @type {bigint} */
  #nationalNumber;

  /**
   * @param {number} countryCode - The country calling code.
   * @param {bigint} nationalNumber - The national number.
   */
  constructor(countryCode, nationalNumber) {
    this.#countryCode = countryCode;
    this.#nationalNumber = nationalNumber;
  }

  /** @returns {number} The country calling code. */
  getCountryCode() { return this.#countryCode; }

  /** @returns {bigint} The national number. */
  getNationalNumber() { return this.#nationalNumber; }

  /** @returns {string} E.164 format. */
  toE164() {
    return `+${this.#countryCode}${this.#nationalNumber}`;
  }

  /** @returns {string} E.164 format. */
  toString() { return this.toE164(); }
}
