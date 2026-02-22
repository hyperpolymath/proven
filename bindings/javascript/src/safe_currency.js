// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCurrency - Monetary values with ISO 4217 codes.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_currency_parse and proven_currency_format use struct
 * parameters ([3]u8 currency code) that require buffer marshaling.
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * ISO 4217 currency code constants.
 * @readonly
 * @enum {string}
 */
export const CurrencyCode = Object.freeze({
  USD: 'USD',
  EUR: 'EUR',
  GBP: 'GBP',
  JPY: 'JPY',
  CHF: 'CHF',
  CAD: 'CAD',
  AUD: 'AUD',
  CNY: 'CNY',
  INR: 'INR',
  BRL: 'BRL',
});

/**
 * Money representation backed by minor units (cents).
 * All arithmetic is done in integer minor units to avoid floating-point errors.
 */
export class Money {
  /** @type {bigint} Amount in minor units (e.g. cents). */
  #amountMinor;
  /** @type {string} 3-letter ISO 4217 code. */
  #code;
  /** @type {number} Number of decimal places. */
  #decimalPlaces;

  /**
   * @param {bigint} amountMinor - Amount in minor units.
   * @param {string} code - ISO 4217 currency code.
   * @param {number} decimalPlaces - Decimal places for this currency.
   */
  constructor(amountMinor, code, decimalPlaces) {
    this.#amountMinor = amountMinor;
    this.#code = code;
    this.#decimalPlaces = decimalPlaces;
  }

  /** @returns {bigint} The amount in minor units. */
  getAmountMinor() { return this.#amountMinor; }

  /** @returns {string} The currency code. */
  getCode() { return this.#code; }

  /** @returns {number} The number of decimal places. */
  getDecimalPlaces() { return this.#decimalPlaces; }

  /**
   * Create Money from major units (e.g. dollars).
   *
   * @param {number} amount - Amount in major units.
   * @param {string} code - ISO 4217 currency code.
   * @param {number} [decimalPlaces=2] - Decimal places.
   * @returns {{ ok: true, value: Money } | { ok: false, error: string }}
   */
  static fromMajorUnits(amount, code, decimalPlaces = 2) {
    if (typeof code !== 'string' || code.length !== 3) {
      return err('Currency code must be exactly 3 characters');
    }
    const factor = Math.pow(10, decimalPlaces);
    const minor = BigInt(Math.round(amount * factor));
    return ok(new Money(minor, code.toUpperCase(), decimalPlaces));
  }

  /**
   * Create Money from minor units directly.
   *
   * @param {number|bigint} amountMinor - Amount in minor units.
   * @param {string} code - ISO 4217 currency code.
   * @param {number} [decimalPlaces=2] - Decimal places.
   * @returns {{ ok: true, value: Money } | { ok: false, error: string }}
   */
  static fromMinorUnits(amountMinor, code, decimalPlaces = 2) {
    if (typeof code !== 'string' || code.length !== 3) {
      return err('Currency code must be exactly 3 characters');
    }
    return ok(new Money(BigInt(amountMinor), code.toUpperCase(), decimalPlaces));
  }

  /**
   * Format as string (e.g. "USD 12.34").
   * The FFI proven_currency_format would do this but requires struct params.
   *
   * @returns {string}
   */
  format() {
    const factor = BigInt(Math.pow(10, this.#decimalPlaces));
    const major = this.#amountMinor / factor;
    const minor = this.#amountMinor % factor;
    const minorStr = minor.toString().padStart(this.#decimalPlaces, '0');
    return `${this.#code} ${major}.${minorStr}`;
  }
}
