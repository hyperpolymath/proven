// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeCurrency - Currency and money operations that cannot crash.
 *
 * Provides safe money arithmetic using minor units (cents/pence) to avoid
 * floating-point precision issues. All calculations use integer arithmetic.
 * @module
 */

import { ok, err } from './result.js';

/**
 * ISO 4217 currency codes with their minor unit (decimal places) information.
 * @readonly
 * @enum {{ code: string, name: string, minorUnits: number, symbol: string }}
 */
export const CurrencyCode = Object.freeze({
  USD: { code: 'USD', name: 'US Dollar', minorUnits: 2, symbol: '$' },
  EUR: { code: 'EUR', name: 'Euro', minorUnits: 2, symbol: '€' },
  GBP: { code: 'GBP', name: 'British Pound', minorUnits: 2, symbol: '£' },
  JPY: { code: 'JPY', name: 'Japanese Yen', minorUnits: 0, symbol: '¥' },
  CHF: { code: 'CHF', name: 'Swiss Franc', minorUnits: 2, symbol: 'CHF' },
  CAD: { code: 'CAD', name: 'Canadian Dollar', minorUnits: 2, symbol: 'CA$' },
  AUD: { code: 'AUD', name: 'Australian Dollar', minorUnits: 2, symbol: 'A$' },
  CNY: { code: 'CNY', name: 'Chinese Yuan', minorUnits: 2, symbol: '¥' },
  INR: { code: 'INR', name: 'Indian Rupee', minorUnits: 2, symbol: '₹' },
  MXN: { code: 'MXN', name: 'Mexican Peso', minorUnits: 2, symbol: 'MX$' },
  BRL: { code: 'BRL', name: 'Brazilian Real', minorUnits: 2, symbol: 'R$' },
  KRW: { code: 'KRW', name: 'South Korean Won', minorUnits: 0, symbol: '₩' },
  SGD: { code: 'SGD', name: 'Singapore Dollar', minorUnits: 2, symbol: 'S$' },
  HKD: { code: 'HKD', name: 'Hong Kong Dollar', minorUnits: 2, symbol: 'HK$' },
  SEK: { code: 'SEK', name: 'Swedish Krona', minorUnits: 2, symbol: 'kr' },
  NOK: { code: 'NOK', name: 'Norwegian Krone', minorUnits: 2, symbol: 'kr' },
  DKK: { code: 'DKK', name: 'Danish Krone', minorUnits: 2, symbol: 'kr' },
  PLN: { code: 'PLN', name: 'Polish Zloty', minorUnits: 2, symbol: 'zł' },
  CZK: { code: 'CZK', name: 'Czech Koruna', minorUnits: 2, symbol: 'Kč' },
  NZD: { code: 'NZD', name: 'New Zealand Dollar', minorUnits: 2, symbol: 'NZ$' },
  ZAR: { code: 'ZAR', name: 'South African Rand', minorUnits: 2, symbol: 'R' },
  RUB: { code: 'RUB', name: 'Russian Ruble', minorUnits: 2, symbol: '₽' },
  TRY: { code: 'TRY', name: 'Turkish Lira', minorUnits: 2, symbol: '₺' },
  THB: { code: 'THB', name: 'Thai Baht', minorUnits: 2, symbol: '฿' },
  IDR: { code: 'IDR', name: 'Indonesian Rupiah', minorUnits: 2, symbol: 'Rp' },
  MYR: { code: 'MYR', name: 'Malaysian Ringgit', minorUnits: 2, symbol: 'RM' },
  PHP: { code: 'PHP', name: 'Philippine Peso', minorUnits: 2, symbol: '₱' },
  ILS: { code: 'ILS', name: 'Israeli Shekel', minorUnits: 2, symbol: '₪' },
  AED: { code: 'AED', name: 'UAE Dirham', minorUnits: 2, symbol: 'د.إ' },
  SAR: { code: 'SAR', name: 'Saudi Riyal', minorUnits: 2, symbol: '﷼' },
  // Cryptocurrencies (8 decimal places common)
  BTC: { code: 'BTC', name: 'Bitcoin', minorUnits: 8, symbol: '₿' },
  ETH: { code: 'ETH', name: 'Ethereum', minorUnits: 18, symbol: 'Ξ' },
  // Currencies with 3 decimal places
  KWD: { code: 'KWD', name: 'Kuwaiti Dinar', minorUnits: 3, symbol: 'د.ك' },
  BHD: { code: 'BHD', name: 'Bahraini Dinar', minorUnits: 3, symbol: 'ب.د' },
  OMR: { code: 'OMR', name: 'Omani Rial', minorUnits: 3, symbol: 'ر.ع.' },
});

/**
 * Maximum safe integer for minor units arithmetic.
 * Using Number.MAX_SAFE_INTEGER to ensure precision.
 * @type {number}
 */
const MAX_MINOR_UNITS = Number.MAX_SAFE_INTEGER;

/**
 * Minimum safe integer for minor units arithmetic.
 * @type {number}
 */
const MIN_MINOR_UNITS = Number.MIN_SAFE_INTEGER;

/**
 * Money class representing a monetary value in minor units.
 *
 * All arithmetic is performed using integer minor units to avoid
 * floating-point precision issues.
 */
export class Money {
  /** @type {number} */
  #minorUnits;

  /** @type {{ code: string, name: string, minorUnits: number, symbol: string }} */
  #currency;

  /**
   * Create a Money instance.
   * Use static factory methods instead of direct construction.
   *
   * @param {number} minorUnits - The amount in minor units (e.g., cents).
   * @param {{ code: string, name: string, minorUnits: number, symbol: string }} currency - The currency.
   */
  constructor(minorUnits, currency) {
    if (!Number.isInteger(minorUnits)) {
      throw new Error('Minor units must be an integer');
    }
    if (minorUnits > MAX_MINOR_UNITS || minorUnits < MIN_MINOR_UNITS) {
      throw new Error('Minor units exceed safe integer range');
    }
    this.#minorUnits = minorUnits;
    this.#currency = currency;
  }

  /**
   * Create Money from minor units (cents, pence, etc.).
   *
   * @param {number} minorUnits - Amount in minor units.
   * @param {{ code: string, name: string, minorUnits: number, symbol: string }} currency - Currency definition.
   * @returns {{ ok: true, value: Money } | { ok: false, error: string }}
   *
   * @example
   * const result = Money.fromMinorUnits(1050, CurrencyCode.USD);
   * // Represents $10.50
   */
  static fromMinorUnits(minorUnits, currency) {
    if (typeof minorUnits !== 'number' || !Number.isFinite(minorUnits)) {
      return err('Minor units must be a finite number');
    }

    if (!Number.isInteger(minorUnits)) {
      return err('Minor units must be an integer');
    }

    if (minorUnits > MAX_MINOR_UNITS || minorUnits < MIN_MINOR_UNITS) {
      return err('Minor units exceed safe integer range');
    }

    if (!currency || typeof currency.code !== 'string') {
      return err('Invalid currency');
    }

    return ok(new Money(minorUnits, currency));
  }

  /**
   * Create Money from a major units amount (e.g., dollars).
   *
   * @param {number} majorUnits - Amount in major units.
   * @param {{ code: string, name: string, minorUnits: number, symbol: string }} currency - Currency definition.
   * @returns {{ ok: true, value: Money } | { ok: false, error: string }}
   *
   * @example
   * const result = Money.fromMajorUnits(10.50, CurrencyCode.USD);
   * // Represents $10.50 (stored as 1050 cents)
   */
  static fromMajorUnits(majorUnits, currency) {
    if (typeof majorUnits !== 'number' || !Number.isFinite(majorUnits)) {
      return err('Major units must be a finite number');
    }

    if (!currency || typeof currency.minorUnits !== 'number') {
      return err('Invalid currency');
    }

    const multiplier = Math.pow(10, currency.minorUnits);
    const minorUnits = Math.round(majorUnits * multiplier);

    return Money.fromMinorUnits(minorUnits, currency);
  }

  /**
   * Parse a string amount to Money.
   *
   * @param {string} amountString - The amount string (e.g., "10.50").
   * @param {{ code: string, name: string, minorUnits: number, symbol: string }} currency - Currency definition.
   * @returns {{ ok: true, value: Money } | { ok: false, error: string }}
   *
   * @example
   * const result = Money.parse("10.50", CurrencyCode.USD);
   */
  static parse(amountString, currency) {
    if (typeof amountString !== 'string') {
      return err('Amount must be a string');
    }

    const trimmed = amountString.trim().replace(/,/g, '');
    const parsed = parseFloat(trimmed);

    if (!Number.isFinite(parsed)) {
      return err('Invalid amount format');
    }

    return Money.fromMajorUnits(parsed, currency);
  }

  /**
   * Create zero Money for a currency.
   *
   * @param {{ code: string, name: string, minorUnits: number, symbol: string }} currency - Currency definition.
   * @returns {Money}
   */
  static zero(currency) {
    return new Money(0, currency);
  }

  /**
   * Get the amount in minor units.
   *
   * @returns {number}
   */
  getMinorUnits() {
    return this.#minorUnits;
  }

  /**
   * Get the amount in major units.
   *
   * @returns {number}
   */
  getMajorUnits() {
    const multiplier = Math.pow(10, this.#currency.minorUnits);
    return this.#minorUnits / multiplier;
  }

  /**
   * Get the currency.
   *
   * @returns {{ code: string, name: string, minorUnits: number, symbol: string }}
   */
  getCurrency() {
    return this.#currency;
  }

  /**
   * Add another Money amount.
   *
   * @param {Money} other - The amount to add.
   * @returns {{ ok: true, value: Money } | { ok: false, error: string }}
   *
   * @example
   * const sum = money1.add(money2);
   */
  add(other) {
    if (!(other instanceof Money)) {
      return err('Cannot add non-Money value');
    }

    if (other.#currency.code !== this.#currency.code) {
      return err('Cannot add different currencies');
    }

    const result = this.#minorUnits + other.#minorUnits;

    if (result > MAX_MINOR_UNITS || result < MIN_MINOR_UNITS) {
      return err('Addition would overflow');
    }

    return ok(new Money(result, this.#currency));
  }

  /**
   * Subtract another Money amount.
   *
   * @param {Money} other - The amount to subtract.
   * @returns {{ ok: true, value: Money } | { ok: false, error: string }}
   */
  subtract(other) {
    if (!(other instanceof Money)) {
      return err('Cannot subtract non-Money value');
    }

    if (other.#currency.code !== this.#currency.code) {
      return err('Cannot subtract different currencies');
    }

    const result = this.#minorUnits - other.#minorUnits;

    if (result > MAX_MINOR_UNITS || result < MIN_MINOR_UNITS) {
      return err('Subtraction would overflow');
    }

    return ok(new Money(result, this.#currency));
  }

  /**
   * Multiply by a scalar.
   *
   * @param {number} multiplier - The multiplier.
   * @returns {{ ok: true, value: Money } | { ok: false, error: string }}
   */
  multiply(multiplier) {
    if (typeof multiplier !== 'number' || !Number.isFinite(multiplier)) {
      return err('Multiplier must be a finite number');
    }

    const result = Math.round(this.#minorUnits * multiplier);

    if (result > MAX_MINOR_UNITS || result < MIN_MINOR_UNITS) {
      return err('Multiplication would overflow');
    }

    return ok(new Money(result, this.#currency));
  }

  /**
   * Divide by a scalar.
   *
   * @param {number} divisor - The divisor.
   * @returns {{ ok: true, value: Money } | { ok: false, error: string }}
   */
  divide(divisor) {
    if (typeof divisor !== 'number' || !Number.isFinite(divisor)) {
      return err('Divisor must be a finite number');
    }

    if (divisor === 0) {
      return err('Cannot divide by zero');
    }

    const result = Math.round(this.#minorUnits / divisor);

    return ok(new Money(result, this.#currency));
  }

  /**
   * Allocate money among multiple recipients.
   *
   * Handles remainder distribution to ensure no money is lost.
   *
   * @param {number[]} ratios - The allocation ratios.
   * @returns {{ ok: true, value: Money[] } | { ok: false, error: string }}
   *
   * @example
   * const [share1, share2] = money.allocate([1, 1]).value;
   * // Splits evenly, handles odd amounts correctly
   */
  allocate(ratios) {
    if (!Array.isArray(ratios) || ratios.length === 0) {
      return err('Ratios must be a non-empty array');
    }

    const totalRatio = ratios.reduce((sum, ratio) => sum + ratio, 0);
    if (totalRatio <= 0) {
      return err('Total ratio must be positive');
    }

    const allocations = [];
    let remainder = this.#minorUnits;

    for (let ratioIndex = 0; ratioIndex < ratios.length; ratioIndex++) {
      const ratio = ratios[ratioIndex];
      if (ratio < 0) {
        return err('Ratios must be non-negative');
      }

      const allocation = Math.floor((this.#minorUnits * ratio) / totalRatio);
      allocations.push(allocation);
      remainder -= allocation;
    }

    // Distribute remainder to first recipients
    for (let allocationIndex = 0; remainder > 0 && allocationIndex < allocations.length; allocationIndex++) {
      allocations[allocationIndex]++;
      remainder--;
    }

    const moneyAllocations = allocations.map((units) => new Money(units, this.#currency));
    return ok(moneyAllocations);
  }

  /**
   * Check if amount is zero.
   *
   * @returns {boolean}
   */
  isZero() {
    return this.#minorUnits === 0;
  }

  /**
   * Check if amount is positive.
   *
   * @returns {boolean}
   */
  isPositive() {
    return this.#minorUnits > 0;
  }

  /**
   * Check if amount is negative.
   *
   * @returns {boolean}
   */
  isNegative() {
    return this.#minorUnits < 0;
  }

  /**
   * Get absolute value.
   *
   * @returns {Money}
   */
  abs() {
    return new Money(Math.abs(this.#minorUnits), this.#currency);
  }

  /**
   * Negate the amount.
   *
   * @returns {Money}
   */
  negate() {
    return new Money(-this.#minorUnits, this.#currency);
  }

  /**
   * Compare with another Money.
   *
   * @param {Money} other - The other amount.
   * @returns {number} -1 if less, 0 if equal, 1 if greater.
   */
  compareTo(other) {
    if (!(other instanceof Money) || other.#currency.code !== this.#currency.code) {
      throw new Error('Cannot compare different currencies');
    }

    if (this.#minorUnits < other.#minorUnits) return -1;
    if (this.#minorUnits > other.#minorUnits) return 1;
    return 0;
  }

  /**
   * Check equality with another Money.
   *
   * @param {Money} other - The other amount.
   * @returns {boolean}
   */
  equals(other) {
    if (!(other instanceof Money)) return false;
    return this.#minorUnits === other.#minorUnits && this.#currency.code === other.#currency.code;
  }

  /**
   * Format as a string with currency symbol.
   *
   * @param {Object} [options] - Formatting options.
   * @param {boolean} [options.showSymbol=true] - Show currency symbol.
   * @param {boolean} [options.showCode=false] - Show currency code.
   * @param {string} [options.locale='en-US'] - Locale for number formatting.
   * @returns {string}
   *
   * @example
   * money.format()  // "$10.50"
   * money.format({ showCode: true })  // "$10.50 USD"
   */
  format(options = {}) {
    const { showSymbol = true, showCode = false, locale = 'en-US' } = options;

    const majorUnits = this.getMajorUnits();
    const formattedNumber = majorUnits.toLocaleString(locale, {
      minimumFractionDigits: this.#currency.minorUnits,
      maximumFractionDigits: this.#currency.minorUnits,
    });

    let result = '';
    if (showSymbol) {
      result += this.#currency.symbol;
    }
    result += formattedNumber;
    if (showCode) {
      result += ` ${this.#currency.code}`;
    }

    return result;
  }

  /**
   * Convert to string representation.
   *
   * @returns {string}
   */
  toString() {
    return this.format();
  }

  /**
   * Convert to JSON-serializable object.
   *
   * @returns {{ minorUnits: number, currencyCode: string }}
   */
  toJSON() {
    return {
      minorUnits: this.#minorUnits,
      currencyCode: this.#currency.code,
    };
  }
}
