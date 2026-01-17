// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe currency handling with type-safe operations.
 */

import { err, ok, type Result } from './result.ts';

/** ISO 4217 currency codes (commonly used subset). */
export enum CurrencyCode {
  /** United States Dollar */
  USD = 'USD',
  /** Euro */
  EUR = 'EUR',
  /** British Pound Sterling */
  GBP = 'GBP',
  /** Japanese Yen */
  JPY = 'JPY',
  /** Chinese Yuan */
  CNY = 'CNY',
  /** Australian Dollar */
  AUD = 'AUD',
  /** Canadian Dollar */
  CAD = 'CAD',
  /** Swiss Franc */
  CHF = 'CHF',
  /** Hong Kong Dollar */
  HKD = 'HKD',
  /** Singapore Dollar */
  SGD = 'SGD',
  /** Swedish Krona */
  SEK = 'SEK',
  /** Norwegian Krone */
  NOK = 'NOK',
  /** Danish Krone */
  DKK = 'DKK',
  /** New Zealand Dollar */
  NZD = 'NZD',
  /** South Korean Won */
  KRW = 'KRW',
  /** Indian Rupee */
  INR = 'INR',
  /** Brazilian Real */
  BRL = 'BRL',
  /** Mexican Peso */
  MXN = 'MXN',
  /** Russian Ruble */
  RUB = 'RUB',
  /** South African Rand */
  ZAR = 'ZAR',
  /** Turkish Lira */
  TRY = 'TRY',
  /** Polish Zloty */
  PLN = 'PLN',
  /** Thai Baht */
  THB = 'THB',
  /** Indonesian Rupiah */
  IDR = 'IDR',
  /** Malaysian Ringgit */
  MYR = 'MYR',
  /** Philippine Peso */
  PHP = 'PHP',
  /** Czech Koruna */
  CZK = 'CZK',
  /** Israeli New Shekel */
  ILS = 'ILS',
  /** UAE Dirham */
  AED = 'AED',
  /** Saudi Riyal */
  SAR = 'SAR',
  /** Bitcoin */
  BTC = 'BTC',
  /** Ethereum */
  ETH = 'ETH',
}

/** Currency metadata. */
interface CurrencyInfo {
  /** Currency code. */
  code: CurrencyCode;
  /** Number of decimal places. */
  decimalPlaces: number;
  /** Currency symbol. */
  symbol: string;
  /** Currency name. */
  name: string;
}

/** Currency metadata lookup. */
const CURRENCY_INFO: Record<CurrencyCode, CurrencyInfo> = {
  [CurrencyCode.USD]: { code: CurrencyCode.USD, decimalPlaces: 2, symbol: '$', name: 'US Dollar' },
  [CurrencyCode.EUR]: { code: CurrencyCode.EUR, decimalPlaces: 2, symbol: '€', name: 'Euro' },
  [CurrencyCode.GBP]: {
    code: CurrencyCode.GBP,
    decimalPlaces: 2,
    symbol: '£',
    name: 'British Pound',
  },
  [CurrencyCode.JPY]: {
    code: CurrencyCode.JPY,
    decimalPlaces: 0,
    symbol: '¥',
    name: 'Japanese Yen',
  },
  [CurrencyCode.CNY]: {
    code: CurrencyCode.CNY,
    decimalPlaces: 2,
    symbol: '¥',
    name: 'Chinese Yuan',
  },
  [CurrencyCode.AUD]: {
    code: CurrencyCode.AUD,
    decimalPlaces: 2,
    symbol: 'A$',
    name: 'Australian Dollar',
  },
  [CurrencyCode.CAD]: {
    code: CurrencyCode.CAD,
    decimalPlaces: 2,
    symbol: 'C$',
    name: 'Canadian Dollar',
  },
  [CurrencyCode.CHF]: {
    code: CurrencyCode.CHF,
    decimalPlaces: 2,
    symbol: 'CHF',
    name: 'Swiss Franc',
  },
  [CurrencyCode.HKD]: {
    code: CurrencyCode.HKD,
    decimalPlaces: 2,
    symbol: 'HK$',
    name: 'Hong Kong Dollar',
  },
  [CurrencyCode.SGD]: {
    code: CurrencyCode.SGD,
    decimalPlaces: 2,
    symbol: 'S$',
    name: 'Singapore Dollar',
  },
  [CurrencyCode.SEK]: {
    code: CurrencyCode.SEK,
    decimalPlaces: 2,
    symbol: 'kr',
    name: 'Swedish Krona',
  },
  [CurrencyCode.NOK]: {
    code: CurrencyCode.NOK,
    decimalPlaces: 2,
    symbol: 'kr',
    name: 'Norwegian Krone',
  },
  [CurrencyCode.DKK]: {
    code: CurrencyCode.DKK,
    decimalPlaces: 2,
    symbol: 'kr',
    name: 'Danish Krone',
  },
  [CurrencyCode.NZD]: {
    code: CurrencyCode.NZD,
    decimalPlaces: 2,
    symbol: 'NZ$',
    name: 'New Zealand Dollar',
  },
  [CurrencyCode.KRW]: {
    code: CurrencyCode.KRW,
    decimalPlaces: 0,
    symbol: '₩',
    name: 'South Korean Won',
  },
  [CurrencyCode.INR]: {
    code: CurrencyCode.INR,
    decimalPlaces: 2,
    symbol: '₹',
    name: 'Indian Rupee',
  },
  [CurrencyCode.BRL]: {
    code: CurrencyCode.BRL,
    decimalPlaces: 2,
    symbol: 'R$',
    name: 'Brazilian Real',
  },
  [CurrencyCode.MXN]: {
    code: CurrencyCode.MXN,
    decimalPlaces: 2,
    symbol: '$',
    name: 'Mexican Peso',
  },
  [CurrencyCode.RUB]: {
    code: CurrencyCode.RUB,
    decimalPlaces: 2,
    symbol: '₽',
    name: 'Russian Ruble',
  },
  [CurrencyCode.ZAR]: {
    code: CurrencyCode.ZAR,
    decimalPlaces: 2,
    symbol: 'R',
    name: 'South African Rand',
  },
  [CurrencyCode.TRY]: {
    code: CurrencyCode.TRY,
    decimalPlaces: 2,
    symbol: '₺',
    name: 'Turkish Lira',
  },
  [CurrencyCode.PLN]: {
    code: CurrencyCode.PLN,
    decimalPlaces: 2,
    symbol: 'zł',
    name: 'Polish Zloty',
  },
  [CurrencyCode.THB]: { code: CurrencyCode.THB, decimalPlaces: 2, symbol: '฿', name: 'Thai Baht' },
  [CurrencyCode.IDR]: {
    code: CurrencyCode.IDR,
    decimalPlaces: 0,
    symbol: 'Rp',
    name: 'Indonesian Rupiah',
  },
  [CurrencyCode.MYR]: {
    code: CurrencyCode.MYR,
    decimalPlaces: 2,
    symbol: 'RM',
    name: 'Malaysian Ringgit',
  },
  [CurrencyCode.PHP]: {
    code: CurrencyCode.PHP,
    decimalPlaces: 2,
    symbol: '₱',
    name: 'Philippine Peso',
  },
  [CurrencyCode.CZK]: {
    code: CurrencyCode.CZK,
    decimalPlaces: 2,
    symbol: 'Kč',
    name: 'Czech Koruna',
  },
  [CurrencyCode.ILS]: {
    code: CurrencyCode.ILS,
    decimalPlaces: 2,
    symbol: '₪',
    name: 'Israeli Shekel',
  },
  [CurrencyCode.AED]: {
    code: CurrencyCode.AED,
    decimalPlaces: 2,
    symbol: 'د.إ',
    name: 'UAE Dirham',
  },
  [CurrencyCode.SAR]: {
    code: CurrencyCode.SAR,
    decimalPlaces: 2,
    symbol: '﷼',
    name: 'Saudi Riyal',
  },
  [CurrencyCode.BTC]: { code: CurrencyCode.BTC, decimalPlaces: 8, symbol: '₿', name: 'Bitcoin' },
  [CurrencyCode.ETH]: { code: CurrencyCode.ETH, decimalPlaces: 18, symbol: 'Ξ', name: 'Ethereum' },
};

/**
 * Immutable Money class for type-safe currency operations.
 * Stores amounts in minor units (cents) to avoid floating-point precision issues.
 */
export class Money {
  /** Amount in minor units (e.g., cents for USD). */
  readonly minorUnits: bigint;
  /** Currency code. */
  readonly currency: CurrencyCode;

  private constructor(minorUnits: bigint, currency: CurrencyCode) {
    this.minorUnits = minorUnits;
    this.currency = currency;
  }

  /**
   * Create Money from minor units (e.g., cents).
   *
   * @example
   * ```ts
   * const amount = Money.fromMinorUnits(1099n, CurrencyCode.USD);
   * // Represents $10.99
   * ```
   */
  static fromMinorUnits(minorUnits: bigint, currency: CurrencyCode): Money {
    return new Money(minorUnits, currency);
  }

  /**
   * Create Money from major units (e.g., dollars).
   *
   * @example
   * ```ts
   * const amount = Money.fromMajorUnits(10.99, CurrencyCode.USD);
   * // Represents $10.99
   * ```
   */
  static fromMajorUnits(majorUnits: number, currency: CurrencyCode): Money {
    const info = CURRENCY_INFO[currency];
    const multiplier = 10 ** info.decimalPlaces;
    const minorUnits = BigInt(Math.round(majorUnits * multiplier));
    return new Money(minorUnits, currency);
  }

  /**
   * Parse a money string.
   *
   * @example
   * ```ts
   * const result = Money.parse("10.99", CurrencyCode.USD);
   * ```
   */
  static parse(value: string, currency: CurrencyCode): Result<Money> {
    const trimmed = value.trim().replace(/[,$\s]/g, '');
    if (!trimmed) {
      return err('Empty value');
    }

    const num = parseFloat(trimmed);
    if (isNaN(num) || !isFinite(num)) {
      return err('Invalid number');
    }

    return ok(Money.fromMajorUnits(num, currency));
  }

  /** Get amount in major units. */
  toMajorUnits(): number {
    const info = CURRENCY_INFO[this.currency];
    const divisor = 10 ** info.decimalPlaces;
    return Number(this.minorUnits) / divisor;
  }

  /** Add two Money amounts (must be same currency). */
  add(other: Money): Result<Money> {
    if (this.currency !== other.currency) {
      return err(`Currency mismatch: ${this.currency} vs ${other.currency}`);
    }
    return ok(new Money(this.minorUnits + other.minorUnits, this.currency));
  }

  /** Subtract Money amount (must be same currency). */
  subtract(other: Money): Result<Money> {
    if (this.currency !== other.currency) {
      return err(`Currency mismatch: ${this.currency} vs ${other.currency}`);
    }
    return ok(new Money(this.minorUnits - other.minorUnits, this.currency));
  }

  /** Multiply by a scalar. */
  multiply(factor: number): Money {
    const result = BigInt(Math.round(Number(this.minorUnits) * factor));
    return new Money(result, this.currency);
  }

  /** Divide by a scalar. */
  divide(divisor: number): Result<Money> {
    if (divisor === 0) {
      return err('Division by zero');
    }
    const result = BigInt(Math.round(Number(this.minorUnits) / divisor));
    return ok(new Money(result, this.currency));
  }

  /** Check if negative. */
  isNegative(): boolean {
    return this.minorUnits < 0n;
  }

  /** Check if zero. */
  isZero(): boolean {
    return this.minorUnits === 0n;
  }

  /** Check if positive. */
  isPositive(): boolean {
    return this.minorUnits > 0n;
  }

  /** Get absolute value. */
  abs(): Money {
    return this.minorUnits < 0n ? new Money(-this.minorUnits, this.currency) : this;
  }

  /** Negate the amount. */
  negate(): Money {
    return new Money(-this.minorUnits, this.currency);
  }

  /** Compare with another Money amount. Returns -1, 0, or 1. */
  compare(other: Money): Result<number> {
    if (this.currency !== other.currency) {
      return err(`Currency mismatch: ${this.currency} vs ${other.currency}`);
    }
    if (this.minorUnits < other.minorUnits) return ok(-1);
    if (this.minorUnits > other.minorUnits) return ok(1);
    return ok(0);
  }

  /** Check equality. */
  equals(other: Money): boolean {
    return this.currency === other.currency && this.minorUnits === other.minorUnits;
  }

  /**
   * Format as string with currency symbol.
   *
   * @example
   * ```ts
   * const money = Money.fromMajorUnits(1234.56, CurrencyCode.USD);
   * console.log(money.format()); // "$1,234.56"
   * ```
   */
  format(options?: { locale?: string; useSymbol?: boolean }): string {
    const info = CURRENCY_INFO[this.currency];
    const majorUnits = this.toMajorUnits();
    const locale = options?.locale ?? 'en-US';
    const useSymbol = options?.useSymbol ?? true;

    return new Intl.NumberFormat(locale, {
      style: 'currency',
      currency: this.currency,
      currencyDisplay: useSymbol ? 'symbol' : 'code',
      minimumFractionDigits: info.decimalPlaces,
      maximumFractionDigits: info.decimalPlaces,
    }).format(majorUnits);
  }

  /** Convert to string (ISO format: "10.99 USD"). */
  toString(): string {
    const info = CURRENCY_INFO[this.currency];
    return `${this.toMajorUnits().toFixed(info.decimalPlaces)} ${this.currency}`;
  }
}

/**
 * Safe currency operations.
 */
export class SafeCurrency {
  /** Get currency info by code. */
  static getInfo(code: CurrencyCode): CurrencyInfo {
    return CURRENCY_INFO[code];
  }

  /** Get decimal places for a currency. */
  static getDecimalPlaces(code: CurrencyCode): number {
    return CURRENCY_INFO[code].decimalPlaces;
  }

  /** Get currency symbol. */
  static getSymbol(code: CurrencyCode): string {
    return CURRENCY_INFO[code].symbol;
  }

  /** Get currency name. */
  static getName(code: CurrencyCode): string {
    return CURRENCY_INFO[code].name;
  }

  /** Check if a string is a valid currency code. */
  static isValidCode(code: string): code is CurrencyCode {
    return Object.values(CurrencyCode).includes(code as CurrencyCode);
  }

  /** Parse a currency code string. */
  static parseCode(code: string): Result<CurrencyCode> {
    const upper = code.trim().toUpperCase();
    if (this.isValidCode(upper)) {
      return ok(upper as CurrencyCode);
    }
    return err(`Invalid currency code: ${code}`);
  }

  /** Get all supported currency codes. */
  static allCodes(): CurrencyCode[] {
    return Object.values(CurrencyCode);
  }

  /** Check if currency is a cryptocurrency. */
  static isCrypto(code: CurrencyCode): boolean {
    return code === CurrencyCode.BTC || code === CurrencyCode.ETH;
  }

  /** Check if currency is a fiat currency. */
  static isFiat(code: CurrencyCode): boolean {
    return !this.isCrypto(code);
  }

  /**
   * Allocate money across multiple parts (handles rounding).
   * Useful for splitting bills or distributing funds.
   *
   * @example
   * ```ts
   * const total = Money.fromMajorUnits(100, CurrencyCode.USD);
   * const parts = SafeCurrency.allocate(total, [1, 1, 1]); // Split equally 3 ways
   * // parts = [$33.34, $33.33, $33.33]
   * ```
   */
  static allocate(money: Money, ratios: number[]): Money[] {
    if (ratios.length === 0) {
      return [];
    }

    const totalRatio = ratios.reduce((sum, r) => sum + r, 0);
    if (totalRatio === 0) {
      return ratios.map(() => Money.fromMinorUnits(0n, money.currency));
    }

    const results: Money[] = [];
    let remainder = money.minorUnits;

    for (let i = 0; i < ratios.length; i++) {
      const share = (money.minorUnits * BigInt(Math.round(ratios[i] * 1000))) /
        BigInt(Math.round(totalRatio * 1000));
      results.push(Money.fromMinorUnits(share, money.currency));
      remainder -= share;
    }

    // Distribute remainder (penny by penny) to first parts
    for (let i = 0; remainder > 0n && i < results.length; i++) {
      results[i] = Money.fromMinorUnits(results[i].minorUnits + 1n, money.currency);
      remainder -= 1n;
    }

    return results;
  }

  /** Zero money for a given currency. */
  static zero(currency: CurrencyCode): Money {
    return Money.fromMinorUnits(0n, currency);
  }
}
