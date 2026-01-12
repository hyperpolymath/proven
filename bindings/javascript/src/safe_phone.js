// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafePhone - Phone number operations that cannot crash.
 *
 * Provides safe phone number parsing, validation, and formatting
 * following E.164 and international standards.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Country code enumeration with calling codes and metadata.
 * @readonly
 * @enum {{ code: string, callingCode: string, name: string, pattern: RegExp, minLength: number, maxLength: number }}
 */
export const CountryCode = Object.freeze({
  US: {
    code: 'US',
    callingCode: '1',
    name: 'United States',
    pattern: /^[2-9]\d{2}[2-9]\d{6}$/,
    minLength: 10,
    maxLength: 10,
  },
  CA: {
    code: 'CA',
    callingCode: '1',
    name: 'Canada',
    pattern: /^[2-9]\d{2}[2-9]\d{6}$/,
    minLength: 10,
    maxLength: 10,
  },
  GB: {
    code: 'GB',
    callingCode: '44',
    name: 'United Kingdom',
    pattern: /^[1-9]\d{9,10}$/,
    minLength: 10,
    maxLength: 11,
  },
  DE: {
    code: 'DE',
    callingCode: '49',
    name: 'Germany',
    pattern: /^[1-9]\d{6,14}$/,
    minLength: 7,
    maxLength: 15,
  },
  FR: {
    code: 'FR',
    callingCode: '33',
    name: 'France',
    pattern: /^[1-9]\d{8}$/,
    minLength: 9,
    maxLength: 9,
  },
  IT: {
    code: 'IT',
    callingCode: '39',
    name: 'Italy',
    pattern: /^\d{9,11}$/,
    minLength: 9,
    maxLength: 11,
  },
  ES: {
    code: 'ES',
    callingCode: '34',
    name: 'Spain',
    pattern: /^[6-9]\d{8}$/,
    minLength: 9,
    maxLength: 9,
  },
  JP: {
    code: 'JP',
    callingCode: '81',
    name: 'Japan',
    pattern: /^[1-9]\d{8,9}$/,
    minLength: 9,
    maxLength: 10,
  },
  CN: {
    code: 'CN',
    callingCode: '86',
    name: 'China',
    pattern: /^1[3-9]\d{9}$/,
    minLength: 11,
    maxLength: 11,
  },
  IN: {
    code: 'IN',
    callingCode: '91',
    name: 'India',
    pattern: /^[6-9]\d{9}$/,
    minLength: 10,
    maxLength: 10,
  },
  AU: {
    code: 'AU',
    callingCode: '61',
    name: 'Australia',
    pattern: /^4\d{8}$|^[2-9]\d{7,8}$/,
    minLength: 8,
    maxLength: 9,
  },
  BR: {
    code: 'BR',
    callingCode: '55',
    name: 'Brazil',
    pattern: /^[1-9]{2}[2-9]\d{7,8}$/,
    minLength: 10,
    maxLength: 11,
  },
  MX: {
    code: 'MX',
    callingCode: '52',
    name: 'Mexico',
    pattern: /^[1-9]\d{9}$/,
    minLength: 10,
    maxLength: 10,
  },
  KR: {
    code: 'KR',
    callingCode: '82',
    name: 'South Korea',
    pattern: /^[1-9]\d{7,9}$/,
    minLength: 8,
    maxLength: 10,
  },
  RU: {
    code: 'RU',
    callingCode: '7',
    name: 'Russia',
    pattern: /^9\d{9}$/,
    minLength: 10,
    maxLength: 10,
  },
  ZA: {
    code: 'ZA',
    callingCode: '27',
    name: 'South Africa',
    pattern: /^[1-9]\d{8}$/,
    minLength: 9,
    maxLength: 9,
  },
  NL: {
    code: 'NL',
    callingCode: '31',
    name: 'Netherlands',
    pattern: /^[1-9]\d{8}$/,
    minLength: 9,
    maxLength: 9,
  },
  SE: {
    code: 'SE',
    callingCode: '46',
    name: 'Sweden',
    pattern: /^[1-9]\d{6,12}$/,
    minLength: 7,
    maxLength: 13,
  },
  CH: {
    code: 'CH',
    callingCode: '41',
    name: 'Switzerland',
    pattern: /^[1-9]\d{8}$/,
    minLength: 9,
    maxLength: 9,
  },
  PL: {
    code: 'PL',
    callingCode: '48',
    name: 'Poland',
    pattern: /^[1-9]\d{8}$/,
    minLength: 9,
    maxLength: 9,
  },
  SG: {
    code: 'SG',
    callingCode: '65',
    name: 'Singapore',
    pattern: /^[689]\d{7}$/,
    minLength: 8,
    maxLength: 8,
  },
  AE: {
    code: 'AE',
    callingCode: '971',
    name: 'United Arab Emirates',
    pattern: /^5[024568]\d{7}$/,
    minLength: 9,
    maxLength: 9,
  },
  IL: {
    code: 'IL',
    callingCode: '972',
    name: 'Israel',
    pattern: /^5\d{8}$/,
    minLength: 9,
    maxLength: 9,
  },
  NZ: {
    code: 'NZ',
    callingCode: '64',
    name: 'New Zealand',
    pattern: /^2\d{7,9}$|^[3-9]\d{6,7}$/,
    minLength: 7,
    maxLength: 10,
  },
  IE: {
    code: 'IE',
    callingCode: '353',
    name: 'Ireland',
    pattern: /^[1-9]\d{6,9}$/,
    minLength: 7,
    maxLength: 10,
  },
});

/**
 * Phone number type classification.
 * @readonly
 * @enum {string}
 */
export const PhoneNumberType = Object.freeze({
  MOBILE: 'mobile',
  FIXED_LINE: 'fixed_line',
  TOLL_FREE: 'toll_free',
  PREMIUM_RATE: 'premium_rate',
  UNKNOWN: 'unknown',
});

/**
 * Safe phone number class with parsing and formatting.
 */
export class PhoneNumber {
  /** @type {string} */
  #nationalNumber;

  /** @type {{ code: string, callingCode: string, name: string }} */
  #countryCode;

  /** @type {string|null} */
  #extension;

  /**
   * Create a PhoneNumber instance.
   * Use static factory methods instead of direct construction.
   *
   * @param {string} nationalNumber - The national significant number.
   * @param {{ code: string, callingCode: string, name: string }} countryCode - Country information.
   * @param {string|null} extension - Optional extension.
   */
  constructor(nationalNumber, countryCode, extension = null) {
    this.#nationalNumber = nationalNumber;
    this.#countryCode = countryCode;
    this.#extension = extension;
  }

  /**
   * Parse a phone number string.
   *
   * Accepts various formats:
   * - E.164: +14155551234
   * - International: +1 (415) 555-1234
   * - National: (415) 555-1234
   * - With extension: +14155551234 ext 123
   *
   * @param {string} phoneString - The phone number string.
   * @param {{ code: string, callingCode: string, name: string }} [defaultCountry] - Default country if not specified.
   * @returns {{ ok: true, value: PhoneNumber } | { ok: false, error: string }}
   *
   * @example
   * const result = PhoneNumber.parse('+14155551234');
   * const result2 = PhoneNumber.parse('4155551234', CountryCode.US);
   */
  static parse(phoneString, defaultCountry = null) {
    if (typeof phoneString !== 'string') {
      return err('Phone number must be a string');
    }

    let input = phoneString.trim();
    if (!input) {
      return err('Phone number cannot be empty');
    }

    // Extract extension
    let extension = null;
    const extensionMatch = input.match(/(?:ext|x|extension)[.\s]*(\d+)$/i);
    if (extensionMatch) {
      extension = extensionMatch[1];
      input = input.slice(0, extensionMatch.index).trim();
    }

    // Remove all non-digit characters except leading +
    const hasPlus = input.startsWith('+');
    const digits = input.replace(/\D/g, '');

    if (digits.length === 0) {
      return err('Phone number contains no digits');
    }

    if (digits.length < 7) {
      return err('Phone number too short');
    }

    if (digits.length > 15) {
      return err('Phone number too long');
    }

    // Try to detect country from E.164 format
    let countryCode = null;
    let nationalNumber = digits;

    if (hasPlus) {
      // Try to match calling codes
      const countryEntries = Object.values(CountryCode);

      // Sort by calling code length (longest first) for proper matching
      const sortedCountries = [...countryEntries].sort(
        (countryA, countryB) => countryB.callingCode.length - countryA.callingCode.length
      );

      for (const country of sortedCountries) {
        if (digits.startsWith(country.callingCode)) {
          countryCode = country;
          nationalNumber = digits.slice(country.callingCode.length);
          break;
        }
      }

      if (!countryCode) {
        return err('Unknown country calling code');
      }
    } else if (defaultCountry) {
      countryCode = defaultCountry;
      nationalNumber = digits;
    } else {
      return err('Country code required for non-E.164 format');
    }

    // Validate national number length
    if (nationalNumber.length < countryCode.minLength) {
      return err('National number too short for country');
    }

    if (nationalNumber.length > countryCode.maxLength) {
      return err('National number too long for country');
    }

    return ok(new PhoneNumber(nationalNumber, countryCode, extension));
  }

  /**
   * Create a phone number from components.
   *
   * @param {string} nationalNumber - The national significant number.
   * @param {{ code: string, callingCode: string, name: string }} countryCode - Country information.
   * @param {string|null} [extension] - Optional extension.
   * @returns {{ ok: true, value: PhoneNumber } | { ok: false, error: string }}
   */
  static fromComponents(nationalNumber, countryCode, extension = null) {
    if (typeof nationalNumber !== 'string') {
      return err('National number must be a string');
    }

    const digits = nationalNumber.replace(/\D/g, '');

    if (digits.length === 0) {
      return err('National number contains no digits');
    }

    if (!countryCode || typeof countryCode.callingCode !== 'string') {
      return err('Invalid country code');
    }

    if (digits.length < countryCode.minLength) {
      return err('National number too short for country');
    }

    if (digits.length > countryCode.maxLength) {
      return err('National number too long for country');
    }

    return ok(new PhoneNumber(digits, countryCode, extension));
  }

  /**
   * Check if a string is a valid phone number.
   *
   * @param {string} phoneString - The string to validate.
   * @param {{ code: string, callingCode: string, name: string }} [defaultCountry] - Default country.
   * @returns {boolean}
   */
  static isValid(phoneString, defaultCountry = null) {
    return PhoneNumber.parse(phoneString, defaultCountry).ok;
  }

  /**
   * Get the national significant number (digits only).
   *
   * @returns {string}
   */
  getNationalNumber() {
    return this.#nationalNumber;
  }

  /**
   * Get the country code information.
   *
   * @returns {{ code: string, callingCode: string, name: string }}
   */
  getCountryCode() {
    return this.#countryCode;
  }

  /**
   * Get the extension if present.
   *
   * @returns {string|null}
   */
  getExtension() {
    return this.#extension;
  }

  /**
   * Format in E.164 format (+14155551234).
   *
   * E.164 is the international telephone numbering standard
   * used for SMS, call routing, and storage.
   *
   * @returns {string}
   *
   * @example
   * phone.formatE164()  // "+14155551234"
   */
  formatE164() {
    const base = `+${this.#countryCode.callingCode}${this.#nationalNumber}`;
    return base;
  }

  /**
   * Format in international format (+1 415 555 1234).
   *
   * @returns {string}
   *
   * @example
   * phone.formatInternational()  // "+1 415 555 1234"
   */
  formatInternational() {
    const formatted = this.#formatNationalParts().join(' ');
    let result = `+${this.#countryCode.callingCode} ${formatted}`;

    if (this.#extension) {
      result += ` ext. ${this.#extension}`;
    }

    return result;
  }

  /**
   * Format in national format ((415) 555-1234).
   *
   * @returns {string}
   *
   * @example
   * phone.formatNational()  // "(415) 555-1234"
   */
  formatNational() {
    const parts = this.#formatNationalParts();

    let result;
    // US/CA format
    if (this.#countryCode.code === 'US' || this.#countryCode.code === 'CA') {
      if (parts.length === 3) {
        result = `(${parts[0]}) ${parts[1]}-${parts[2]}`;
      } else {
        result = parts.join(' ');
      }
    } else {
      result = parts.join(' ');
    }

    if (this.#extension) {
      result += ` ext. ${this.#extension}`;
    }

    return result;
  }

  /**
   * Format for RFC 3966 tel: URI.
   *
   * @returns {string}
   *
   * @example
   * phone.formatRfc3966()  // "tel:+14155551234"
   */
  formatRfc3966() {
    let uri = `tel:${this.formatE164()}`;

    if (this.#extension) {
      uri += `;ext=${this.#extension}`;
    }

    return uri;
  }

  /**
   * Format national number into logical groups.
   *
   * @private
   * @returns {string[]}
   */
  #formatNationalParts() {
    const num = this.#nationalNumber;

    // US/CA: 3-3-4
    if ((this.#countryCode.code === 'US' || this.#countryCode.code === 'CA') && num.length === 10) {
      return [num.slice(0, 3), num.slice(3, 6), num.slice(6)];
    }

    // UK: various formats
    if (this.#countryCode.code === 'GB') {
      if (num.length === 10) {
        return [num.slice(0, 4), num.slice(4, 7), num.slice(7)];
      }
      if (num.length === 11) {
        return [num.slice(0, 5), num.slice(5, 8), num.slice(8)];
      }
    }

    // France: 2-2-2-2-2
    if (this.#countryCode.code === 'FR' && num.length === 9) {
      return [num.slice(0, 1), num.slice(1, 3), num.slice(3, 5), num.slice(5, 7), num.slice(7)];
    }

    // Germany: variable
    if (this.#countryCode.code === 'DE') {
      if (num.length <= 9) {
        return [num.slice(0, 3), num.slice(3)];
      }
      return [num.slice(0, 4), num.slice(4, 7), num.slice(7)];
    }

    // Default: groups of 3-4
    const parts = [];
    let remaining = num;
    while (remaining.length > 4) {
      parts.push(remaining.slice(0, 3));
      remaining = remaining.slice(3);
    }
    if (remaining) {
      parts.push(remaining);
    }

    return parts;
  }

  /**
   * Get the phone number type (mobile, fixed line, etc.).
   *
   * @returns {string}
   */
  getType() {
    const num = this.#nationalNumber;

    // US/CA mobile detection
    if (this.#countryCode.code === 'US' || this.#countryCode.code === 'CA') {
      // Toll-free
      if (/^8(00|33|44|55|66|77|88)/.test(num)) {
        return PhoneNumberType.TOLL_FREE;
      }
      // Premium rate
      if (/^900/.test(num)) {
        return PhoneNumberType.PREMIUM_RATE;
      }
      // US doesn't distinguish mobile/fixed by number
      return PhoneNumberType.UNKNOWN;
    }

    // UK mobile
    if (this.#countryCode.code === 'GB' && /^7/.test(num)) {
      return PhoneNumberType.MOBILE;
    }

    // India mobile
    if (this.#countryCode.code === 'IN' && /^[6-9]/.test(num)) {
      return PhoneNumberType.MOBILE;
    }

    // China mobile
    if (this.#countryCode.code === 'CN' && /^1[3-9]/.test(num)) {
      return PhoneNumberType.MOBILE;
    }

    // Australia mobile
    if (this.#countryCode.code === 'AU' && /^4/.test(num)) {
      return PhoneNumberType.MOBILE;
    }

    return PhoneNumberType.UNKNOWN;
  }

  /**
   * Check equality with another PhoneNumber.
   *
   * @param {PhoneNumber} other - The other phone number.
   * @returns {boolean}
   */
  equals(other) {
    if (!(other instanceof PhoneNumber)) {
      return false;
    }

    return (
      this.#nationalNumber === other.#nationalNumber &&
      this.#countryCode.code === other.#countryCode.code &&
      this.#extension === other.#extension
    );
  }

  /**
   * Convert to string (E.164 format).
   *
   * @returns {string}
   */
  toString() {
    return this.formatE164();
  }

  /**
   * Convert to JSON-serializable object.
   *
   * @returns {{ e164: string, national: string, countryCode: string, extension: string|null }}
   */
  toJSON() {
    return {
      e164: this.formatE164(),
      national: this.getNationalNumber(),
      countryCode: this.#countryCode.code,
      extension: this.#extension,
    };
  }
}
