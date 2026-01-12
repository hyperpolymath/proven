// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Safe phone number parsing and validation (E.164 format).
 */

import { err, ok, type Result } from './result.ts';

/** ISO 3166-1 alpha-2 country codes with calling codes (commonly used subset). */
export enum CountryCode {
  /** United States */
  US = 'US',
  /** Canada */
  CA = 'CA',
  /** United Kingdom */
  GB = 'GB',
  /** Germany */
  DE = 'DE',
  /** France */
  FR = 'FR',
  /** Italy */
  IT = 'IT',
  /** Spain */
  ES = 'ES',
  /** Netherlands */
  NL = 'NL',
  /** Belgium */
  BE = 'BE',
  /** Switzerland */
  CH = 'CH',
  /** Austria */
  AT = 'AT',
  /** Australia */
  AU = 'AU',
  /** New Zealand */
  NZ = 'NZ',
  /** Japan */
  JP = 'JP',
  /** China */
  CN = 'CN',
  /** South Korea */
  KR = 'KR',
  /** India */
  IN = 'IN',
  /** Brazil */
  BR = 'BR',
  /** Mexico */
  MX = 'MX',
  /** Russia */
  RU = 'RU',
  /** South Africa */
  ZA = 'ZA',
  /** Singapore */
  SG = 'SG',
  /** Hong Kong */
  HK = 'HK',
  /** Israel */
  IL = 'IL',
  /** United Arab Emirates */
  AE = 'AE',
  /** Saudi Arabia */
  SA = 'SA',
  /** Sweden */
  SE = 'SE',
  /** Norway */
  NO = 'NO',
  /** Denmark */
  DK = 'DK',
  /** Finland */
  FI = 'FI',
  /** Poland */
  PL = 'PL',
  /** Ireland */
  IE = 'IE',
  /** Portugal */
  PT = 'PT',
  /** Greece */
  GR = 'GR',
  /** Turkey */
  TR = 'TR',
  /** Argentina */
  AR = 'AR',
  /** Chile */
  CL = 'CL',
  /** Colombia */
  CO = 'CO',
  /** Indonesia */
  ID = 'ID',
  /** Malaysia */
  MY = 'MY',
  /** Thailand */
  TH = 'TH',
  /** Philippines */
  PH = 'PH',
  /** Vietnam */
  VN = 'VN',
  /** Egypt */
  EG = 'EG',
  /** Nigeria */
  NG = 'NG',
  /** Kenya */
  KE = 'KE',
  /** Czech Republic */
  CZ = 'CZ',
  /** Hungary */
  HU = 'HU',
  /** Romania */
  RO = 'RO',
  /** Ukraine */
  UA = 'UA',
}

/** Country calling code metadata. */
interface CountryInfo {
  /** Country code. */
  code: CountryCode;
  /** Calling code (e.g., "1" for US). */
  callingCode: string;
  /** Country name. */
  name: string;
  /** Example national number format. */
  exampleFormat: string;
  /** Minimum national number length. */
  minLength: number;
  /** Maximum national number length. */
  maxLength: number;
}

/** Country metadata lookup. */
const COUNTRY_INFO: Record<CountryCode, CountryInfo> = {
  [CountryCode.US]: {
    code: CountryCode.US,
    callingCode: '1',
    name: 'United States',
    exampleFormat: '(XXX) XXX-XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.CA]: {
    code: CountryCode.CA,
    callingCode: '1',
    name: 'Canada',
    exampleFormat: '(XXX) XXX-XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.GB]: {
    code: CountryCode.GB,
    callingCode: '44',
    name: 'United Kingdom',
    exampleFormat: 'XXXX XXXXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.DE]: {
    code: CountryCode.DE,
    callingCode: '49',
    name: 'Germany',
    exampleFormat: 'XXXX XXXXXXX',
    minLength: 10,
    maxLength: 11,
  },
  [CountryCode.FR]: {
    code: CountryCode.FR,
    callingCode: '33',
    name: 'France',
    exampleFormat: 'X XX XX XX XX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.IT]: {
    code: CountryCode.IT,
    callingCode: '39',
    name: 'Italy',
    exampleFormat: 'XXX XXX XXXX',
    minLength: 9,
    maxLength: 10,
  },
  [CountryCode.ES]: {
    code: CountryCode.ES,
    callingCode: '34',
    name: 'Spain',
    exampleFormat: 'XXX XX XX XX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.NL]: {
    code: CountryCode.NL,
    callingCode: '31',
    name: 'Netherlands',
    exampleFormat: 'XX XXX XXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.BE]: {
    code: CountryCode.BE,
    callingCode: '32',
    name: 'Belgium',
    exampleFormat: 'XXX XX XX XX',
    minLength: 8,
    maxLength: 9,
  },
  [CountryCode.CH]: {
    code: CountryCode.CH,
    callingCode: '41',
    name: 'Switzerland',
    exampleFormat: 'XX XXX XX XX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.AT]: {
    code: CountryCode.AT,
    callingCode: '43',
    name: 'Austria',
    exampleFormat: 'XXX XXXXXXX',
    minLength: 10,
    maxLength: 13,
  },
  [CountryCode.AU]: {
    code: CountryCode.AU,
    callingCode: '61',
    name: 'Australia',
    exampleFormat: 'XXXX XXX XXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.NZ]: {
    code: CountryCode.NZ,
    callingCode: '64',
    name: 'New Zealand',
    exampleFormat: 'XX XXX XXXX',
    minLength: 8,
    maxLength: 10,
  },
  [CountryCode.JP]: {
    code: CountryCode.JP,
    callingCode: '81',
    name: 'Japan',
    exampleFormat: 'XX-XXXX-XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.CN]: {
    code: CountryCode.CN,
    callingCode: '86',
    name: 'China',
    exampleFormat: 'XXX XXXX XXXX',
    minLength: 11,
    maxLength: 11,
  },
  [CountryCode.KR]: {
    code: CountryCode.KR,
    callingCode: '82',
    name: 'South Korea',
    exampleFormat: 'XX-XXXX-XXXX',
    minLength: 9,
    maxLength: 10,
  },
  [CountryCode.IN]: {
    code: CountryCode.IN,
    callingCode: '91',
    name: 'India',
    exampleFormat: 'XXXXX XXXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.BR]: {
    code: CountryCode.BR,
    callingCode: '55',
    name: 'Brazil',
    exampleFormat: '(XX) XXXXX-XXXX',
    minLength: 10,
    maxLength: 11,
  },
  [CountryCode.MX]: {
    code: CountryCode.MX,
    callingCode: '52',
    name: 'Mexico',
    exampleFormat: 'XXX XXX XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.RU]: {
    code: CountryCode.RU,
    callingCode: '7',
    name: 'Russia',
    exampleFormat: 'XXX XXX-XX-XX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.ZA]: {
    code: CountryCode.ZA,
    callingCode: '27',
    name: 'South Africa',
    exampleFormat: 'XX XXX XXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.SG]: {
    code: CountryCode.SG,
    callingCode: '65',
    name: 'Singapore',
    exampleFormat: 'XXXX XXXX',
    minLength: 8,
    maxLength: 8,
  },
  [CountryCode.HK]: {
    code: CountryCode.HK,
    callingCode: '852',
    name: 'Hong Kong',
    exampleFormat: 'XXXX XXXX',
    minLength: 8,
    maxLength: 8,
  },
  [CountryCode.IL]: {
    code: CountryCode.IL,
    callingCode: '972',
    name: 'Israel',
    exampleFormat: 'XX-XXX-XXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.AE]: {
    code: CountryCode.AE,
    callingCode: '971',
    name: 'UAE',
    exampleFormat: 'XX XXX XXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.SA]: {
    code: CountryCode.SA,
    callingCode: '966',
    name: 'Saudi Arabia',
    exampleFormat: 'XX XXX XXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.SE]: {
    code: CountryCode.SE,
    callingCode: '46',
    name: 'Sweden',
    exampleFormat: 'XX-XXX XX XX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.NO]: {
    code: CountryCode.NO,
    callingCode: '47',
    name: 'Norway',
    exampleFormat: 'XXX XX XXX',
    minLength: 8,
    maxLength: 8,
  },
  [CountryCode.DK]: {
    code: CountryCode.DK,
    callingCode: '45',
    name: 'Denmark',
    exampleFormat: 'XX XX XX XX',
    minLength: 8,
    maxLength: 8,
  },
  [CountryCode.FI]: {
    code: CountryCode.FI,
    callingCode: '358',
    name: 'Finland',
    exampleFormat: 'XX XXX XXXX',
    minLength: 9,
    maxLength: 10,
  },
  [CountryCode.PL]: {
    code: CountryCode.PL,
    callingCode: '48',
    name: 'Poland',
    exampleFormat: 'XXX XXX XXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.IE]: {
    code: CountryCode.IE,
    callingCode: '353',
    name: 'Ireland',
    exampleFormat: 'XX XXX XXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.PT]: {
    code: CountryCode.PT,
    callingCode: '351',
    name: 'Portugal',
    exampleFormat: 'XXX XXX XXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.GR]: {
    code: CountryCode.GR,
    callingCode: '30',
    name: 'Greece',
    exampleFormat: 'XXX XXX XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.TR]: {
    code: CountryCode.TR,
    callingCode: '90',
    name: 'Turkey',
    exampleFormat: 'XXX XXX XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.AR]: {
    code: CountryCode.AR,
    callingCode: '54',
    name: 'Argentina',
    exampleFormat: 'XX XXXX-XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.CL]: {
    code: CountryCode.CL,
    callingCode: '56',
    name: 'Chile',
    exampleFormat: 'X XXXX XXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.CO]: {
    code: CountryCode.CO,
    callingCode: '57',
    name: 'Colombia',
    exampleFormat: 'XXX XXX XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.ID]: {
    code: CountryCode.ID,
    callingCode: '62',
    name: 'Indonesia',
    exampleFormat: 'XXX-XXXX-XXXX',
    minLength: 9,
    maxLength: 12,
  },
  [CountryCode.MY]: {
    code: CountryCode.MY,
    callingCode: '60',
    name: 'Malaysia',
    exampleFormat: 'XX-XXX XXXX',
    minLength: 9,
    maxLength: 10,
  },
  [CountryCode.TH]: {
    code: CountryCode.TH,
    callingCode: '66',
    name: 'Thailand',
    exampleFormat: 'XX XXX XXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.PH]: {
    code: CountryCode.PH,
    callingCode: '63',
    name: 'Philippines',
    exampleFormat: 'XXX XXX XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.VN]: {
    code: CountryCode.VN,
    callingCode: '84',
    name: 'Vietnam',
    exampleFormat: 'XX XXX XX XX',
    minLength: 9,
    maxLength: 10,
  },
  [CountryCode.EG]: {
    code: CountryCode.EG,
    callingCode: '20',
    name: 'Egypt',
    exampleFormat: 'XX XXXX XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.NG]: {
    code: CountryCode.NG,
    callingCode: '234',
    name: 'Nigeria',
    exampleFormat: 'XXX XXX XXXX',
    minLength: 10,
    maxLength: 10,
  },
  [CountryCode.KE]: {
    code: CountryCode.KE,
    callingCode: '254',
    name: 'Kenya',
    exampleFormat: 'XXX XXXXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.CZ]: {
    code: CountryCode.CZ,
    callingCode: '420',
    name: 'Czech Republic',
    exampleFormat: 'XXX XXX XXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.HU]: {
    code: CountryCode.HU,
    callingCode: '36',
    name: 'Hungary',
    exampleFormat: 'XX XXX XXXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.RO]: {
    code: CountryCode.RO,
    callingCode: '40',
    name: 'Romania',
    exampleFormat: 'XXX XXX XXX',
    minLength: 9,
    maxLength: 9,
  },
  [CountryCode.UA]: {
    code: CountryCode.UA,
    callingCode: '380',
    name: 'Ukraine',
    exampleFormat: 'XX XXX XXXX',
    minLength: 9,
    maxLength: 9,
  },
};

/** Phone number type. */
export enum PhoneNumberType {
  /** Mobile phone number. */
  Mobile = 'mobile',
  /** Fixed line (landline). */
  FixedLine = 'fixed-line',
  /** Toll-free number. */
  TollFree = 'toll-free',
  /** Premium rate number. */
  PremiumRate = 'premium-rate',
  /** Unknown type. */
  Unknown = 'unknown',
}

/** Parsed phone number structure. */
export interface PhoneNumber {
  /** Country code. */
  readonly countryCode: CountryCode;
  /** Calling code (without +). */
  readonly callingCode: string;
  /** National number (without country code). */
  readonly nationalNumber: string;
  /** Full E.164 format (e.g., "+14155552671"). */
  readonly e164: string;
  /** Phone number type. */
  readonly type: PhoneNumberType;
}

/**
 * Safe phone number operations.
 */
export class SafePhone {
  /**
   * Parse a phone number with country hint.
   *
   * @example
   * ```ts
   * const result = SafePhone.parse("(415) 555-2671", CountryCode.US);
   * if (result.ok) {
   *   console.log(result.value.e164); // "+14155552671"
   * }
   * ```
   */
  static parse(phoneNumber: string, countryHint: CountryCode): Result<PhoneNumber> {
    // Remove all non-digit characters except leading +
    const hasPlus = phoneNumber.trim().startsWith('+');
    const digits = phoneNumber.replace(/\D/g, '');

    if (!digits) {
      return err('Empty phone number');
    }

    const countryInfo = COUNTRY_INFO[countryHint];
    let nationalNumber: string;
    let callingCode: string;

    // Handle E.164 format (starts with +)
    if (hasPlus && digits.startsWith(countryInfo.callingCode)) {
      callingCode = countryInfo.callingCode;
      nationalNumber = digits.slice(callingCode.length);
    } else if (digits.length >= countryInfo.minLength && digits.length <= countryInfo.maxLength) {
      // Assume national format
      callingCode = countryInfo.callingCode;
      nationalNumber = digits;
    } else if (digits.startsWith(countryInfo.callingCode)) {
      // International format without +
      callingCode = countryInfo.callingCode;
      nationalNumber = digits.slice(callingCode.length);
    } else {
      // Try to detect country from leading digits
      const detected = this.detectCountry(digits);
      if (detected) {
        callingCode = COUNTRY_INFO[detected].callingCode;
        nationalNumber = digits.slice(callingCode.length);
      } else {
        return err('Unable to parse phone number');
      }
    }

    // Validate national number length
    if (
      nationalNumber.length < countryInfo.minLength || nationalNumber.length > countryInfo.maxLength
    ) {
      return err(`Invalid phone number length for ${countryInfo.name}`);
    }

    const e164 = `+${callingCode}${nationalNumber}`;
    const type = this.detectType(nationalNumber, countryHint);

    return ok({
      countryCode: countryHint,
      callingCode,
      nationalNumber,
      e164,
      type,
    });
  }

  /**
   * Parse an E.164 formatted phone number.
   *
   * @example
   * ```ts
   * const result = SafePhone.parseE164("+14155552671");
   * ```
   */
  static parseE164(e164: string): Result<PhoneNumber> {
    const trimmed = e164.trim();

    if (!trimmed.startsWith('+')) {
      return err('E.164 must start with +');
    }

    const digits = trimmed.slice(1).replace(/\D/g, '');

    if (digits.length < 7 || digits.length > 15) {
      return err('Invalid E.164 length');
    }

    // Detect country from calling code
    const country = this.detectCountry(digits);
    if (!country) {
      return err('Unknown country code');
    }

    return this.parse(trimmed, country);
  }

  /** Format a phone number in E.164 format. */
  static formatE164(phone: PhoneNumber): string {
    return phone.e164;
  }

  /**
   * Format a phone number in national format.
   *
   * @example
   * ```ts
   * const phone = SafePhone.parse("4155552671", CountryCode.US).value;
   * console.log(SafePhone.formatNational(phone)); // "(415) 555-2671"
   * ```
   */
  static formatNational(phone: PhoneNumber): string {
    const _info = COUNTRY_INFO[phone.countryCode];
    const digits = phone.nationalNumber;

    // Simple formatting based on country
    switch (phone.countryCode) {
      case CountryCode.US:
      case CountryCode.CA:
        if (digits.length === 10) {
          return `(${digits.slice(0, 3)}) ${digits.slice(3, 6)}-${digits.slice(6)}`;
        }
        break;
      case CountryCode.GB:
        if (digits.length === 10) {
          return `${digits.slice(0, 4)} ${digits.slice(4)}`;
        }
        break;
      case CountryCode.DE:
        if (digits.length >= 10) {
          return `${digits.slice(0, 4)} ${digits.slice(4)}`;
        }
        break;
      case CountryCode.FR:
        if (digits.length === 9) {
          return `${digits[0]} ${digits.slice(1, 3)} ${digits.slice(3, 5)} ${digits.slice(5, 7)} ${
            digits.slice(7)
          }`;
        }
        break;
      case CountryCode.JP:
        if (digits.length === 10) {
          return `${digits.slice(0, 2)}-${digits.slice(2, 6)}-${digits.slice(6)}`;
        }
        break;
    }

    // Default: return as-is with spaces every 3-4 digits
    return digits.replace(/(\d{3,4})(?=\d)/g, '$1 ').trim();
  }

  /**
   * Format a phone number in international format.
   *
   * @example
   * ```ts
   * const phone = SafePhone.parse("4155552671", CountryCode.US).value;
   * console.log(SafePhone.formatInternational(phone)); // "+1 415 555 2671"
   * ```
   */
  static formatInternational(phone: PhoneNumber): string {
    const digits = phone.nationalNumber;
    const formatted = digits.replace(/(\d{3,4})(?=\d)/g, '$1 ').trim();
    return `+${phone.callingCode} ${formatted}`;
  }

  /** Check if a string is a valid phone number for a country. */
  static isValid(phoneNumber: string, countryHint: CountryCode): boolean {
    return this.parse(phoneNumber, countryHint).ok;
  }

  /** Check if a string is a valid E.164 phone number. */
  static isValidE164(e164: string): boolean {
    return this.parseE164(e164).ok;
  }

  /** Get country info by code. */
  static getCountryInfo(code: CountryCode): CountryInfo {
    return COUNTRY_INFO[code];
  }

  /** Get calling code for a country. */
  static getCallingCode(code: CountryCode): string {
    return COUNTRY_INFO[code].callingCode;
  }

  /** Get all supported country codes. */
  static allCountryCodes(): CountryCode[] {
    return Object.values(CountryCode);
  }

  /** Detect country from phone digits. */
  private static detectCountry(digits: string): CountryCode | null {
    // Try matching from longest to shortest calling codes
    const sortedEntries = Object.entries(COUNTRY_INFO).sort(
      ([, a], [, b]) => b.callingCode.length - a.callingCode.length,
    );

    for (const [code, info] of sortedEntries) {
      if (digits.startsWith(info.callingCode)) {
        return code as CountryCode;
      }
    }

    return null;
  }

  /** Detect phone number type (basic heuristics). */
  private static detectType(nationalNumber: string, country: CountryCode): PhoneNumberType {
    const firstDigit = nationalNumber[0];

    // US/Canada mobile detection (basic)
    if (
      (country === CountryCode.US || country === CountryCode.CA) && nationalNumber.length === 10
    ) {
      // Toll-free prefixes
      if (
        ['800', '888', '877', '866', '855', '844', '833'].some((p) => nationalNumber.startsWith(p))
      ) {
        return PhoneNumberType.TollFree;
      }
      // Premium rate
      if (nationalNumber.startsWith('900')) {
        return PhoneNumberType.PremiumRate;
      }
    }

    // UK mobile detection
    if (country === CountryCode.GB && firstDigit === '7') {
      return PhoneNumberType.Mobile;
    }

    // German mobile detection
    if (
      country === CountryCode.DE && ['15', '16', '17'].some((p) => nationalNumber.startsWith(p))
    ) {
      return PhoneNumberType.Mobile;
    }

    // French mobile detection
    if (country === CountryCode.FR && ['6', '7'].includes(firstDigit)) {
      return PhoneNumberType.Mobile;
    }

    // Default to unknown - would need libphonenumber for accurate detection
    return PhoneNumberType.Unknown;
  }
}
