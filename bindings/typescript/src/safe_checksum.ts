// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * CRC-32 lookup table.
 */
const CRC32_TABLE = (() => {
  const table = new Uint32Array(256);
  for (let i = 0; i < 256; i++) {
    let c = i;
    for (let j = 0; j < 8; j++) {
      c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
    }
    table[i] = c >>> 0;
  }
  return table;
})();

/**
 * Calculate CRC-32 checksum.
 */
export function crc32(data: Uint8Array | string): number {
  const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
  let crc = 0xffffffff;

  for (let i = 0; i < bytes.length; i++) {
    crc = CRC32_TABLE[(crc ^ bytes[i]) & 0xff] ^ (crc >>> 8);
  }

  return (crc ^ 0xffffffff) >>> 0;
}

/**
 * Calculate CRC-32 and return as hex string.
 */
export function crc32Hex(data: Uint8Array | string): string {
  return crc32(data).toString(16).padStart(8, '0');
}

/**
 * Calculate Adler-32 checksum.
 */
export function adler32(data: Uint8Array | string): number {
  const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
  const MOD = 65521;

  let a = 1;
  let b = 0;

  for (let i = 0; i < bytes.length; i++) {
    a = (a + bytes[i]) % MOD;
    b = (b + a) % MOD;
  }

  return ((b << 16) | a) >>> 0;
}

/**
 * Calculate Adler-32 and return as hex string.
 */
export function adler32Hex(data: Uint8Array | string): string {
  return adler32(data).toString(16).padStart(8, '0');
}

/**
 * Calculate FNV-1a hash (32-bit).
 */
export function fnv1a32(data: Uint8Array | string): number {
  const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
  const FNV_PRIME = 0x01000193;
  const FNV_OFFSET = 0x811c9dc5;

  let hash = FNV_OFFSET;

  for (let i = 0; i < bytes.length; i++) {
    hash ^= bytes[i];
    hash = Math.imul(hash, FNV_PRIME);
  }

  return hash >>> 0;
}

/**
 * Calculate FNV-1a (32-bit) and return as hex string.
 */
export function fnv1a32Hex(data: Uint8Array | string): string {
  return fnv1a32(data).toString(16).padStart(8, '0');
}

/**
 * Validate Luhn checksum (credit card numbers, etc.).
 */
export function luhnValidate(number: string): boolean {
  const digits = number.replace(/\D/g, '');
  if (digits.length === 0) return false;

  let sum = 0;
  let alternate = false;

  for (let i = digits.length - 1; i >= 0; i--) {
    let n = parseInt(digits[i], 10);
    if (alternate) {
      n *= 2;
      if (n > 9) n -= 9;
    }
    sum += n;
    alternate = !alternate;
  }

  return sum % 10 === 0;
}

/**
 * Calculate Luhn check digit.
 */
export function luhnCheckDigit(number: string): number {
  const digits = number.replace(/\D/g, '');

  let sum = 0;
  let alternate = true;

  for (let i = digits.length - 1; i >= 0; i--) {
    let n = parseInt(digits[i], 10);
    if (alternate) {
      n *= 2;
      if (n > 9) n -= 9;
    }
    sum += n;
    alternate = !alternate;
  }

  return (10 - (sum % 10)) % 10;
}

/**
 * Append Luhn check digit to a number string.
 */
export function luhnAppend(number: string): string {
  const digits = number.replace(/\D/g, '');
  return digits + luhnCheckDigit(digits);
}

/**
 * Calculate simple XOR checksum.
 */
export function xorChecksum(data: Uint8Array | string): number {
  const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
  let checksum = 0;
  for (let i = 0; i < bytes.length; i++) {
    checksum ^= bytes[i];
  }
  return checksum;
}

/**
 * Calculate simple sum checksum (modulo 256).
 */
export function sumChecksum(data: Uint8Array | string): number {
  const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
  let sum = 0;
  for (let i = 0; i < bytes.length; i++) {
    sum = (sum + bytes[i]) & 0xff;
  }
  return sum;
}

/**
 * Calculate Fletcher-16 checksum.
 */
export function fletcher16(data: Uint8Array | string): number {
  const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
  let sum1 = 0;
  let sum2 = 0;

  for (let i = 0; i < bytes.length; i++) {
    sum1 = (sum1 + bytes[i]) % 255;
    sum2 = (sum2 + sum1) % 255;
  }

  return (sum2 << 8) | sum1;
}

/**
 * Calculate Fletcher-16 and return as hex string.
 */
export function fletcher16Hex(data: Uint8Array | string): string {
  return fletcher16(data).toString(16).padStart(4, '0');
}

/**
 * Validate ISBN-10 checksum.
 */
export function isbn10Validate(isbn: string): boolean {
  const digits = isbn.replace(/[\s-]/g, '');
  if (digits.length !== 10) return false;

  let sum = 0;
  for (let i = 0; i < 9; i++) {
    const d = parseInt(digits[i], 10);
    if (isNaN(d)) return false;
    sum += d * (10 - i);
  }

  const last = digits[9].toUpperCase();
  if (last === 'X') {
    sum += 10;
  } else {
    const d = parseInt(last, 10);
    if (isNaN(d)) return false;
    sum += d;
  }

  return sum % 11 === 0;
}

/**
 * Validate ISBN-13 checksum.
 */
export function isbn13Validate(isbn: string): boolean {
  const digits = isbn.replace(/[\s-]/g, '');
  if (digits.length !== 13) return false;

  let sum = 0;
  for (let i = 0; i < 13; i++) {
    const d = parseInt(digits[i], 10);
    if (isNaN(d)) return false;
    sum += d * (i % 2 === 0 ? 1 : 3);
  }

  return sum % 10 === 0;
}

/**
 * Calculate IBAN checksum.
 */
export function ibanValidate(iban: string): boolean {
  const normalized = iban.replace(/\s/g, '').toUpperCase();
  if (!/^[A-Z]{2}\d{2}[A-Z0-9]+$/.test(normalized)) return false;

  // Move first 4 chars to end
  const rearranged = normalized.slice(4) + normalized.slice(0, 4);

  // Convert letters to numbers (A=10, B=11, etc.)
  let numStr = '';
  for (const char of rearranged) {
    if (char >= 'A' && char <= 'Z') {
      numStr += (char.charCodeAt(0) - 55).toString();
    } else {
      numStr += char;
    }
  }

  // Calculate mod 97 using string division
  let remainder = 0;
  for (const char of numStr) {
    remainder = (remainder * 10 + parseInt(char, 10)) % 97;
  }

  return remainder === 1;
}

export const SafeChecksum = {
  crc32,
  crc32Hex,
  adler32,
  adler32Hex,
  fnv1a32,
  fnv1a32Hex,
  luhnValidate,
  luhnCheckDigit,
  luhnAppend,
  xorChecksum,
  sumChecksum,
  fletcher16,
  fletcher16Hex,
  isbn10Validate,
  isbn13Validate,
  ibanValidate,
};
