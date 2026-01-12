// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Proven - Safe, formally verified library for math, crypto, parsing, and validation.
 *
 * Deno bindings for the proven library.
 *
 * @module
 */

export { type MathResult, SafeMath } from './src/safe_math.ts';
export { SafeString } from './src/safe_string.ts';
export { type EmailParts, SafeEmail } from './src/safe_email.ts';
export { type ParsedUrl, SafeUrl } from './src/safe_url.ts';
export { SafePath } from './src/safe_path.ts';
export { type DateComponents, SafeDateTime, type TimeComponents } from './src/safe_datetime.ts';
export { type Cidr, Ipv4Class, SafeNetwork } from './src/safe_network.ts';
export { type PasswordPolicy, PasswordStrength, SafePassword } from './src/safe_password.ts';
export { SafeJson } from './src/safe_json.ts';
export { SafeUuid, type Uuid, UuidVariant, UuidVersion } from './src/safe_uuid.ts';
export { CurrencyCode, Money, SafeCurrency } from './src/safe_currency.ts';
export { CountryCode, type PhoneNumber, PhoneNumberType, SafePhone } from './src/safe_phone.ts';
export { HexCase, SafeHex } from './src/safe_hex.ts';
// v0.8.0 - Network Extended
export { type Header, SafeHeader } from './src/safe_header.ts';
export {
  type Cookie,
  type CookieAttributes,
  type CookiePrefix,
  DEFAULT_ATTRIBUTES,
  SafeCookie,
  type SameSite,
  SESSION_ATTRIBUTES,
} from './src/safe_cookie.ts';
export {
  type Charset,
  type ContentType,
  type MediaCategory,
  type MediaType,
  SafeContentType,
} from './src/safe_content_type.ts';
export { err, isErr, isOk, ok, type Result } from './src/result.ts';
