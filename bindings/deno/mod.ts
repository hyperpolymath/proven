// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Proven - Code that cannot crash.
 *
 * A formally verified safety library. All computation delegates to
 * Idris 2 (dependent types + totality checking) via the Zig FFI layer.
 * This Deno binding is a thin wrapper over libproven.
 *
 * @module
 */

// FFI infrastructure
export {
  close,
  getLoadError,
  isAvailable,
  ProvenStatus,
  statusToError,
} from './src/ffi.ts';

// Result utilities
export { err, isErr, isOk, ok, type Result, unwrap, unwrapOr } from './src/result.ts';

// Core modules
export { type MathResult, SafeMath } from './src/safe_math.ts';
export { SafeString } from './src/safe_string.ts';
export { SafePath } from './src/safe_path.ts';
export { Email, SafeEmail } from './src/safe_email.ts';
export { SafeUrl } from './src/safe_url.ts';
export { SafeCrypto } from './src/safe_crypto.ts';
export { SafeHex } from './src/safe_hex.ts';

// Data modules
export { JsonType, JsonValue, SafeJson } from './src/safe_json.ts';
export { SafeDateTime } from './src/safe_datetime.ts';
export { SafeFloat } from './src/safe_float.ts';
export { SafeAngle } from './src/safe_angle.ts';
export { LengthUnit, SafeUnit, TemperatureUnit } from './src/safe_unit.ts';

// Algorithm modules
export { type Coordinate, SafeGeo } from './src/safe_geo.ts';
export { SafeProbability } from './src/safe_probability.ts';
export { SafeChecksum } from './src/safe_checksum.ts';
export { SafeML } from './src/safe_ml.ts';
export { SafeCalculator } from './src/safe_calculator.ts';

// Security modules
export { PasswordStrength, SafePassword } from './src/safe_password.ts';
export { SafeHeader } from './src/safe_header.ts';
export { CookiePrefix, SafeCookie } from './src/safe_cookie.ts';
export { SafeContentType } from './src/safe_content_type.ts';

/**
 * Library version.
 */
export const VERSION = '0.9.0';
