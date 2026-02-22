// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Proven - Code that cannot crash.
 *
 * A formally verified safety library. All computation delegates to
 * Idris 2 (dependent types + totality checking) via the Zig FFI layer.
 * This JavaScript binding is a thin wrapper over libproven.
 *
 * @module
 */

// FFI infrastructure
export { ProvenStatus, statusToError, isAvailable, getLoadError, close } from './ffi.js';

// Result utilities
export { ok, err, isOk, isErr, unwrap, unwrapOr } from './result.js';

// Core modules
export { SafeMath } from './safe_math.js';
export { SafeString } from './safe_string.js';
export { SafePath } from './safe_path.js';
export { SafeEmail, Email } from './safe_email.js';
export { SafeUrl, Url } from './safe_url.js';
export { SafeNetwork, Ipv4Address, Cidr } from './safe_network.js';
export { SafeCrypto, ConstantTime, SecureRandom, Base64, Hash, Hmac } from './safe_crypto.js';
export { SafeUUID, UuidVersion, UuidVariant } from './safe_uuid.js';
export { Money, CurrencyCode } from './safe_currency.js';
export { PhoneNumber, CountryCode, PhoneNumberType } from './safe_phone.js';
export { SafeHex } from './safe_hex.js';

// Data modules
export { SafeJson, JsonValue, JsonType } from './safe_json.js';
export { SafeDateTime, DateTime } from './safe_datetime.js';
export { SafeFloat } from './safe_float.js';
export { SafeVersion, SemVer, VersionConstraint } from './safe_version.js';
export { SafeColor, Rgb, Rgba } from './safe_color.js';
export { SafeAngle, Degrees, Radians } from './safe_angle.js';
export { SafeUnit, LengthUnit, MassUnit, TemperatureUnit, TimeUnit, DataUnit } from './safe_unit.js';

// Data structure modules
export { BoundedBuffer, RingBuffer } from './safe_buffer.js';
export { BoundedQueue, PriorityQueue } from './safe_queue.js';
export { BloomFilter } from './safe_bloom.js';
export { LruCache } from './safe_lru.js';
export { DirectedGraph, Edge, SafeGraph } from './safe_graph.js';

// Algorithm modules
export { SafeGeo, Coordinate, Distance } from './safe_geo.js';
export { SafeProbability, Probability } from './safe_probability.js';
export { SafeChecksum, Crc32, Adler32, Fnv, Luhn } from './safe_checksum.js';
export { Vector, Matrix } from './safe_tensor.js';
export { SafeML } from './safe_ml.js';
export { SafeCalculator, Expression } from './safe_calculator.js';

// Security modules
export { SafePassword, PasswordStrength } from './safe_password.js';
export { SafeHeader, HeaderName } from './safe_header.js';
export { SafeCookie, Cookie, CookiePrefix } from './safe_cookie.js';
export { SafeContentType, ContentType, MediaCategory, Charset } from './safe_content_type.js';

// Resilience modules
export { TokenBucket, SlidingWindowLimiter, FixedWindowLimiter, SafeRateLimiter } from './safe_rate_limiter.js';
export { CircuitBreaker, CircuitState, SafeCircuitBreaker } from './safe_circuit_breaker.js';
export { Retry, RetryConfig, RetryResult, RetryStrategy, SafeRetry } from './safe_retry.js';

// State modules
export { MonotonicCounter, MonotonicTimestamp, MonotonicId, MonotonicSequence, SafeMonotonic } from './safe_monotonic.js';
export { StateMachine, StateMachineBuilder, SafeStateMachine } from './safe_state_machine.js';

/**
 * Library version.
 * @type {string}
 */
export const VERSION = '0.9.0';

/**
 * All available modules.
 * @type {string[]}
 */
export const MODULES = [
  'result',
  'safe_math',
  'safe_string',
  'safe_path',
  'safe_email',
  'safe_url',
  'safe_network',
  'safe_crypto',
  'safe_uuid',
  'safe_currency',
  'safe_phone',
  'safe_hex',
  'safe_json',
  'safe_datetime',
  'safe_float',
  'safe_version',
  'safe_color',
  'safe_angle',
  'safe_unit',
  'safe_buffer',
  'safe_queue',
  'safe_bloom',
  'safe_lru',
  'safe_graph',
  'safe_geo',
  'safe_probability',
  'safe_checksum',
  'safe_tensor',
  'safe_ml',
  'safe_calculator',
  'safe_password',
  'safe_header',
  'safe_cookie',
  'safe_content_type',
  'safe_rate_limiter',
  'safe_circuit_breaker',
  'safe_retry',
  'safe_monotonic',
  'safe_state_machine',
];
