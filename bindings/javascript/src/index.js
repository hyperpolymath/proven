// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Proven - Code that cannot crash
 *
 * A verified safety library providing safe wrappers for common operations.
 * All operations return Result types and prevent undefined behavior.
 *
 * JavaScript bindings for the proven library.
 * @module
 */

// Result utilities
export { ok, err, isOk, isErr, unwrap, unwrapOr } from './result.js';

// Core modules
export { SafeUUID, UuidVersion, UuidVariant } from './safe_uuid.js';
export { Money, CurrencyCode } from './safe_currency.js';
export { PhoneNumber, CountryCode, PhoneNumberType } from './safe_phone.js';
export { SafeHex } from './safe_hex.js';
export { SafeMath } from './safe_math.js';
export { SafeString } from './safe_string.js';
export { SafeJson, JsonValue } from './safe_json.js';
export { SafeUrl, Url } from './safe_url.js';
export { SafeEmail, Email } from './safe_email.js';
export { SafePath } from './safe_path.js';

// Network modules
export { SafeNetwork, Ipv4Address, Cidr } from './safe_network.js';

// DateTime modules
export { SafeDateTime, DateTime } from './safe_datetime.js';

// Security modules
export { SafePassword, PasswordStrength } from './safe_password.js';
export { SafeHeader, HeaderName } from './safe_header.js';
export { SafeCookie, Cookie } from './safe_cookie.js';
export { SafeContentType, ContentType } from './safe_content_type.js';

// Numerical modules
export { SafeFloat } from './safe_float.js';
export { Vector, Matrix } from './safe_tensor.js';
export { SafeML } from './safe_ml.js';
export { SafeCalculator, Expression } from './safe_calculator.js';

// Extended modules
export { SafeVersion, SemVer, VersionConstraint } from './safe_version.js';
export { SafeGeo, Coordinate, Distance } from './safe_geo.js';
export { Degrees, Radians, SafeAngle } from './safe_angle.js';
export { SafeChecksum, Crc32, Adler32, Fnv, Luhn } from './safe_checksum.js';
export { Probability, SafeProbability } from './safe_probability.js';
export { SafeUnit, LengthUnit, MassUnit, TemperatureUnit, TimeUnit, DataUnit } from './safe_unit.js';

// Color modules
export { Rgb, Rgba, SafeColor } from './safe_color.js';

// Data structure modules
export { BoundedBuffer, RingBuffer } from './safe_buffer.js';
export { BoundedQueue, PriorityQueue } from './safe_queue.js';
export { BloomFilter } from './safe_bloom.js';
export { LruCache } from './safe_lru.js';

// Resilience modules
export { TokenBucket, SlidingWindowLimiter, FixedWindowLimiter, SafeRateLimiter } from './safe_rate_limiter.js';
export { CircuitBreaker, CircuitState, SafeCircuitBreaker } from './safe_circuit_breaker.js';
export { Retry, RetryConfig, RetryResult, RetryStrategy, SafeRetry } from './safe_retry.js';

// State modules
export { MonotonicCounter, MonotonicTimestamp, MonotonicId, MonotonicSequence, SafeMonotonic } from './safe_monotonic.js';
export { StateMachine, StateMachineBuilder, SafeStateMachine } from './safe_state_machine.js';

// Graph modules
export { DirectedGraph, Edge, SafeGraph } from './safe_graph.js';

// Crypto modules
export { Base64, Hex, SecureRandom, ConstantTime, Hash, Hmac, SafeCrypto } from './safe_crypto.js';

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
  'safe_uuid',
  'safe_currency',
  'safe_phone',
  'safe_hex',
  'safe_math',
  'safe_string',
  'safe_json',
  'safe_url',
  'safe_email',
  'safe_path',
  'safe_network',
  'safe_datetime',
  'safe_password',
  'safe_header',
  'safe_cookie',
  'safe_content_type',
  'safe_float',
  'safe_tensor',
  'safe_ml',
  'safe_calculator',
  'safe_version',
  'safe_geo',
  'safe_angle',
  'safe_checksum',
  'safe_probability',
  'safe_unit',
  'safe_color',
  'safe_buffer',
  'safe_queue',
  'safe_bloom',
  'safe_lru',
  'safe_rate_limiter',
  'safe_circuit_breaker',
  'safe_retry',
  'safe_monotonic',
  'safe_state_machine',
  'safe_graph',
  'safe_crypto',
];
