// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven - Code that cannot crash
 *
 * A verified safety library providing:
 * - SafeMath: Arithmetic without overflow/underflow/division-by-zero
 * - SafeString: UTF-8 and escaping without exceptions
 * - SafeUrl: URL parsing without malformed URL crashes
 * - SafeEmail: Validation without regex catastrophic backtracking
 * - SafePath: Filesystem ops without traversal attacks
 * - SafeCrypto: Cryptographic primitives done right
 * - SafeNetwork: Network operations that cannot fail unsafely
 * - SafeJson: JSON parsing with safe path access
 * - SafeDatetime: DateTime handling without invalid dates
 * - SafePassword: Password validation without timing attacks
 * - SafeHeader: HTTP header validation preventing injection
 * - SafeCookie: RFC 6265 compliant cookie handling
 * - SafeContentType: MIME type parsing and validation
 * - SafeFloat: Floating point without NaN/Infinity
 * - SafeTensor: Vector and matrix operations with bounds checking
 * - SafeML: Machine learning primitives with numerical stability
 * - SafeVersion: Semantic versioning parsing and comparison
 * - SafeGeo: Geographic coordinates and calculations
 * - SafeAngle: Angle conversions with proper wrapping
 * - SafeChecksum: Hash and checksum verification
 * - SafeProbability: Probability values clamped to [0,1]
 * - SafeUnit: Unit conversions without precision loss
 * - SafeColor: Color manipulation with WCAG compliance
 * - SafeBuffer: Bounded buffers and ring buffers
 * - SafeQueue: Bounded queues and priority queues
 * - SafeBloom: Bloom filters for probabilistic membership
 * - SafeLRU: LRU cache with TTL support
 * - SafeRateLimiter: Rate limiting algorithms
 * - SafeCircuitBreaker: Circuit breaker pattern
 * - SafeRetry: Retry with exponential backoff
 * - SafeMonotonic: Monotonic counters and timestamps
 * - SafeStateMachine: Finite state machines
 * - SafeGraph: Graph data structures and algorithms
 * - SafeCalculator: Safe arithmetic and expression evaluation
 */

// Core modules
export { SafeMath } from './safe_math.js';
export { SafeString } from './safe_string.js';
export { SafeUrl, type ParsedUrl } from './safe_url.js';
export { SafeEmail } from './safe_email.js';
export { SafePath } from './safe_path.js';
export { SafeCrypto } from './safe_crypto.js';
export { SafeNetwork, type IPv4 } from './safe_network.js';

// Data types
export { SafeJson } from './safe_json.js';
export { SafeDatetime, DateTime, Duration } from './safe_datetime.js';
export { SafeFloat } from './safe_float.js';
export { SafeVersion, SemVer } from './safe_version.js';

// Security
export { SafePassword } from './safe_password.js';
export { SafeHeader, HeaderValue, HeaderName, SafeHeaders } from './safe_header.js';
export { SafeCookie, Cookie } from './safe_cookie.js';
export { SafeContentType } from './safe_content_type.js';

// Scientific
export { SafeTensor, Vector, Matrix } from './safe_tensor.js';
export { SafeML } from './safe_ml.js';
export { SafeGeo, Coordinate } from './safe_geo.js';
export { SafeAngle, Degrees, Radians } from './safe_angle.js';
export { SafeProbability, Probability } from './safe_probability.js';
export { SafeUnit, Length, Mass, Temperature, TimeValue, DataSize } from './safe_unit.js';
export { SafeChecksum } from './safe_checksum.js';

// Visual
export { SafeColor, RGB, RGBA, HSL } from './safe_color.js';

// Data structures
export { SafeBuffer, BoundedBuffer, RingBuffer, GrowableBuffer } from './safe_buffer.js';
export { SafeQueue, BoundedQueue, PriorityQueue, BoundedDeque } from './safe_queue.js';
export { SafeBloom, BloomFilter, CountingBloomFilter } from './safe_bloom.js';
export { SafeLRU, LRUCache, TTLLRUCache } from './safe_lru.js';
export { SafeGraph, DirectedGraph, UndirectedGraph } from './safe_graph.js';

// Resilience patterns
export {
  SafeRateLimiter,
  TokenBucket,
  SlidingWindowLimiter,
  FixedWindowLimiter,
  LeakyBucket,
  ConcurrencyLimiter,
} from './safe_rate_limiter.js';
export {
  SafeCircuitBreaker,
  CircuitBreaker,
  CircuitBreakerGroup,
  CircuitState,
} from './safe_circuit_breaker.js';
export {
  SafeRetry,
  RetryBuilder,
  RetryPresets,
  RetryStrategy,
} from './safe_retry.js';

// Utilities
export {
  SafeMonotonic,
  MonotonicCounter,
  MonotonicTimestamp,
  MonotonicID,
  MonotonicSequence,
  MonotonicVersion,
} from './safe_monotonic.js';
export { SafeStateMachine, StateMachine, StateMachineBuilder } from './safe_state_machine.js';
export { SafeCalculatorModule, SafeCalculator, ExpressionParser } from './safe_calculator.js';

// Infrastructure
export { ProvenError, type ProvenStatus } from './error.js';
export { init, isInitialized } from './wasm.js';

export const VERSION = '0.4.0';
