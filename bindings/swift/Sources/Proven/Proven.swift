// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Proven - Safety-first utility functions with formal verification guarantees.
///
/// This library provides 38 modules of safe operations:
///
/// ## Core Types
/// - SafeMath: Division, modulo, checked arithmetic with overflow detection
/// - SafeFloat: IEEE 754 safe floating-point preventing NaN/Infinity
/// - SafeString: HTML/SQL/JS/URL escaping, safe truncation
/// - SafeJson: JSON parsing, path access, safe serialization
/// - SafeDatetime: Date/time operations with timezone safety
///
/// ## Data Types
/// - SafeVersion: Semantic versioning (SemVer) parsing and comparison
/// - SafeColor: RGB, RGBA, HSL color types with WCAG contrast checking
/// - SafeAngle: Degrees and radians with conversions and normalization
/// - SafeUnit: Length, mass, temperature, time, data unit conversions
/// - SafeProbability: Probability operations, Bayes theorem, distributions
///
/// ## Security
/// - SafeCrypto: Constant-time comparison, secure zeroing
/// - SafePassword: Password strength validation and generation
/// - SafeHex: Hexadecimal encoding/decoding with constant-time comparison
/// - SafeChecksum: CRC-32, Adler-32, FNV, Luhn validation
///
/// ## Network
/// - SafeNetwork: IPv4 parsing, private/loopback/public classification
/// - SafeUrl: URL parsing with full component extraction
/// - SafeEmail: Validation, parsing, normalization
/// - SafePhone: E.164 phone number parsing and formatting
/// - SafeHeader: HTTP header validation and common headers
/// - SafeCookie: HTTP cookie parsing, formatting, security attributes
/// - SafeContentType: MIME type parsing and file extension mapping
///
/// ## Data Structures
/// - SafeBuffer: BoundedBuffer, RingBuffer, GrowableBuffer
/// - SafeQueue: BoundedQueue, PriorityQueue, BoundedDeque
/// - SafeLRU: LRU cache with optional TTL support
/// - SafeBloom: Bloom filter and counting Bloom filter
/// - SafeGraph: Directed and undirected graphs with BFS, DFS, topological sort
/// - SafeTensor: Multi-dimensional array operations
///
/// ## Resilience Patterns
/// - SafeRateLimiter: Token bucket, sliding window, fixed window, leaky bucket
/// - SafeCircuitBreaker: Circuit breaker pattern for fault tolerance
/// - SafeRetry: Retry with exponential backoff and jitter
///
/// ## Utilities
/// - SafePath: Traversal detection and filename sanitization
/// - SafeUUID: RFC 4122 UUID parsing, formatting, and generation
/// - SafeCurrency: Type-safe monetary values with ISO 4217 codes
/// - SafeMonotonic: Monotonically increasing counters, timestamps, IDs
///
/// ## Scientific
/// - SafeCalculator: Safe arithmetic operations and expression parsing
/// - SafeGeo: Geographic calculations (haversine, bearing, bounding box)
/// - SafeML: Machine learning utilities (activations, normalization, metrics)
///
/// ## Infrastructure
/// - SafeStateMachine: Type-safe finite state machine
///
/// All modules use Swift's Result type or optionals for error handling,
/// ensuring compile-time safety and preventing runtime crashes.

import Foundation

/// Library version.
public let PROVEN_VERSION = "0.4.0"

/// Library module count.
public let PROVEN_MODULE_COUNT = 38
