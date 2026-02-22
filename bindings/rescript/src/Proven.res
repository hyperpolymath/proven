// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Proven - Code that cannot crash
 *
 * ReScript FFI bindings for verified safety functions.
 * All computation delegates to Idris 2 verified code via the JavaScript FFI
 * layer, which calls libproven (Idris 2 + Zig) via Deno.dlopen.
 *
 * IMPORTANT: This is an FFI-only binding layer.
 * Language bindings MUST be thin wrappers, not reimplementations.
 * See ADR-008 in META.scm.
 *
 * Modules:
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
 */

// ============================================================================
// Infrastructure
// ============================================================================

module FFI = Proven_FFI
module Error = ProvenError
module Result = ProvenResult
module Bitwise = Proven_Bitwise

// ============================================================================
// Core modules
// ============================================================================

module SafeMath = ProvenSafeMath
module SafeString = ProvenSafeString
module SafeUrl = ProvenSafeUrl
module SafeEmail = ProvenSafeEmail
module SafePath = ProvenSafePath
module SafeCrypto = ProvenSafeCrypto
module SafeNetwork = ProvenSafeNetwork

// ============================================================================
// Data types
// ============================================================================

module SafeJson = ProvenSafeJson
module SafeDatetime = ProvenSafeDatetime
module SafeFloat = ProvenSafeFloat
module SafeVersion = ProvenSafeVersion

// ============================================================================
// Security
// ============================================================================

module SafePassword = ProvenSafePassword
module SafeHeader = ProvenSafeHeader
module SafeCookie = ProvenSafeCookie
module SafeContentType = ProvenSafeContentType

// ============================================================================
// Scientific
// ============================================================================

module SafeTensor = ProvenSafeTensor
module SafeML = ProvenSafeMl
module SafeGeo = ProvenSafeGeo
module SafeAngle = ProvenSafeAngle
module SafeProbability = ProvenSafeProbability
module SafeUnit = ProvenSafeUnit
module SafeChecksum = ProvenSafeChecksum

// ============================================================================
// Visual
// ============================================================================

module SafeColor = ProvenSafeColor

// ============================================================================
// Data structures
// ============================================================================

module SafeBuffer = ProvenSafeBuffer
module SafeQueue = ProvenSafeQueue
module SafeBloom = ProvenSafeBloom
module SafeLRU = ProvenSafeLru
module SafeGraph = ProvenSafeGraph

// ============================================================================
// Resilience patterns
// ============================================================================

module SafeRateLimiter = ProvenSafeRateLimiter
module SafeCircuitBreaker = ProvenSafeCircuitBreaker
module SafeRetry = ProvenSafeRetry

// ============================================================================
// Utilities
// ============================================================================

module SafeMonotonic = ProvenSafeMonotonic
module SafeStateMachine = ProvenSafeStateMachine

// ============================================================================
// Public API
// ============================================================================

let version = "0.4.0"

/** Initialize the Proven library. */
let init = Proven_FFI.init

/** Check if initialized. */
let isInitialized = Proven_FFI.isInitialized

/** Cleanup. */
let deinit = Proven_FFI.deinit
