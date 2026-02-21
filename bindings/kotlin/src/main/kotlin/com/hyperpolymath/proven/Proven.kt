// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Proven - Safety-first utility functions with formal verification guarantees.
 *
 * This library provides 38 modules of safe operations:
 *
 * ## Core Modules (11)
 * - SafeMath: Safe arithmetic with overflow protection
 * - SafeString: String manipulation with bounds checking
 * - SafePath: Path manipulation without traversal attacks
 * - SafeEmail: Email validation and parsing
 * - SafeUrl: URL parsing and validation
 * - SafeNetwork: IP address and network utilities
 * - SafeCrypto: Cryptographic utilities
 * - SafeUuid: UUID generation and validation
 * - SafeCurrency: Currency handling with precision
 * - SafePhone: Phone number parsing and validation
 * - SafeHex: Hexadecimal encoding/decoding
 *
 * ## Data Modules (7)
 * - SafeJson: JSON parsing and validation
 * - SafeDatetime: Date and time handling
 * - SafeFloat: Float operations preventing NaN/Infinity
 * - SafeVersion: Semantic versioning
 * - SafeColor: Color manipulation with WCAG contrast
 * - SafeAngle: Angle conversions and trigonometry
 * - SafeUnit: Unit conversions
 *
 * ## Data Structure Modules (5)
 * - SafeBuffer: Bounded and ring buffers
 * - SafeQueue: Priority queues and deques
 * - SafeBloom: Bloom filters
 * - SafeLRU: LRU caches with TTL
 * - SafeGraph: Graph data structures and algorithms
 *
 * ## Resilience Modules (4)
 * - SafeRateLimiter: Rate limiting algorithms
 * - SafeCircuitBreaker: Circuit breaker pattern
 * - SafeRetry: Retry strategies with backoff
 * - SafeMonotonic: Monotonic counters and timestamps
 *
 * ## State Modules (2)
 * - SafeStateMachine: State machine builder
 * - SafeCalculator: Expression parser and calculator
 *
 * ## Algorithm Modules (4)
 * - SafeGeo: Geographic calculations
 * - SafeProbability: Probability distributions
 * - SafeChecksum: Checksum algorithms
 * - SafeTensor: Tensor operations
 *
 * ## Security Modules (2)
 * - SafePassword: Password validation and generation
 * - SafeML: Machine learning utilities
 *
 * ## HTTP Modules (3)
 * - SafeHeader: HTTP header validation
 * - SafeCookie: HTTP cookie handling
 * - SafeContentType: MIME type handling
 *
 * @see <a href="https://github.com/hyperpolymath/proven">GitHub Repository</a>
 */
object Proven {
    /**
     * Library version.
     */
    const val VERSION = "0.4.0"

    /**
     * Total number of modules.
     */
    const val MODULE_COUNT = 38

    /**
     * All module names.
     */
    val MODULES = listOf(
        // Core Modules
        "SafeMath",
        "SafeString",
        "SafePath",
        "SafeEmail",
        "SafeUrl",
        "SafeNetwork",
        "SafeCrypto",
        "SafeUuid",
        "SafeCurrency",
        "SafePhone",
        "SafeHex",

        // Data Modules
        "SafeJson",
        "SafeDatetime",
        "SafeFloat",
        "SafeVersion",
        "SafeColor",
        "SafeAngle",
        "SafeUnit",

        // Data Structure Modules
        "SafeBuffer",
        "SafeQueue",
        "SafeBloom",
        "SafeLRU",
        "SafeGraph",

        // Resilience Modules
        "SafeRateLimiter",
        "SafeCircuitBreaker",
        "SafeRetry",
        "SafeMonotonic",

        // State Modules
        "SafeStateMachine",
        "SafeCalculator",

        // Algorithm Modules
        "SafeGeo",
        "SafeProbability",
        "SafeChecksum",
        "SafeTensor",

        // Security Modules
        "SafePassword",
        "SafeML",

        // HTTP Modules
        "SafeHeader",
        "SafeCookie",
        "SafeContentType"
    )

    /**
     * Core safety principles this library adheres to:
     *
     * 1. No runtime exceptions from valid inputs
     * 2. Explicit error handling via Result types or nullable returns
     * 3. Bounds checking on all array/collection access
     * 4. No integer overflow (checked arithmetic)
     * 5. No null pointer exceptions
     * 6. Thread-safe where applicable
     * 7. Input validation at API boundaries
     */
    val SAFETY_PRINCIPLES = listOf(
        "No runtime exceptions from valid inputs",
        "Explicit error handling via Result types or nullable returns",
        "Bounds checking on all array/collection access",
        "No integer overflow (checked arithmetic)",
        "No null pointer exceptions",
        "Thread-safe where applicable",
        "Input validation at API boundaries"
    )

    /**
     * Get version information.
     */
    fun versionInfo(): String = "Proven v$VERSION ($MODULE_COUNT modules)"
}
