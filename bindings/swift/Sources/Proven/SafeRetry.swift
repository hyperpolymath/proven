// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Retry logic delegated to libproven FFI.
///
/// Exponential backoff delay calculation and retry eligibility
/// via the formally verified Idris 2 core.

import CProven

/// Retry configuration matching the C struct.
public struct RetryConfig: Sendable {
    public let maxAttempts: UInt32
    public let baseDelayMs: UInt64
    public let maxDelayMs: UInt64
    public let multiplier: Double

    public init(
        maxAttempts: UInt32 = 3,
        baseDelayMs: UInt64 = 1000,
        maxDelayMs: UInt64 = 30000,
        multiplier: Double = 2.0
    ) {
        self.maxAttempts = maxAttempts
        self.baseDelayMs = baseDelayMs
        self.maxDelayMs = maxDelayMs
        self.multiplier = multiplier
    }

    /// Convert to the C ProvenRetryConfig struct.
    internal var cConfig: ProvenRetryConfig {
        ProvenRetryConfig(
            max_attempts: maxAttempts,
            base_delay_ms: baseDelayMs,
            max_delay_ms: maxDelayMs,
            multiplier: multiplier
        )
    }
}

/// Retry utilities delegated to libproven.
public enum SafeRetry {
    /// Calculate the delay in milliseconds for a given attempt number.
    public static func delay(config: RetryConfig, attempt: UInt32) -> UInt64 {
        proven_retry_delay(config.cConfig, attempt)
    }

    /// Check if a retry should be attempted for the given attempt number.
    public static func shouldRetry(config: RetryConfig, attempt: UInt32) -> Bool {
        proven_retry_should_retry(config.cConfig, attempt)
    }
}
