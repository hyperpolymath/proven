// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Rate limiter delegated to libproven FFI.
///
/// Token bucket rate limiting via the formally verified Idris 2 core.

import CProven

/// Token bucket rate limiter backed by libproven.
/// This class manages the lifecycle of a C-allocated rate limiter.
public final class SafeRateLimiter {
    private let limiter: UnsafeMutablePointer<ProvenRateLimiter>

    /// Create a token bucket rate limiter.
    /// Returns nil if allocation fails.
    /// - Parameters:
    ///   - capacity: Maximum number of tokens.
    ///   - refillRate: Tokens added per second.
    public init?(capacity: Double, refillRate: Double) {
        guard let ptr = proven_rate_limiter_create(capacity, refillRate) else {
            return nil
        }
        self.limiter = ptr
    }

    deinit {
        proven_rate_limiter_free(limiter)
    }

    /// Try to acquire the specified number of tokens.
    /// Returns true if tokens were available, false if rate limited.
    @discardableResult
    public func tryAcquire(_ tokens: Double = 1.0) -> Bool {
        proven_rate_limiter_try_acquire(limiter, tokens)
    }
}
