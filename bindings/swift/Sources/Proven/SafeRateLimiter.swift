// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Rate limit result.
public struct RateLimitResult {
    public let allowed: Bool
    public let remaining: Int
    public let resetAt: Double
    public let retryAfter: Double?
}

/// Token bucket rate limiter.
public class TokenBucket {
    public let capacity: Double
    public let refillRate: Double // tokens per second
    private var tokens: Double
    private var lastRefill: Double
    private let getNow: () -> Double

    /// Create a token bucket.
    /// - Parameters:
    ///   - capacity: Maximum tokens
    ///   - refillRate: Tokens added per second
    ///   - getNow: Function to get current time in milliseconds
    public init(capacity: Double, refillRate: Double, getNow: @escaping () -> Double = { Date().timeIntervalSince1970 * 1000 }) {
        self.capacity = max(1, capacity)
        self.refillRate = max(0.001, refillRate)
        self.tokens = self.capacity
        self.getNow = getNow
        self.lastRefill = getNow()
    }

    private func refill() {
        let now = getNow()
        let elapsed = (now - lastRefill) / 1000
        let tokensToAdd = elapsed * refillRate
        tokens = min(capacity, tokens + tokensToAdd)
        lastRefill = now
    }

    /// Try to consume tokens.
    public func tryConsume(_ count: Double = 1) -> RateLimitResult {
        refill()

        if count <= tokens {
            tokens -= count
            return RateLimitResult(
                allowed: true,
                remaining: Int(tokens),
                resetAt: lastRefill + ((capacity - tokens) / refillRate) * 1000,
                retryAfter: nil
            )
        }

        let tokensNeeded = count - tokens
        let retryAfter = (tokensNeeded / refillRate) * 1000

        return RateLimitResult(
            allowed: false,
            remaining: Int(tokens),
            resetAt: lastRefill + (capacity / refillRate) * 1000,
            retryAfter: retryAfter
        )
    }

    /// Get current token count.
    public var available: Int {
        refill()
        return Int(tokens)
    }

    /// Reset bucket to full capacity.
    public func reset() {
        tokens = capacity
        lastRefill = getNow()
    }
}

/// Sliding window rate limiter.
public class SlidingWindowLimiter {
    public let maxRequests: Int
    public let windowMs: Double
    private var timestamps: [Double] = []
    private let getNow: () -> Double

    public init(maxRequests: Int, windowMs: Double, getNow: @escaping () -> Double = { Date().timeIntervalSince1970 * 1000 }) {
        self.maxRequests = max(1, maxRequests)
        self.windowMs = max(1, windowMs)
        self.getNow = getNow
    }

    private func cleanup() {
        let cutoff = getNow() - windowMs
        timestamps.removeAll { $0 < cutoff }
    }

    /// Try to acquire a slot.
    public func tryAcquire() -> RateLimitResult {
        cleanup()
        let now = getNow()

        if timestamps.count < maxRequests {
            timestamps.append(now)
            return RateLimitResult(
                allowed: true,
                remaining: maxRequests - timestamps.count,
                resetAt: now + windowMs,
                retryAfter: nil
            )
        }

        let oldestTimestamp = timestamps.first!
        let retryAfter = oldestTimestamp + windowMs - now

        return RateLimitResult(
            allowed: false,
            remaining: 0,
            resetAt: oldestTimestamp + windowMs,
            retryAfter: retryAfter
        )
    }

    public var count: Int {
        cleanup()
        return timestamps.count
    }

    public func reset() {
        timestamps.removeAll()
    }
}

/// Fixed window rate limiter.
public class FixedWindowLimiter {
    public let maxRequests: Int
    public let windowMs: Double
    private var windowStart: Double
    private var requestCount: Int = 0
    private let getNow: () -> Double

    public init(maxRequests: Int, windowMs: Double, getNow: @escaping () -> Double = { Date().timeIntervalSince1970 * 1000 }) {
        self.maxRequests = max(1, maxRequests)
        self.windowMs = max(1, windowMs)
        self.getNow = getNow
        self.windowStart = getNow()
    }

    private func maybeResetWindow() {
        let now = getNow()
        if now >= windowStart + windowMs {
            windowStart = now
            requestCount = 0
        }
    }

    /// Try to acquire a slot.
    public func tryAcquire() -> RateLimitResult {
        maybeResetWindow()
        let now = getNow()
        let windowEnd = windowStart + windowMs

        if requestCount < maxRequests {
            requestCount += 1
            return RateLimitResult(
                allowed: true,
                remaining: maxRequests - requestCount,
                resetAt: windowEnd,
                retryAfter: nil
            )
        }

        return RateLimitResult(
            allowed: false,
            remaining: 0,
            resetAt: windowEnd,
            retryAfter: windowEnd - now
        )
    }

    public var count: Int {
        maybeResetWindow()
        return requestCount
    }

    public func reset() {
        windowStart = getNow()
        requestCount = 0
    }
}

/// Leaky bucket rate limiter.
public class LeakyBucket {
    public let capacity: Double
    public let leakRate: Double // requests per second
    private var waterLevel: Double = 0
    private var lastLeak: Double
    private let getNow: () -> Double

    public init(capacity: Double, leakRate: Double, getNow: @escaping () -> Double = { Date().timeIntervalSince1970 * 1000 }) {
        self.capacity = max(1, capacity)
        self.leakRate = max(0.001, leakRate)
        self.getNow = getNow
        self.lastLeak = getNow()
    }

    private func leak() {
        let now = getNow()
        let elapsed = (now - lastLeak) / 1000
        let leaked = elapsed * leakRate
        waterLevel = max(0, waterLevel - leaked)
        lastLeak = now
    }

    /// Try to add a request.
    public func tryAdd(_ amount: Double = 1) -> RateLimitResult {
        leak()

        if waterLevel + amount <= capacity {
            waterLevel += amount
            return RateLimitResult(
                allowed: true,
                remaining: Int(capacity - waterLevel),
                resetAt: lastLeak + (waterLevel / leakRate) * 1000,
                retryAfter: nil
            )
        }

        let overflow = waterLevel + amount - capacity
        let retryAfter = (overflow / leakRate) * 1000

        return RateLimitResult(
            allowed: false,
            remaining: Int(capacity - waterLevel),
            resetAt: lastLeak + (capacity / leakRate) * 1000,
            retryAfter: retryAfter
        )
    }

    public var level: Double {
        leak()
        return waterLevel
    }

    public func reset() {
        waterLevel = 0
        lastLeak = getNow()
    }
}

/// Concurrency limiter.
public class ConcurrencyLimiter {
    public let maxConcurrent: Int
    private var current: Int = 0
    private let lock = NSLock()

    public init(maxConcurrent: Int) {
        self.maxConcurrent = max(1, maxConcurrent)
    }

    /// Try to acquire a slot.
    public func tryAcquire() -> Bool {
        lock.lock()
        defer { lock.unlock() }

        if current < maxConcurrent {
            current += 1
            return true
        }
        return false
    }

    /// Release a slot.
    public func release() {
        lock.lock()
        defer { lock.unlock() }

        if current > 0 {
            current -= 1
        }
    }

    public var count: Int {
        lock.lock()
        defer { lock.unlock() }
        return current
    }

    public var available: Int {
        lock.lock()
        defer { lock.unlock() }
        return maxConcurrent - current
    }
}
