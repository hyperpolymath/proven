// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock
import kotlin.math.min

/**
 * Token bucket rate limiter.
 */
class TokenBucket(
    val capacity: Int,
    val refillRate: Double,
    val refillIntervalMs: Long = 1000
) {
    private val lock = ReentrantLock()
    private var tokens: Double = capacity.toDouble()
    private var lastRefill = System.currentTimeMillis()

    init {
        require(capacity > 0) { "Capacity must be positive" }
        require(refillRate > 0) { "Refill rate must be positive" }
    }

    /**
     * Try to acquire tokens.
     */
    fun tryAcquire(count: Int = 1): Boolean = lock.withLock {
        refill()
        if (tokens >= count) {
            tokens -= count
            true
        } else {
            false
        }
    }

    /**
     * Get current token count.
     */
    fun availableTokens(): Int = lock.withLock {
        refill()
        tokens.toInt()
    }

    /**
     * Time until next token available (ms).
     */
    fun timeUntilAvailable(count: Int = 1): Long = lock.withLock {
        refill()
        if (tokens >= count) 0
        else {
            val needed = count - tokens
            ((needed / refillRate) * refillIntervalMs).toLong()
        }
    }

    private fun refill() {
        val now = System.currentTimeMillis()
        val elapsed = now - lastRefill
        if (elapsed >= refillIntervalMs) {
            val intervals = elapsed / refillIntervalMs
            tokens = min(capacity.toDouble(), tokens + intervals * refillRate)
            lastRefill = now - (elapsed % refillIntervalMs)
        }
    }
}

/**
 * Sliding window rate limiter.
 */
class SlidingWindowLimiter(
    val limit: Int,
    val windowSizeMs: Long
) {
    private val lock = ReentrantLock()
    private val timestamps = ArrayDeque<Long>()

    init {
        require(limit > 0) { "Limit must be positive" }
        require(windowSizeMs > 0) { "Window size must be positive" }
    }

    /**
     * Try to record a request.
     */
    fun tryAcquire(): Boolean = lock.withLock {
        cleanup()
        if (timestamps.size < limit) {
            timestamps.addLast(System.currentTimeMillis())
            true
        } else {
            false
        }
    }

    /**
     * Get current request count in window.
     */
    fun currentCount(): Int = lock.withLock {
        cleanup()
        timestamps.size
    }

    /**
     * Get remaining capacity.
     */
    fun remaining(): Int = lock.withLock {
        cleanup()
        limit - timestamps.size
    }

    /**
     * Time until window resets (ms).
     */
    fun timeUntilReset(): Long = lock.withLock {
        cleanup()
        if (timestamps.isEmpty()) 0
        else {
            val oldest = timestamps.first()
            val resetTime = oldest + windowSizeMs
            maxOf(0, resetTime - System.currentTimeMillis())
        }
    }

    private fun cleanup() {
        val cutoff = System.currentTimeMillis() - windowSizeMs
        while (timestamps.isNotEmpty() && timestamps.first() < cutoff) {
            timestamps.removeFirst()
        }
    }
}

/**
 * Fixed window rate limiter.
 */
class FixedWindowLimiter(
    val limit: Int,
    val windowSizeMs: Long
) {
    private val lock = ReentrantLock()
    private var windowStart = System.currentTimeMillis()
    private var count = 0

    init {
        require(limit > 0) { "Limit must be positive" }
        require(windowSizeMs > 0) { "Window size must be positive" }
    }

    /**
     * Try to record a request.
     */
    fun tryAcquire(): Boolean = lock.withLock {
        checkWindow()
        if (count < limit) {
            count++
            true
        } else {
            false
        }
    }

    /**
     * Get current request count.
     */
    fun currentCount(): Int = lock.withLock {
        checkWindow()
        count
    }

    /**
     * Get remaining capacity.
     */
    fun remaining(): Int = lock.withLock {
        checkWindow()
        limit - count
    }

    /**
     * Time until window resets (ms).
     */
    fun timeUntilReset(): Long = lock.withLock {
        val nextWindow = windowStart + windowSizeMs
        maxOf(0, nextWindow - System.currentTimeMillis())
    }

    private fun checkWindow() {
        val now = System.currentTimeMillis()
        if (now >= windowStart + windowSizeMs) {
            windowStart = now - (now % windowSizeMs)
            count = 0
        }
    }
}

/**
 * Leaky bucket rate limiter.
 */
class LeakyBucket(
    val capacity: Int,
    val leakRatePerSecond: Double
) {
    private val lock = ReentrantLock()
    private var waterLevel: Double = 0.0
    private var lastLeak = System.currentTimeMillis()

    init {
        require(capacity > 0) { "Capacity must be positive" }
        require(leakRatePerSecond > 0) { "Leak rate must be positive" }
    }

    /**
     * Try to add water (request) to bucket.
     */
    fun tryAcquire(amount: Int = 1): Boolean = lock.withLock {
        leak()
        if (waterLevel + amount <= capacity) {
            waterLevel += amount
            true
        } else {
            false
        }
    }

    /**
     * Get current water level.
     */
    fun currentLevel(): Double = lock.withLock {
        leak()
        waterLevel
    }

    /**
     * Get available capacity.
     */
    fun available(): Double = lock.withLock {
        leak()
        capacity - waterLevel
    }

    private fun leak() {
        val now = System.currentTimeMillis()
        val elapsed = (now - lastLeak) / 1000.0
        waterLevel = maxOf(0.0, waterLevel - elapsed * leakRatePerSecond)
        lastLeak = now
    }
}

/**
 * Concurrency limiter.
 */
class ConcurrencyLimiter(val maxConcurrent: Int) {
    private val lock = ReentrantLock()
    private var active = 0

    init {
        require(maxConcurrent > 0) { "Max concurrent must be positive" }
    }

    /**
     * Try to acquire a slot.
     */
    fun tryAcquire(): Boolean = lock.withLock {
        if (active < maxConcurrent) {
            active++
            true
        } else {
            false
        }
    }

    /**
     * Release a slot.
     */
    fun release() = lock.withLock {
        if (active > 0) active--
    }

    /**
     * Get active count.
     */
    fun activeCount(): Int = lock.withLock { active }

    /**
     * Get available slots.
     */
    fun available(): Int = lock.withLock { maxConcurrent - active }

    /**
     * Execute with automatic release.
     */
    inline fun <T> withPermit(block: () -> T): T? {
        if (!tryAcquire()) return null
        return try {
            block()
        } finally {
            release()
        }
    }
}
