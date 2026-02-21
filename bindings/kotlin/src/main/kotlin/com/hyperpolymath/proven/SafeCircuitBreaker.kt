// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * Circuit breaker state.
 */
enum class CircuitState {
    CLOSED,     // Normal operation
    OPEN,       // Failing, rejecting calls
    HALF_OPEN   // Testing if service recovered
}

/**
 * Circuit breaker error.
 */
class CircuitOpenException(message: String) : Exception(message)

/**
 * Circuit breaker for fault tolerance.
 */
class CircuitBreaker(
    val name: String,
    val failureThreshold: Int = 5,
    val resetTimeoutMs: Long = 30000,
    val halfOpenMaxCalls: Int = 3
) {
    private val lock = ReentrantLock()
    private var state = CircuitState.CLOSED
    private var failureCount = 0
    private var successCount = 0
    private var lastFailureTime = 0L
    private var halfOpenCalls = 0

    /**
     * Get current state.
     */
    fun getState(): CircuitState = lock.withLock {
        updateState()
        state
    }

    /**
     * Check if circuit allows calls.
     */
    fun allowRequest(): Boolean = lock.withLock {
        updateState()
        when (state) {
            CircuitState.CLOSED -> true
            CircuitState.OPEN -> false
            CircuitState.HALF_OPEN -> halfOpenCalls < halfOpenMaxCalls
        }
    }

    /**
     * Record a successful call.
     */
    fun recordSuccess() = lock.withLock {
        successCount++
        when (state) {
            CircuitState.HALF_OPEN -> {
                halfOpenCalls++
                if (successCount >= halfOpenMaxCalls) {
                    reset()
                }
            }
            CircuitState.CLOSED -> {
                failureCount = 0
            }
            else -> {}
        }
    }

    /**
     * Record a failed call.
     */
    fun recordFailure() = lock.withLock {
        failureCount++
        lastFailureTime = System.currentTimeMillis()

        when (state) {
            CircuitState.HALF_OPEN -> {
                // Any failure in half-open trips back to open
                state = CircuitState.OPEN
                halfOpenCalls = 0
            }
            CircuitState.CLOSED -> {
                if (failureCount >= failureThreshold) {
                    state = CircuitState.OPEN
                }
            }
            else -> {}
        }
    }

    /**
     * Reset the circuit breaker.
     */
    fun reset() = lock.withLock {
        state = CircuitState.CLOSED
        failureCount = 0
        successCount = 0
        halfOpenCalls = 0
    }

    /**
     * Force circuit open.
     */
    fun forceOpen() = lock.withLock {
        state = CircuitState.OPEN
        lastFailureTime = System.currentTimeMillis()
    }

    /**
     * Get failure count.
     */
    fun getFailureCount(): Int = lock.withLock { failureCount }

    /**
     * Get success count.
     */
    fun getSuccessCount(): Int = lock.withLock { successCount }

    /**
     * Execute with circuit breaker protection.
     */
    inline fun <T> execute(block: () -> T): Result<T> {
        if (!allowRequest()) {
            return Result.failure(CircuitOpenException("Circuit breaker '$name' is open"))
        }

        return try {
            val result = block()
            recordSuccess()
            Result.success(result)
        } catch (e: Exception) {
            recordFailure()
            Result.failure(e)
        }
    }

    private fun updateState() {
        if (state == CircuitState.OPEN) {
            val elapsed = System.currentTimeMillis() - lastFailureTime
            if (elapsed >= resetTimeoutMs) {
                state = CircuitState.HALF_OPEN
                halfOpenCalls = 0
                successCount = 0
            }
        }
    }
}

/**
 * Group of circuit breakers.
 */
class CircuitBreakerGroup {
    private val lock = ReentrantLock()
    private val breakers = mutableMapOf<String, CircuitBreaker>()

    /**
     * Get or create a circuit breaker.
     */
    fun get(
        name: String,
        failureThreshold: Int = 5,
        resetTimeoutMs: Long = 30000,
        halfOpenMaxCalls: Int = 3
    ): CircuitBreaker = lock.withLock {
        breakers.getOrPut(name) {
            CircuitBreaker(name, failureThreshold, resetTimeoutMs, halfOpenMaxCalls)
        }
    }

    /**
     * Get all breaker statuses.
     */
    fun getStatuses(): Map<String, CircuitState> = lock.withLock {
        breakers.mapValues { it.value.getState() }
    }

    /**
     * Reset all breakers.
     */
    fun resetAll() = lock.withLock {
        breakers.values.forEach { it.reset() }
    }

    /**
     * Remove a breaker.
     */
    fun remove(name: String) = lock.withLock {
        breakers.remove(name)
    }
}
