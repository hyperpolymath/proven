// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlinx.coroutines.delay
import kotlin.math.min
import kotlin.math.pow
import kotlin.random.Random

/**
 * Retry strategy.
 */
enum class RetryStrategy {
    FIXED,
    LINEAR,
    EXPONENTIAL,
    EXPONENTIAL_WITH_JITTER
}

/**
 * Retry configuration.
 */
data class RetryConfig(
    val maxAttempts: Int = 3,
    val strategy: RetryStrategy = RetryStrategy.EXPONENTIAL_WITH_JITTER,
    val baseDelayMs: Long = 1000,
    val maxDelayMs: Long = 30000,
    val multiplier: Double = 2.0,
    val jitterFactor: Double = 0.1,
    val retryOn: (Throwable) -> Boolean = { true }
)

/**
 * Retry builder for fluent configuration.
 */
class RetryBuilder {
    private var maxAttempts = 3
    private var strategy = RetryStrategy.EXPONENTIAL_WITH_JITTER
    private var baseDelayMs = 1000L
    private var maxDelayMs = 30000L
    private var multiplier = 2.0
    private var jitterFactor = 0.1
    private var retryOn: (Throwable) -> Boolean = { true }

    fun maxAttempts(n: Int) = apply { maxAttempts = n }
    fun strategy(s: RetryStrategy) = apply { strategy = s }
    fun baseDelay(ms: Long) = apply { baseDelayMs = ms }
    fun maxDelay(ms: Long) = apply { maxDelayMs = ms }
    fun multiplier(m: Double) = apply { multiplier = m }
    fun jitter(factor: Double) = apply { jitterFactor = factor }
    fun retryOn(predicate: (Throwable) -> Boolean) = apply { retryOn = predicate }

    fun build(): RetryConfig = RetryConfig(
        maxAttempts, strategy, baseDelayMs, maxDelayMs, multiplier, jitterFactor, retryOn
    )
}

/**
 * Retry presets.
 */
object RetryPresets {
    val QUICK = RetryConfig(maxAttempts = 3, baseDelayMs = 100, maxDelayMs = 1000)
    val STANDARD = RetryConfig(maxAttempts = 5, baseDelayMs = 1000, maxDelayMs = 30000)
    val PATIENT = RetryConfig(maxAttempts = 10, baseDelayMs = 2000, maxDelayMs = 60000)
    val AGGRESSIVE = RetryConfig(maxAttempts = 20, baseDelayMs = 500, maxDelayMs = 10000)
    val NO_RETRY = RetryConfig(maxAttempts = 1)
}

/**
 * Retry utilities.
 */
object SafeRetry {
    /**
     * Calculate delay for attempt.
     */
    fun calculateDelay(config: RetryConfig, attempt: Int): Long {
        val baseDelay = when (config.strategy) {
            RetryStrategy.FIXED -> config.baseDelayMs
            RetryStrategy.LINEAR -> config.baseDelayMs * attempt
            RetryStrategy.EXPONENTIAL -> (config.baseDelayMs * config.multiplier.pow(attempt - 1)).toLong()
            RetryStrategy.EXPONENTIAL_WITH_JITTER -> {
                val exp = (config.baseDelayMs * config.multiplier.pow(attempt - 1)).toLong()
                val jitter = (exp * config.jitterFactor * Random.nextDouble()).toLong()
                exp + jitter
            }
        }
        return min(baseDelay, config.maxDelayMs)
    }

    /**
     * Retry a block synchronously.
     */
    inline fun <T> retry(
        config: RetryConfig = RetryPresets.STANDARD,
        block: (attempt: Int) -> T
    ): Result<T> {
        var lastException: Throwable? = null

        for (attempt in 1..config.maxAttempts) {
            try {
                return Result.success(block(attempt))
            } catch (e: Throwable) {
                lastException = e

                if (attempt >= config.maxAttempts || !config.retryOn(e)) {
                    break
                }

                val delayMs = calculateDelay(config, attempt)
                Thread.sleep(delayMs)
            }
        }

        return Result.failure(lastException ?: IllegalStateException("Retry failed"))
    }

    /**
     * Retry with specific exceptions.
     */
    inline fun <T, reified E : Throwable> retryOn(
        config: RetryConfig = RetryPresets.STANDARD,
        crossinline block: (attempt: Int) -> T
    ): Result<T> {
        val modifiedConfig = config.copy(retryOn = { it is E })
        return retry(modifiedConfig, block)
    }

    /**
     * Retry with callback on each failure.
     */
    inline fun <T> retryWithCallback(
        config: RetryConfig = RetryPresets.STANDARD,
        crossinline onRetry: (attempt: Int, exception: Throwable, delayMs: Long) -> Unit,
        block: (attempt: Int) -> T
    ): Result<T> {
        var lastException: Throwable? = null

        for (attempt in 1..config.maxAttempts) {
            try {
                return Result.success(block(attempt))
            } catch (e: Throwable) {
                lastException = e

                if (attempt >= config.maxAttempts || !config.retryOn(e)) {
                    break
                }

                val delayMs = calculateDelay(config, attempt)
                onRetry(attempt, e, delayMs)
                Thread.sleep(delayMs)
            }
        }

        return Result.failure(lastException ?: IllegalStateException("Retry failed"))
    }
}

/**
 * Suspending retry utilities.
 */
object SafeRetryAsync {
    /**
     * Retry a suspending block.
     */
    suspend inline fun <T> retry(
        config: RetryConfig = RetryPresets.STANDARD,
        crossinline block: suspend (attempt: Int) -> T
    ): Result<T> {
        var lastException: Throwable? = null

        for (attempt in 1..config.maxAttempts) {
            try {
                return Result.success(block(attempt))
            } catch (e: Throwable) {
                lastException = e

                if (attempt >= config.maxAttempts || !config.retryOn(e)) {
                    break
                }

                val delayMs = SafeRetry.calculateDelay(config, attempt)
                delay(delayMs)
            }
        }

        return Result.failure(lastException ?: IllegalStateException("Retry failed"))
    }
}
