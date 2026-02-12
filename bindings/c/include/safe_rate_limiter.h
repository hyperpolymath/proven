/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_rate_limiter.h
 * @brief Token bucket rate limiting
 *
 * Provides a token bucket rate limiter for controlling request rates
 * with automatic token refill over time.
 */

#ifndef SAFE_RATE_LIMITER_H
#define SAFE_RATE_LIMITER_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Rate Limiter Types
 * ============================================================================ */

/**
 * @brief Rate limiter state
 * @note Use proven_rate_limiter_create() to allocate, proven_rate_limiter_free() to deallocate
 */
typedef struct ProvenRateLimiter {
    double tokens;          /**< Current number of tokens */
    double capacity;        /**< Maximum token capacity */
    double refill_rate;     /**< Tokens per second refill rate */
    int64_t last_refill;    /**< Timestamp of last refill (milliseconds) */
} ProvenRateLimiter;

/* ============================================================================
 * Rate Limiter Operations
 * ============================================================================ */

/**
 * @brief Create a rate limiter
 * @param capacity Maximum token capacity
 * @param refill_rate Tokens per second to refill
 * @return Rate limiter pointer, or NULL on failure
 *
 * Returns NULL if capacity or refill_rate is <= 0.
 */
ProvenRateLimiter* proven_rate_limiter_create(double capacity, double refill_rate);

/**
 * @brief Try to acquire tokens
 * @param limiter Rate limiter
 * @param tokens Number of tokens to acquire
 * @return true if tokens were acquired, false if not enough tokens
 *
 * Automatically refills tokens based on elapsed time before checking.
 * Returns false if limiter is NULL or tokens <= 0.
 */
bool proven_rate_limiter_try_acquire(ProvenRateLimiter* limiter, double tokens);

/**
 * @brief Free rate limiter
 * @param limiter Rate limiter to free (may be NULL)
 */
void proven_rate_limiter_free(ProvenRateLimiter* limiter);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_RATE_LIMITER_H */
