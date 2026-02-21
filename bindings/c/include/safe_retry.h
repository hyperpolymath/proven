/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_retry.h
 * @brief Exponential backoff retry configuration
 *
 * Provides retry configuration with exponential backoff and jitter
 * for resilient retry strategies.
 */

#ifndef SAFE_RETRY_H
#define SAFE_RETRY_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Retry Types
 * ============================================================================ */

/**
 * @brief Retry configuration
 */
typedef struct ProvenRetryConfig {
    uint32_t max_attempts;      /**< Maximum number of retry attempts */
    uint64_t base_delay_ms;     /**< Base delay in milliseconds */
    uint64_t max_delay_ms;      /**< Maximum delay cap in milliseconds */
    double multiplier;          /**< Exponential multiplier (e.g., 2.0) */
} ProvenRetryConfig;

/* ============================================================================
 * Retry Operations
 * ============================================================================ */

/**
 * @brief Calculate delay for a given attempt
 * @param config Retry configuration
 * @param attempt Current attempt number (1-based)
 * @return Delay in milliseconds (with jitter applied)
 *
 * Returns 0 if attempt is 0 or exceeds max_attempts.
 * Applies exponential backoff: base_delay * multiplier^(attempt-1)
 * Adds random jitter of +/-25% to prevent thundering herd.
 */
uint64_t proven_retry_delay(ProvenRetryConfig config, uint32_t attempt);

/**
 * @brief Check if should retry
 * @param config Retry configuration
 * @param attempt Current attempt number
 * @return true if more attempts allowed, false otherwise
 */
bool proven_retry_should_retry(ProvenRetryConfig config, uint32_t attempt);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_RETRY_H */
