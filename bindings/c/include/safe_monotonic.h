/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_monotonic.h
 * @brief Monotonically increasing sequences
 *
 * Provides a monotonic counter that only increases, useful for
 * sequence numbers, version counters, and ordering guarantees.
 */

#ifndef SAFE_MONOTONIC_H
#define SAFE_MONOTONIC_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Monotonic Counter Types
 * ============================================================================ */

/**
 * @brief Monotonic counter structure
 * @note Use proven_monotonic_create() to allocate, proven_monotonic_free() to deallocate
 */
typedef struct ProvenMonotonicCounter {
    uint64_t value;         /**< Current counter value */
    uint64_t max_value;     /**< Maximum allowed value */
} ProvenMonotonicCounter;

/* ============================================================================
 * Monotonic Counter Operations
 * ============================================================================ */

/**
 * @brief Create a monotonic counter
 * @param initial Initial value
 * @param max_value Maximum value (counter fails when reached)
 * @return Counter pointer, or NULL on failure
 *
 * Returns NULL if initial >= max_value.
 */
ProvenMonotonicCounter* proven_monotonic_create(uint64_t initial, uint64_t max_value);

/**
 * @brief Get next value and increment
 * @param counter Monotonic counter
 * @return Result with status and next value
 *
 * Returns PROVEN_ERR_NULL_POINTER if counter is NULL.
 * Returns PROVEN_ERR_OVERFLOW if counter has reached max_value.
 */
ProvenIntResult proven_monotonic_next(ProvenMonotonicCounter* counter);

/**
 * @brief Free monotonic counter
 * @param counter Counter to free (may be NULL)
 */
void proven_monotonic_free(ProvenMonotonicCounter* counter);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_MONOTONIC_H */
