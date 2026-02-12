/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_circuit_breaker.h
 * @brief Fault tolerance circuit breaker pattern
 *
 * Provides a circuit breaker for fault tolerance, automatically
 * opening when failures exceed a threshold and testing recovery.
 */

#ifndef SAFE_CIRCUIT_BREAKER_H
#define SAFE_CIRCUIT_BREAKER_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Circuit Breaker Types
 * ============================================================================ */

/**
 * @brief Circuit breaker state
 */
typedef enum ProvenCircuitState {
    PROVEN_CIRCUIT_CLOSED = 0,     /**< Normal operation, requests allowed */
    PROVEN_CIRCUIT_OPEN = 1,       /**< Failing, requests rejected */
    PROVEN_CIRCUIT_HALF_OPEN = 2   /**< Testing recovery */
} ProvenCircuitState;

/**
 * @brief Circuit breaker structure
 * @note Use proven_circuit_breaker_create() to allocate, proven_circuit_breaker_free() to deallocate
 */
typedef struct ProvenCircuitBreaker {
    ProvenCircuitState state;
    uint32_t failure_count;
    uint32_t failure_threshold;
    uint32_t success_count;
    uint32_t success_threshold;
    int64_t last_failure;       /**< Timestamp of last failure (milliseconds) */
    int64_t timeout_ms;         /**< Time before attempting recovery */
} ProvenCircuitBreaker;

/* ============================================================================
 * Circuit Breaker Operations
 * ============================================================================ */

/**
 * @brief Create a circuit breaker
 * @param failure_threshold Number of failures before opening circuit
 * @param success_threshold Number of successes in half-open state to close circuit
 * @param timeout_ms Time in milliseconds before trying half-open recovery
 * @return Circuit breaker pointer, or NULL on failure
 *
 * Returns NULL if any threshold is 0 or timeout_ms <= 0.
 */
ProvenCircuitBreaker* proven_circuit_breaker_create(
    uint32_t failure_threshold,
    uint32_t success_threshold,
    int64_t timeout_ms
);

/**
 * @brief Check if a request should be allowed
 * @param cb Circuit breaker
 * @return true if request is allowed, false if circuit is open
 *
 * Returns false if cb is NULL.
 * When circuit is open and timeout has elapsed, transitions to half-open.
 */
bool proven_circuit_breaker_allow(ProvenCircuitBreaker* cb);

/**
 * @brief Record a successful operation
 * @param cb Circuit breaker
 *
 * In half-open state, increments success count. If threshold reached,
 * transitions to closed state.
 */
void proven_circuit_breaker_success(ProvenCircuitBreaker* cb);

/**
 * @brief Record a failed operation
 * @param cb Circuit breaker
 *
 * Increments failure count. If threshold reached or in half-open state,
 * transitions to open state.
 */
void proven_circuit_breaker_failure(ProvenCircuitBreaker* cb);

/**
 * @brief Get current circuit state
 * @param cb Circuit breaker
 * @return Current state (returns OPEN if cb is NULL)
 */
ProvenCircuitState proven_circuit_breaker_state(ProvenCircuitBreaker* cb);

/**
 * @brief Free circuit breaker
 * @param cb Circuit breaker to free (may be NULL)
 */
void proven_circuit_breaker_free(ProvenCircuitBreaker* cb);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_CIRCUIT_BREAKER_H */
