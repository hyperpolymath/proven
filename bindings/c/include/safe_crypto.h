/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_crypto.h
 * @brief Cryptographic primitives
 *
 * Provides timing-safe comparison and secure random byte generation.
 */

#ifndef SAFE_CRYPTO_H
#define SAFE_CRYPTO_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Timing-Safe Operations
 * ============================================================================ */

/**
 * @brief Constant-time byte comparison (timing-safe)
 * @param ptr1 First buffer
 * @param len1 Length of first buffer
 * @param ptr2 Second buffer
 * @param len2 Length of second buffer
 * @return Result with comparison status
 *
 * Use this for comparing secrets to prevent timing attacks.
 * Returns false if lengths differ.
 */
ProvenBoolResult proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2
);

/* ============================================================================
 * Random Generation
 * ============================================================================ */

/**
 * @brief Fill buffer with cryptographically secure random bytes
 * @param ptr Buffer to fill
 * @param len Number of bytes to generate
 * @return Status code
 *
 * Uses the system's cryptographically secure random number generator.
 */
ProvenStatus proven_crypto_random_bytes(uint8_t* ptr, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_CRYPTO_H */
