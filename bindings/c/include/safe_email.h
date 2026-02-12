/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_email.h
 * @brief Email address validation
 *
 * Provides RFC 5321 compliant email address validation.
 */

#ifndef SAFE_EMAIL_H
#define SAFE_EMAIL_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Email Validation
 * ============================================================================ */

/**
 * @brief Validate email address (RFC 5321 simplified)
 * @param ptr Pointer to email string
 * @param len Length of email
 * @return Result with validation status
 *
 * Validates basic email format with local part and domain.
 * Does not verify that the email address actually exists.
 */
ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_EMAIL_H */
