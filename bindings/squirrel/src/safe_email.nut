// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeEmail - Email validation for Squirrel.
//
// All operations delegate to libproven via the native "proven" table.
// Returns null on error.

/**
 * SafeEmail - RFC 5321 email address validation.
 *
 * @example
 *   local email = SafeEmail();
 *   print(email.is_valid("user@example.com"));  // true
 *   print(email.is_valid("not-an-email"));       // false
 */
class SafeEmail {
    /**
     * Validate an email address (RFC 5321 simplified).
     * @param {string} email - Email address to validate.
     * @return {bool|null} true if valid, false if invalid, null on error.
     */
    function is_valid(email) {
        return proven.validate_email(email);
    }
}
