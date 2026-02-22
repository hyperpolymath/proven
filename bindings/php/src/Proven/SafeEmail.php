<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeEmail - FFI wrapper for proven_email_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe email validation via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeEmail
{
    /**
     * Check if an email address is valid (RFC 5321).
     */
    public static function isValid(string $email): ?bool
    {
        $result = FFI::getLib()->proven_email_is_valid($email, strlen($email));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }
}
