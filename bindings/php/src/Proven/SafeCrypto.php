<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCrypto - FFI wrapper for proven_crypto_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe cryptographic operations via libproven FFI.
 *
 * Provides constant-time comparison and secure random byte generation.
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeCrypto
{
    /**
     * Constant-time byte string comparison (timing attack prevention).
     *
     * @return bool|null True if equal, false if not, null on error.
     */
    public static function constantTimeEq(string $a, string $b): ?bool
    {
        $result = FFI::getLib()->proven_crypto_constant_time_eq(
            $a, strlen($a), $b, strlen($b)
        );
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Fill a buffer with cryptographically secure random bytes.
     *
     * @param int $length Number of bytes to generate.
     * @return string|null Random bytes, or null on error.
     */
    public static function randomBytes(int $length): ?string
    {
        $buf = \FFI::new("char[$length]");
        $status = FFI::getLib()->proven_crypto_random_bytes(
            \FFI::addr($buf[0]), $length
        );
        if ($status !== 0) {
            return null;
        }
        return \FFI::string(\FFI::addr($buf[0]), $length);
    }
}
