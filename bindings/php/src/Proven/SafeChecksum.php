<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeChecksum - FFI wrapper for proven_checksum_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe checksum operations via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeChecksum
{
    /**
     * Compute CRC32 checksum of data.
     *
     * @return int|null The CRC32 value, or null on error.
     */
    public static function crc32(string $data): ?int
    {
        $result = FFI::getLib()->proven_checksum_crc32($data, strlen($data));
        if ($result->status !== 0) {
            return null;
        }
        return (int) $result->value;
    }

    /**
     * Verify that data matches an expected CRC32 checksum.
     *
     * @return bool|null True if matches, false if not, null on error.
     */
    public static function verifyCrc32(string $data, int $expected): ?bool
    {
        $result = FFI::getLib()->proven_checksum_verify_crc32(
            $data, strlen($data), $expected
        );
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }
}
