<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeHex - FFI wrapper for proven_hex_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe hexadecimal encoding/decoding via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeHex
{
    /**
     * Encode raw bytes to hexadecimal string.
     *
     * @param string $data      Raw bytes to encode.
     * @param bool   $uppercase If true, use uppercase hex digits.
     * @return string|null The hex-encoded string, or null on error.
     */
    public static function encode(string $data, bool $uppercase = false): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_hex_encode($data, strlen($data), $uppercase)
        );
    }

    /**
     * Decode a hexadecimal string to raw bytes.
     *
     * @param string $hex The hex string to decode.
     * @return string|null The decoded bytes, or null on error.
     */
    public static function decode(string $hex): ?string
    {
        $result = FFI::getLib()->proven_hex_decode($hex, strlen($hex));
        if ($result->status !== 0 || $result->data === null) {
            return null;
        }
        $bytes = \FFI::string($result->data, $result->length);
        FFI::getLib()->proven_hex_free(\FFI::addr($result));
        return $bytes;
    }

    /**
     * Extract string from StringResult, free C memory, return null on error.
     */
    private static function extractString(mixed $result): ?string
    {
        if ($result->status !== 0 || $result->value === null) {
            return null;
        }
        $str = \FFI::string($result->value, $result->length);
        FFI::getLib()->proven_free_string($result->value);
        return $str;
    }
}
