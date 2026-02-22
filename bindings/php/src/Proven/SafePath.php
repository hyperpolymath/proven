<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafePath - FFI wrapper for proven_path_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe filesystem path operations (traversal detection, filename sanitization).
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafePath
{
    /**
     * Check if a path contains directory traversal sequences (../, etc.).
     */
    public static function hasTraversal(string $path): ?bool
    {
        $result = FFI::getLib()->proven_path_has_traversal($path, strlen($path));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Sanitize a filename by removing dangerous characters.
     */
    public static function sanitizeFilename(string $filename): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_path_sanitize_filename($filename, strlen($filename))
        );
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
