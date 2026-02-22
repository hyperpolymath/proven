<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeVersion - FFI wrapper for proven_version_parse / proven_version_compare.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe semantic version parsing and comparison via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeVersion
{
    /**
     * Parse a semver string (e.g. "1.2.3-alpha").
     *
     * Returns an associative array with major, minor, patch, prerelease
     * on success, or null on error.
     *
     * @return array{major: int, minor: int, patch: int, prerelease: string}|null
     */
    public static function parse(string $version): ?array
    {
        $result = FFI::getLib()->proven_version_parse($version, strlen($version));
        if ($result->status !== 0) {
            return null;
        }
        $v = $result->version;
        $prerelease = '';
        if ($v->prerelease !== null && $v->prerelease_len > 0) {
            $prerelease = \FFI::string($v->prerelease, $v->prerelease_len);
        }
        $data = [
            'major' => (int) $v->major,
            'minor' => (int) $v->minor,
            'patch' => (int) $v->patch,
            'prerelease' => $prerelease,
        ];
        FFI::getLib()->proven_version_free(\FFI::addr($v));
        return $data;
    }

    /**
     * Compare two SemanticVersion values.
     *
     * Returns -1, 0, or 1 (like strcmp) or null on parse error.
     *
     * @return int|null -1 if a < b, 0 if equal, 1 if a > b, null on error.
     */
    public static function compare(string $a, string $b): ?int
    {
        $ra = FFI::getLib()->proven_version_parse($a, strlen($a));
        if ($ra->status !== 0) {
            return null;
        }
        $rb = FFI::getLib()->proven_version_parse($b, strlen($b));
        if ($rb->status !== 0) {
            FFI::getLib()->proven_version_free(\FFI::addr($ra->version));
            return null;
        }
        $cmp = FFI::getLib()->proven_version_compare($ra->version, $rb->version);
        FFI::getLib()->proven_version_free(\FFI::addr($ra->version));
        FFI::getLib()->proven_version_free(\FFI::addr($rb->version));
        return (int) $cmp;
    }
}
