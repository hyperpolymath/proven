<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeContentType - FFI wrapper for proven_content_type_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe content-type operations via libproven FFI.
 *
 * Provides MIME type sniffing detection and type classification.
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeContentType
{
    /**
     * Check if a content type can be sniffed as something dangerous
     * (e.g. text/html could be executed as script by browsers).
     */
    public static function canSniffDangerous(string $contentType): ?bool
    {
        $result = FFI::getLib()->proven_content_type_can_sniff_dangerous(
            $contentType, strlen($contentType)
        );
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Check if a MIME subtype/suffix pair represents JSON.
     */
    public static function isJson(string $subtype, string $suffix): ?bool
    {
        $result = FFI::getLib()->proven_content_type_is_json(
            $subtype, strlen($subtype), $suffix, strlen($suffix)
        );
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Check if a MIME subtype/suffix pair represents XML.
     */
    public static function isXml(string $subtype, string $suffix): ?bool
    {
        $result = FFI::getLib()->proven_content_type_is_xml(
            $subtype, strlen($subtype), $suffix, strlen($suffix)
        );
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }
}
