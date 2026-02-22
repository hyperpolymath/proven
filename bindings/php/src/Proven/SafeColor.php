<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeColor - FFI wrapper for proven_color_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe color parsing and conversion via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeColor
{
    /**
     * Parse a hex color string (e.g. "#FF0000" or "FF0000").
     *
     * @return array{r: int, g: int, b: int}|null RGB values, or null on error.
     */
    public static function parseHex(string $hex): ?array
    {
        $result = FFI::getLib()->proven_color_parse_hex($hex, strlen($hex));
        if ($result->status !== 0) {
            return null;
        }
        return [
            'r' => (int) $result->color->r,
            'g' => (int) $result->color->g,
            'b' => (int) $result->color->b,
        ];
    }

    /**
     * Convert an RGB color to HSL.
     *
     * @return array{h: float, s: float, l: float} HSL values.
     */
    public static function rgbToHsl(int $r, int $g, int $b): array
    {
        $rgb = FFI::getLib()->new('RGBColor');
        $rgb->r = $r;
        $rgb->g = $g;
        $rgb->b = $b;
        $hsl = FFI::getLib()->proven_color_rgb_to_hsl($rgb);
        return [
            'h' => (float) $hsl->h,
            's' => (float) $hsl->s,
            'l' => (float) $hsl->l,
        ];
    }

    /**
     * Convert an RGB color to a hex string.
     *
     * @return string|null The hex string (e.g. "#ff0000"), or null on error.
     */
    public static function toHex(int $r, int $g, int $b): ?string
    {
        $rgb = FFI::getLib()->new('RGBColor');
        $rgb->r = $r;
        $rgb->g = $g;
        $rgb->b = $b;
        $result = FFI::getLib()->proven_color_to_hex($rgb);
        if ($result->status !== 0 || $result->value === null) {
            return null;
        }
        $str = \FFI::string($result->value, $result->length);
        FFI::getLib()->proven_free_string($result->value);
        return $str;
    }
}
