/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_color.h
 * @brief Color space conversions
 *
 * Provides parsing and conversion between color representations
 * (RGB, HSL, hex strings).
 */

#ifndef SAFE_COLOR_H
#define SAFE_COLOR_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Color Types
 * ============================================================================ */

/**
 * @brief RGB color (8-bit per channel)
 */
typedef struct ProvenRGBColor {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} ProvenRGBColor;

/**
 * @brief HSL color
 */
typedef struct ProvenHSLColor {
    double h;  /**< Hue: 0-360 degrees */
    double s;  /**< Saturation: 0-1 */
    double l;  /**< Lightness: 0-1 */
} ProvenHSLColor;

/**
 * @brief Color parsing result
 */
typedef struct ProvenColorResult {
    ProvenStatus status;
    ProvenRGBColor color;
} ProvenColorResult;

/* ============================================================================
 * Color Parsing
 * ============================================================================ */

/**
 * @brief Parse hex color string
 * @param ptr Pointer to hex string (e.g., "#FF0000" or "#F00")
 * @param len Length of string
 * @return Result with parsed RGB color
 *
 * Supports both 3-digit (#RGB) and 6-digit (#RRGGBB) formats.
 * Leading # is optional.
 */
ProvenColorResult proven_color_parse_hex(const uint8_t* ptr, size_t len);

/* ============================================================================
 * Color Conversion
 * ============================================================================ */

/**
 * @brief Convert RGB to HSL
 * @param rgb RGB color
 * @return HSL color
 */
ProvenHSLColor proven_color_rgb_to_hsl(ProvenRGBColor rgb);

/**
 * @brief Format RGB as hex string
 * @param rgb RGB color
 * @return Result with hex string (caller must free)
 *
 * Output format: "#rrggbb" (lowercase)
 */
ProvenStringResult proven_color_to_hex(ProvenRGBColor rgb);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_COLOR_H */
