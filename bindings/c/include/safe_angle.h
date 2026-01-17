/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_angle.h
 * @brief Angle conversions and normalization
 *
 * Provides conversion between angle units and normalization
 * to standard ranges.
 */

#ifndef SAFE_ANGLE_H
#define SAFE_ANGLE_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Angle Conversion
 * ============================================================================ */

/**
 * @brief Convert degrees to radians
 * @param degrees Angle in degrees
 * @return Angle in radians
 */
double proven_angle_deg_to_rad(double degrees);

/**
 * @brief Convert radians to degrees
 * @param radians Angle in radians
 * @return Angle in degrees
 */
double proven_angle_rad_to_deg(double radians);

/* ============================================================================
 * Angle Normalization
 * ============================================================================ */

/**
 * @brief Normalize angle to 0-360 degrees
 * @param degrees Angle in degrees (any value)
 * @return Angle normalized to [0, 360)
 *
 * Returns 0 for NaN input.
 */
double proven_angle_normalize_degrees(double degrees);

/**
 * @brief Normalize angle to 0-2*pi radians
 * @param radians Angle in radians (any value)
 * @return Angle normalized to [0, 2*pi)
 *
 * Returns 0 for NaN input.
 */
double proven_angle_normalize_radians(double radians);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_ANGLE_H */
