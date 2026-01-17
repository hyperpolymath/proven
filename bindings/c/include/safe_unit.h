/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_unit.h
 * @brief Physical unit conversions
 *
 * Provides safe conversion between physical units with
 * proper handling of edge cases.
 */

#ifndef SAFE_UNIT_H
#define SAFE_UNIT_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Unit Types
 * ============================================================================ */

/**
 * @brief Length units
 */
typedef enum ProvenLengthUnit {
    PROVEN_LENGTH_METERS = 0,
    PROVEN_LENGTH_KILOMETERS = 1,
    PROVEN_LENGTH_CENTIMETERS = 2,
    PROVEN_LENGTH_MILLIMETERS = 3,
    PROVEN_LENGTH_FEET = 4,
    PROVEN_LENGTH_INCHES = 5,
    PROVEN_LENGTH_MILES = 6,
    PROVEN_LENGTH_YARDS = 7
} ProvenLengthUnit;

/**
 * @brief Temperature units
 */
typedef enum ProvenTempUnit {
    PROVEN_TEMP_CELSIUS = 0,
    PROVEN_TEMP_FAHRENHEIT = 1,
    PROVEN_TEMP_KELVIN = 2
} ProvenTempUnit;

/* ============================================================================
 * Unit Conversion
 * ============================================================================ */

/**
 * @brief Convert length between units
 * @param value Value to convert
 * @param from Source unit
 * @param to Target unit
 * @return Result with converted value
 *
 * Returns PROVEN_ERR_INVALID_ARGUMENT for NaN input.
 */
ProvenFloatResult proven_unit_convert_length(double value, ProvenLengthUnit from, ProvenLengthUnit to);

/**
 * @brief Convert temperature between units
 * @param value Temperature to convert
 * @param from Source unit
 * @param to Target unit
 * @return Result with converted value
 *
 * Returns PROVEN_ERR_INVALID_ARGUMENT for NaN input.
 * Returns PROVEN_ERR_OUT_OF_BOUNDS for temperatures below absolute zero.
 */
ProvenFloatResult proven_unit_convert_temp(double value, ProvenTempUnit from, ProvenTempUnit to);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_UNIT_H */
