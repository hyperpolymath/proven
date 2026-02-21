/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_datetime.h
 * @brief ISO 8601 date/time handling
 *
 * Provides parsing and formatting of ISO 8601 date/time strings
 * with timezone support.
 */

#ifndef SAFE_DATETIME_H
#define SAFE_DATETIME_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * DateTime Types
 * ============================================================================ */

/**
 * @brief DateTime components
 */
typedef struct ProvenDateTime {
    int32_t year;
    uint8_t month;      /**< 1-12 */
    uint8_t day;        /**< 1-31 */
    uint8_t hour;       /**< 0-23 */
    uint8_t minute;     /**< 0-59 */
    uint8_t second;     /**< 0-59 */
    uint32_t nanosecond;
    int16_t tz_offset_minutes;  /**< 0 for UTC, negative for west of UTC */
} ProvenDateTime;

/**
 * @brief DateTime result
 */
typedef struct ProvenDateTimeResult {
    ProvenStatus status;
    ProvenDateTime datetime;
} ProvenDateTimeResult;

/* ============================================================================
 * DateTime Parsing
 * ============================================================================ */

/**
 * @brief Parse ISO 8601 date string
 * @param ptr Pointer to date string (e.g., "2024-12-15T10:30:00Z")
 * @param len Length of string
 * @return Result with parsed datetime
 *
 * Supports formats:
 * - YYYY-MM-DD
 * - YYYY-MM-DDTHH:MM:SS
 * - YYYY-MM-DDTHH:MM:SSZ
 * - YYYY-MM-DDTHH:MM:SS+HH:MM
 */
ProvenDateTimeResult proven_datetime_parse(const uint8_t* ptr, size_t len);

/* ============================================================================
 * DateTime Formatting
 * ============================================================================ */

/**
 * @brief Format DateTime as ISO 8601 string
 * @param dt DateTime to format
 * @return Result with formatted string (caller must free)
 *
 * Output format: YYYY-MM-DDTHH:MM:SSZ
 */
ProvenStringResult proven_datetime_format_iso8601(ProvenDateTime dt);

/* ============================================================================
 * DateTime Utilities
 * ============================================================================ */

/**
 * @brief Check if year is a leap year
 * @param year Year to check
 * @return true if leap year
 */
bool proven_datetime_is_leap_year(int32_t year);

/**
 * @brief Get number of days in a month
 * @param year Year (for leap year calculation)
 * @param month Month (1-12)
 * @return Number of days (0 if invalid month)
 */
uint8_t proven_datetime_days_in_month(int32_t year, uint8_t month);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_DATETIME_H */
