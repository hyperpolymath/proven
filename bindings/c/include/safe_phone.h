/* SPDX-License-Identifier: AGPL-3.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_phone.h
 * @brief Safe phone number validation following E.164.
 *
 * This module provides phone number parsing and validation following
 * the E.164 international telephone numbering plan. It handles country
 * code detection, national number extraction, and various formatting options.
 */

#ifndef SAFE_PHONE_H
#define SAFE_PHONE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

/** Maximum E.164 phone number length (including country code) */
#define PHONE_MAX_DIGITS 15

/** Minimum phone number length (country code + national number) */
#define PHONE_MIN_DIGITS 7

/** Maximum buffer size for formatted phone numbers */
#define PHONE_FORMAT_BUF_SIZE 32

/* ============================================================================
 * Status Codes
 * ============================================================================ */

/**
 * @brief Status codes for phone number operations
 */
typedef enum PhoneStatus {
    PHONE_OK = 0,
    PHONE_ERR_NULL_POINTER = -1,
    PHONE_ERR_EMPTY_INPUT = -2,
    PHONE_ERR_TOO_SHORT = -3,
    PHONE_ERR_TOO_LONG = -4,
    PHONE_ERR_INVALID_CHARS = -5,
    PHONE_ERR_UNKNOWN_COUNTRY = -6,
    PHONE_ERR_BUFFER_TOO_SMALL = -7,
    PHONE_ERR_INVALID_FORMAT = -8
} PhoneStatus;

/* ============================================================================
 * Country Code Enumeration
 * ============================================================================ */

/**
 * @brief Country calling codes (ITU-T E.164)
 *
 * Values represent the numeric calling code.
 */
typedef enum CountryCode {
    COUNTRY_UNKNOWN = 0,
    COUNTRY_US = 1,       /**< USA, Canada, Caribbean */
    COUNTRY_RU = 7,       /**< Russia */
    COUNTRY_EG = 20,      /**< Egypt */
    COUNTRY_ZA = 27,      /**< South Africa */
    COUNTRY_GR = 30,      /**< Greece */
    COUNTRY_NL = 31,      /**< Netherlands */
    COUNTRY_BE = 32,      /**< Belgium */
    COUNTRY_FR = 33,      /**< France */
    COUNTRY_ES = 34,      /**< Spain */
    COUNTRY_HU = 36,      /**< Hungary */
    COUNTRY_IT = 39,      /**< Italy */
    COUNTRY_RO = 40,      /**< Romania */
    COUNTRY_CH = 41,      /**< Switzerland */
    COUNTRY_AT = 43,      /**< Austria */
    COUNTRY_GB = 44,      /**< United Kingdom */
    COUNTRY_DK = 45,      /**< Denmark */
    COUNTRY_SE = 46,      /**< Sweden */
    COUNTRY_NO = 47,      /**< Norway */
    COUNTRY_PL = 48,      /**< Poland */
    COUNTRY_DE = 49,      /**< Germany */
    COUNTRY_PE = 51,      /**< Peru */
    COUNTRY_MX = 52,      /**< Mexico */
    COUNTRY_AR = 54,      /**< Argentina */
    COUNTRY_BR = 55,      /**< Brazil */
    COUNTRY_CL = 56,      /**< Chile */
    COUNTRY_CO = 57,      /**< Colombia */
    COUNTRY_VE = 58,      /**< Venezuela */
    COUNTRY_MY = 60,      /**< Malaysia */
    COUNTRY_AU = 61,      /**< Australia */
    COUNTRY_ID = 62,      /**< Indonesia */
    COUNTRY_PH = 63,      /**< Philippines */
    COUNTRY_NZ = 64,      /**< New Zealand */
    COUNTRY_SG = 65,      /**< Singapore */
    COUNTRY_TH = 66,      /**< Thailand */
    COUNTRY_JP = 81,      /**< Japan */
    COUNTRY_KR = 82,      /**< South Korea */
    COUNTRY_VN = 84,      /**< Vietnam */
    COUNTRY_CN = 86,      /**< China */
    COUNTRY_TR = 90,      /**< Turkey */
    COUNTRY_IN = 91,      /**< India */
    COUNTRY_PK = 92,      /**< Pakistan */
    COUNTRY_AF = 93,      /**< Afghanistan */
    COUNTRY_LK = 94,      /**< Sri Lanka */
    COUNTRY_IR = 98,      /**< Iran */
    COUNTRY_MA = 212,     /**< Morocco */
    COUNTRY_DZ = 213,     /**< Algeria */
    COUNTRY_TN = 216,     /**< Tunisia */
    COUNTRY_NG = 234,     /**< Nigeria */
    COUNTRY_KE = 254,     /**< Kenya */
    COUNTRY_TZ = 255,     /**< Tanzania */
    COUNTRY_IL = 972,     /**< Israel */
    COUNTRY_SA = 966,     /**< Saudi Arabia */
    COUNTRY_AE = 971      /**< UAE */
} CountryCode;

/* ============================================================================
 * Structures
 * ============================================================================ */

/**
 * @brief Country code information
 */
typedef struct CountryInfo {
    CountryCode code;           /**< Country code enum */
    uint16_t calling_code;      /**< Numeric calling code */
    const char* iso_alpha2;     /**< 2-letter ISO code */
    const char* name;           /**< Country name */
} CountryInfo;

/**
 * @brief Parsed phone number
 */
typedef struct PhoneNumber {
    CountryCode country_code;           /**< Country code */
    char national_number[PHONE_MAX_DIGITS + 1]; /**< National number (digits only) */
    uint8_t national_number_len;        /**< Length of national number */
} PhoneNumber;

/**
 * @brief Result type for phone number parsing
 */
typedef struct PhoneResult {
    PhoneStatus status;
    PhoneNumber number;
} PhoneResult;

/**
 * @brief Result type for country code lookup
 */
typedef struct CountryCodeResult {
    PhoneStatus status;
    CountryCode code;
} CountryCodeResult;

/* ============================================================================
 * Phone Number Parsing
 * ============================================================================ */

/**
 * @brief Parse phone number from string
 * @param str Phone number string (may include +, spaces, dashes, parentheses)
 * @param len Length of string
 * @return PhoneResult with status and parsed number
 *
 * Accepts various formats:
 * - +1 555 123 4567
 * - 1-555-123-4567
 * - (555) 123-4567
 * - 15551234567
 */
PhoneResult phone_parse(const char* str, size_t len);

/**
 * @brief Parse phone number from null-terminated string
 * @param str Null-terminated phone number string
 * @return PhoneResult with status and parsed number
 */
PhoneResult phone_parse_cstr(const char* str);

/**
 * @brief Parse phone number with explicit country code
 * @param str Phone number string (national format)
 * @param len Length of string
 * @param country Country code to assume
 * @return PhoneResult with status and parsed number
 */
PhoneResult phone_parse_with_country(const char* str, size_t len, CountryCode country);

/* ============================================================================
 * Phone Number Validation
 * ============================================================================ */

/**
 * @brief Check if string is valid phone number
 * @param str Phone number string
 * @param len Length of string
 * @return true if valid phone number
 */
bool phone_is_valid(const char* str, size_t len);

/**
 * @brief Check if null-terminated string is valid phone number
 * @param str Null-terminated phone number string
 * @return true if valid phone number
 */
bool phone_is_valid_cstr(const char* str);

/* ============================================================================
 * Phone Number Formatting
 * ============================================================================ */

/**
 * @brief Format phone number in E.164 format
 * @param number Parsed phone number
 * @param[out] buf Output buffer
 * @param buf_size Buffer size
 * @return PHONE_OK on success, error code otherwise
 *
 * Output format: "+15551234567"
 */
PhoneStatus phone_format_e164(const PhoneNumber* number, char* buf, size_t buf_size);

/**
 * @brief Format phone number in international format
 * @param number Parsed phone number
 * @param[out] buf Output buffer
 * @param buf_size Buffer size
 * @return PHONE_OK on success, error code otherwise
 *
 * Output format: "+1 555 123 4567"
 */
PhoneStatus phone_format_international(const PhoneNumber* number, char* buf, size_t buf_size);

/**
 * @brief Format phone number in national format
 * @param number Parsed phone number
 * @param[out] buf Output buffer
 * @param buf_size Buffer size
 * @return PHONE_OK on success, error code otherwise
 *
 * Output format: "(555) 123-4567" (US format)
 */
PhoneStatus phone_format_national(const PhoneNumber* number, char* buf, size_t buf_size);

/**
 * @brief Format phone number in RFC 3966 tel: URI format
 * @param number Parsed phone number
 * @param[out] buf Output buffer
 * @param buf_size Buffer size
 * @return PHONE_OK on success, error code otherwise
 *
 * Output format: "tel:+1-555-123-4567"
 */
PhoneStatus phone_format_rfc3966(const PhoneNumber* number, char* buf, size_t buf_size);

/* ============================================================================
 * Country Code Functions
 * ============================================================================ */

/**
 * @brief Get country code from calling code number
 * @param calling_code Numeric calling code (e.g., 1 for USA)
 * @return CountryCode enum value
 */
CountryCode phone_country_from_calling_code(uint16_t calling_code);

/**
 * @brief Get country code from ISO alpha-2 code
 * @param iso_alpha2 2-letter ISO code (e.g., "US")
 * @return CountryCode enum value
 */
CountryCode phone_country_from_iso(const char* iso_alpha2);

/**
 * @brief Get numeric calling code from country code
 * @param code Country code enum
 * @return Numeric calling code (0 if unknown)
 */
uint16_t phone_get_calling_code(CountryCode code);

/**
 * @brief Get country information
 * @param code Country code enum
 * @return Pointer to country info, or NULL if unknown
 */
const CountryInfo* phone_get_country_info(CountryCode code);

/**
 * @brief Get country name
 * @param code Country code enum
 * @return Country name string
 */
const char* phone_get_country_name(CountryCode code);

/**
 * @brief Get ISO alpha-2 code
 * @param code Country code enum
 * @return 2-letter ISO code
 */
const char* phone_get_iso_alpha2(CountryCode code);

/* ============================================================================
 * Phone Number Queries
 * ============================================================================ */

/**
 * @brief Get total digit count (country code + national number)
 * @param number Parsed phone number
 * @return Total digit count
 */
size_t phone_digit_count(const PhoneNumber* number);

/**
 * @brief Compare two phone numbers
 * @param a First phone number
 * @param b Second phone number
 * @return 0 if equal, non-zero otherwise
 */
int phone_compare(const PhoneNumber* a, const PhoneNumber* b);

/**
 * @brief Check if two phone numbers are equal
 * @param a First phone number
 * @param b Second phone number
 * @return true if equal
 */
bool phone_equals(const PhoneNumber* a, const PhoneNumber* b);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/**
 * @brief Get status code description
 * @param status Status code
 * @return Static string describing the status
 */
const char* phone_status_string(PhoneStatus status);

/**
 * @brief Extract only digits from phone number string
 * @param str Input string
 * @param len Length of input
 * @param[out] digits Output buffer for digits
 * @param digits_size Size of output buffer
 * @return Number of digits extracted, or -1 on error
 */
int phone_extract_digits(const char* str, size_t len, char* digits, size_t digits_size);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_PHONE_H */
