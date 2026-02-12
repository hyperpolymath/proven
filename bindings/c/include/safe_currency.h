/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_currency.h
 * @brief Safe currency operations with type-safe monetary values.
 *
 * This module provides ISO 4217 currency codes, type-safe monetary values
 * stored as minor units (cents, satoshis, etc.), and arithmetic operations
 * with overflow checking.
 */

#ifndef SAFE_CURRENCY_H
#define SAFE_CURRENCY_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Status Codes
 * ============================================================================ */

/**
 * @brief Status codes for currency operations
 */
typedef enum CurrencyStatus {
    CURRENCY_OK = 0,
    CURRENCY_ERR_NULL_POINTER = -1,
    CURRENCY_ERR_INVALID_CODE = -2,
    CURRENCY_ERR_CURRENCY_MISMATCH = -3,
    CURRENCY_ERR_OVERFLOW = -4,
    CURRENCY_ERR_UNDERFLOW = -5,
    CURRENCY_ERR_DIVISION_BY_ZERO = -6,
    CURRENCY_ERR_BUFFER_TOO_SMALL = -7,
    CURRENCY_ERR_PARSE_FAILED = -8
} CurrencyStatus;

/* ============================================================================
 * Currency Code Enumeration
 * ============================================================================ */

/**
 * @brief ISO 4217 currency codes
 *
 * Common currencies plus cryptocurrencies. The enum values are assigned
 * based on position, not numeric codes.
 */
typedef enum CurrencyCode {
    /* Major currencies */
    CURRENCY_USD = 0,   /**< US Dollar */
    CURRENCY_EUR,       /**< Euro */
    CURRENCY_GBP,       /**< British Pound */
    CURRENCY_JPY,       /**< Japanese Yen */
    CURRENCY_CHF,       /**< Swiss Franc */
    CURRENCY_CAD,       /**< Canadian Dollar */
    CURRENCY_AUD,       /**< Australian Dollar */
    CURRENCY_NZD,       /**< New Zealand Dollar */
    CURRENCY_CNY,       /**< Chinese Yuan */
    CURRENCY_INR,       /**< Indian Rupee */

    /* Latin American */
    CURRENCY_BRL,       /**< Brazilian Real */
    CURRENCY_MXN,       /**< Mexican Peso */
    CURRENCY_ARS,       /**< Argentine Peso */
    CURRENCY_CLP,       /**< Chilean Peso */
    CURRENCY_COP,       /**< Colombian Peso */
    CURRENCY_PEN,       /**< Peruvian Sol */

    /* Asian */
    CURRENCY_KRW,       /**< South Korean Won */
    CURRENCY_SGD,       /**< Singapore Dollar */
    CURRENCY_HKD,       /**< Hong Kong Dollar */
    CURRENCY_THB,       /**< Thai Baht */
    CURRENCY_MYR,       /**< Malaysian Ringgit */
    CURRENCY_IDR,       /**< Indonesian Rupiah */
    CURRENCY_PHP,       /**< Philippine Peso */
    CURRENCY_VND,       /**< Vietnamese Dong */

    /* European */
    CURRENCY_SEK,       /**< Swedish Krona */
    CURRENCY_NOK,       /**< Norwegian Krone */
    CURRENCY_DKK,       /**< Danish Krone */
    CURRENCY_PLN,       /**< Polish Zloty */
    CURRENCY_CZK,       /**< Czech Koruna */
    CURRENCY_HUF,       /**< Hungarian Forint */
    CURRENCY_RON,       /**< Romanian Leu */
    CURRENCY_BGN,       /**< Bulgarian Lev */
    CURRENCY_HRK,       /**< Croatian Kuna */
    CURRENCY_ISK,       /**< Icelandic Krona */
    CURRENCY_RUB,       /**< Russian Ruble */

    /* Middle East / Africa */
    CURRENCY_ZAR,       /**< South African Rand */
    CURRENCY_TRY,       /**< Turkish Lira */
    CURRENCY_AED,       /**< UAE Dirham */
    CURRENCY_SAR,       /**< Saudi Riyal */
    CURRENCY_ILS,       /**< Israeli Shekel */

    /* Cryptocurrencies */
    CURRENCY_BTC,       /**< Bitcoin */
    CURRENCY_ETH,       /**< Ethereum */

    /* Sentinel */
    CURRENCY_COUNT,     /**< Number of currencies */
    CURRENCY_UNKNOWN = -1 /**< Unknown currency */
} CurrencyCode;

/* ============================================================================
 * Structures
 * ============================================================================ */

/**
 * @brief Currency information structure
 */
typedef struct CurrencyInfo {
    CurrencyCode code;          /**< Currency code */
    const char* iso_code;       /**< 3-letter ISO code */
    const char* name;           /**< Full name */
    const char* symbol;         /**< Currency symbol */
    uint8_t decimal_places;     /**< Number of decimal places */
    uint16_t iso_numeric;       /**< ISO 4217 numeric code */
} CurrencyInfo;

/**
 * @brief Type-safe monetary value
 *
 * Stores amounts in minor units (cents, satoshis, etc.) to avoid
 * floating-point precision issues.
 */
typedef struct Money {
    int64_t minor_units;        /**< Amount in minor units */
    CurrencyCode currency;      /**< Currency code */
} Money;

/**
 * @brief Result type for money operations
 */
typedef struct MoneyResult {
    CurrencyStatus status;
    Money value;
} MoneyResult;

/**
 * @brief Result type for currency code parsing
 */
typedef struct CurrencyCodeResult {
    CurrencyStatus status;
    CurrencyCode code;
} CurrencyCodeResult;

/* ============================================================================
 * Currency Code Functions
 * ============================================================================ */

/**
 * @brief Parse currency code from string
 * @param str 3-letter currency code (e.g., "USD", "EUR")
 * @param len Length of string
 * @return CurrencyCodeResult with status and parsed code
 */
CurrencyCodeResult currency_parse_code(const char* str, size_t len);

/**
 * @brief Parse currency code from null-terminated string
 * @param str Null-terminated 3-letter currency code
 * @return CurrencyCodeResult with status and parsed code
 */
CurrencyCodeResult currency_parse_code_cstr(const char* str);

/**
 * @brief Check if string is valid currency code
 * @param str String to check
 * @param len Length of string
 * @return true if valid currency code
 */
bool currency_is_valid_code(const char* str, size_t len);

/**
 * @brief Get currency information
 * @param code Currency code
 * @return Pointer to currency info, or NULL if invalid code
 */
const CurrencyInfo* currency_get_info(CurrencyCode code);

/**
 * @brief Get decimal places for currency
 * @param code Currency code
 * @return Number of decimal places (0, 2, or 8)
 */
uint8_t currency_get_decimals(CurrencyCode code);

/**
 * @brief Get currency symbol
 * @param code Currency code
 * @return Currency symbol string (may be empty for some currencies)
 */
const char* currency_get_symbol(CurrencyCode code);

/**
 * @brief Get currency name
 * @param code Currency code
 * @return Currency name string
 */
const char* currency_get_name(CurrencyCode code);

/**
 * @brief Get ISO 4217 3-letter code
 * @param code Currency code
 * @return 3-letter ISO code (e.g., "USD")
 */
const char* currency_get_iso_code(CurrencyCode code);

/* ============================================================================
 * Money Creation
 * ============================================================================ */

/**
 * @brief Create money from major units
 * @param amount Amount in major units (dollars, euros, etc.)
 * @param currency Currency code
 * @return Money struct
 *
 * Example: money_from_major(100, CURRENCY_USD) = $100.00 = 10000 cents
 */
Money money_from_major(int64_t amount, CurrencyCode currency);

/**
 * @brief Create money from minor units
 * @param amount Amount in minor units (cents, satoshis, etc.)
 * @param currency Currency code
 * @return Money struct
 *
 * Example: money_from_minor(10000, CURRENCY_USD) = $100.00
 */
Money money_from_minor(int64_t amount, CurrencyCode currency);

/**
 * @brief Create zero money
 * @param currency Currency code
 * @return Money struct with zero value
 */
Money money_zero(CurrencyCode currency);

/**
 * @brief Parse money from string
 * @param str Amount string (e.g., "123.45")
 * @param len Length of string
 * @param currency Currency code
 * @return MoneyResult with status and parsed value
 */
MoneyResult money_parse(const char* str, size_t len, CurrencyCode currency);

/* ============================================================================
 * Money Arithmetic
 * ============================================================================ */

/**
 * @brief Add two money values
 * @param a First value
 * @param b Second value
 * @return MoneyResult with status and sum
 *
 * Returns CURRENCY_ERR_CURRENCY_MISMATCH if currencies differ.
 * Returns CURRENCY_ERR_OVERFLOW if result would overflow.
 */
MoneyResult money_add(Money a, Money b);

/**
 * @brief Subtract two money values
 * @param a First value
 * @param b Second value (subtracted from a)
 * @return MoneyResult with status and difference
 *
 * Returns CURRENCY_ERR_CURRENCY_MISMATCH if currencies differ.
 * Returns CURRENCY_ERR_UNDERFLOW if result would underflow.
 */
MoneyResult money_sub(Money a, Money b);

/**
 * @brief Multiply money by scalar
 * @param m Money value
 * @param scalar Integer multiplier
 * @return MoneyResult with status and product
 *
 * Returns CURRENCY_ERR_OVERFLOW if result would overflow.
 */
MoneyResult money_mul(Money m, int64_t scalar);

/**
 * @brief Divide money by scalar
 * @param m Money value
 * @param divisor Integer divisor
 * @return MoneyResult with status and quotient
 *
 * Returns CURRENCY_ERR_DIVISION_BY_ZERO if divisor is 0.
 */
MoneyResult money_div(Money m, int64_t divisor);

/**
 * @brief Get absolute value of money
 * @param m Money value
 * @return MoneyResult with status and absolute value
 */
MoneyResult money_abs(Money m);

/**
 * @brief Negate money value
 * @param m Money value
 * @return MoneyResult with status and negated value
 */
MoneyResult money_negate(Money m);

/* ============================================================================
 * Money Queries
 * ============================================================================ */

/**
 * @brief Get major units (truncated)
 * @param m Money value
 * @return Major units (dollars, euros, etc.)
 */
int64_t money_get_major(Money m);

/**
 * @brief Get minor units
 * @param m Money value
 * @return Minor units (cents, satoshis, etc.)
 */
int64_t money_get_minor(Money m);

/**
 * @brief Get fractional part in minor units
 * @param m Money value
 * @return Fractional part (0-99 for 2 decimal currencies)
 */
int64_t money_get_fraction(Money m);

/**
 * @brief Check if money is zero
 * @param m Money value
 * @return true if zero
 */
bool money_is_zero(Money m);

/**
 * @brief Check if money is positive
 * @param m Money value
 * @return true if positive (> 0)
 */
bool money_is_positive(Money m);

/**
 * @brief Check if money is negative
 * @param m Money value
 * @return true if negative (< 0)
 */
bool money_is_negative(Money m);

/**
 * @brief Compare two money values
 * @param a First value
 * @param b Second value
 * @return 0 if equal, negative if a < b, positive if a > b
 *
 * @note Returns 0 if currencies differ (check with money_same_currency first)
 */
int money_compare(Money a, Money b);

/**
 * @brief Check if two money values have the same currency
 * @param a First value
 * @param b Second value
 * @return true if same currency
 */
bool money_same_currency(Money a, Money b);

/* ============================================================================
 * Money Formatting
 * ============================================================================ */

/**
 * @brief Format money as string with symbol
 * @param m Money value
 * @param[out] buf Output buffer
 * @param buf_size Buffer size
 * @return CURRENCY_OK on success, error code otherwise
 *
 * Output format: "$123.45" or "-$123.45"
 */
CurrencyStatus money_format(Money m, char* buf, size_t buf_size);

/**
 * @brief Format money as plain decimal string
 * @param m Money value
 * @param[out] buf Output buffer
 * @param buf_size Buffer size
 * @return CURRENCY_OK on success, error code otherwise
 *
 * Output format: "123.45" or "-123.45"
 */
CurrencyStatus money_format_plain(Money m, char* buf, size_t buf_size);

/**
 * @brief Format money with ISO code suffix
 * @param m Money value
 * @param[out] buf Output buffer
 * @param buf_size Buffer size
 * @return CURRENCY_OK on success, error code otherwise
 *
 * Output format: "123.45 USD"
 */
CurrencyStatus money_format_iso(Money m, char* buf, size_t buf_size);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/**
 * @brief Get status code description
 * @param status Status code
 * @return Static string describing the status
 */
const char* currency_status_string(CurrencyStatus status);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_CURRENCY_H */
