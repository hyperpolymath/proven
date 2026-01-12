/* SPDX-License-Identifier: AGPL-3.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_currency.c
 * @brief Safe currency operations implementation.
 */

#include "../include/safe_currency.h"
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>

/* ============================================================================
 * Currency Database
 * ============================================================================ */

static const CurrencyInfo CURRENCY_DATABASE[] = {
    /* Major currencies */
    { CURRENCY_USD, "USD", "US Dollar", "$", 2, 840 },
    { CURRENCY_EUR, "EUR", "Euro", "\xe2\x82\xac", 2, 978 },  /* € */
    { CURRENCY_GBP, "GBP", "British Pound", "\xc2\xa3", 2, 826 },  /* £ */
    { CURRENCY_JPY, "JPY", "Japanese Yen", "\xc2\xa5", 0, 392 },  /* ¥ */
    { CURRENCY_CHF, "CHF", "Swiss Franc", "Fr", 2, 756 },
    { CURRENCY_CAD, "CAD", "Canadian Dollar", "C$", 2, 124 },
    { CURRENCY_AUD, "AUD", "Australian Dollar", "A$", 2, 36 },
    { CURRENCY_NZD, "NZD", "New Zealand Dollar", "NZ$", 2, 554 },
    { CURRENCY_CNY, "CNY", "Chinese Yuan", "\xc2\xa5", 2, 156 },  /* ¥ */
    { CURRENCY_INR, "INR", "Indian Rupee", "\xe2\x82\xb9", 2, 356 },  /* ₹ */

    /* Latin American */
    { CURRENCY_BRL, "BRL", "Brazilian Real", "R$", 2, 986 },
    { CURRENCY_MXN, "MXN", "Mexican Peso", "Mex$", 2, 484 },
    { CURRENCY_ARS, "ARS", "Argentine Peso", "$", 2, 32 },
    { CURRENCY_CLP, "CLP", "Chilean Peso", "$", 0, 152 },
    { CURRENCY_COP, "COP", "Colombian Peso", "$", 2, 170 },
    { CURRENCY_PEN, "PEN", "Peruvian Sol", "S/", 2, 604 },

    /* Asian */
    { CURRENCY_KRW, "KRW", "South Korean Won", "\xe2\x82\xa9", 0, 410 },  /* ₩ */
    { CURRENCY_SGD, "SGD", "Singapore Dollar", "S$", 2, 702 },
    { CURRENCY_HKD, "HKD", "Hong Kong Dollar", "HK$", 2, 344 },
    { CURRENCY_THB, "THB", "Thai Baht", "\xe0\xb8\xbf", 2, 764 },  /* ฿ */
    { CURRENCY_MYR, "MYR", "Malaysian Ringgit", "RM", 2, 458 },
    { CURRENCY_IDR, "IDR", "Indonesian Rupiah", "Rp", 2, 360 },
    { CURRENCY_PHP, "PHP", "Philippine Peso", "\xe2\x82\xb1", 2, 608 },  /* ₱ */
    { CURRENCY_VND, "VND", "Vietnamese Dong", "\xe2\x82\xab", 0, 704 },  /* ₫ */

    /* European */
    { CURRENCY_SEK, "SEK", "Swedish Krona", "kr", 2, 752 },
    { CURRENCY_NOK, "NOK", "Norwegian Krone", "kr", 2, 578 },
    { CURRENCY_DKK, "DKK", "Danish Krone", "kr", 2, 208 },
    { CURRENCY_PLN, "PLN", "Polish Zloty", "z\xc5\x82", 2, 985 },  /* zł */
    { CURRENCY_CZK, "CZK", "Czech Koruna", "K\xc4\x8d", 2, 203 },  /* Kč */
    { CURRENCY_HUF, "HUF", "Hungarian Forint", "Ft", 2, 348 },
    { CURRENCY_RON, "RON", "Romanian Leu", "lei", 2, 946 },
    { CURRENCY_BGN, "BGN", "Bulgarian Lev", "\xd0\xbb\xd0\xb2", 2, 975 },  /* лв */
    { CURRENCY_HRK, "HRK", "Croatian Kuna", "kn", 2, 191 },
    { CURRENCY_ISK, "ISK", "Icelandic Krona", "kr", 0, 352 },
    { CURRENCY_RUB, "RUB", "Russian Ruble", "\xe2\x82\xbd", 2, 643 },  /* ₽ */

    /* Middle East / Africa */
    { CURRENCY_ZAR, "ZAR", "South African Rand", "R", 2, 710 },
    { CURRENCY_TRY, "TRY", "Turkish Lira", "\xe2\x82\xba", 2, 949 },  /* ₺ */
    { CURRENCY_AED, "AED", "UAE Dirham", "AED", 2, 784 },
    { CURRENCY_SAR, "SAR", "Saudi Riyal", "SAR", 2, 682 },
    { CURRENCY_ILS, "ILS", "Israeli Shekel", "\xe2\x82\xaa", 2, 376 },  /* ₪ */

    /* Cryptocurrencies */
    { CURRENCY_BTC, "BTC", "Bitcoin", "\xe2\x82\xbf", 8, 0 },  /* ₿ */
    { CURRENCY_ETH, "ETH", "Ethereum", "\xce\x9e", 8, 0 },  /* Ξ */
};

/* ============================================================================
 * Internal Helpers
 * ============================================================================ */

/**
 * @brief Get multiplier for currency decimal places
 */
static int64_t get_multiplier(CurrencyCode code) {
    uint8_t decimals = currency_get_decimals(code);
    int64_t multiplier = 1;
    for (uint8_t i = 0; i < decimals; i++) {
        multiplier *= 10;
    }
    return multiplier;
}

/**
 * @brief Check for addition overflow
 */
static bool add_would_overflow(int64_t a, int64_t b) {
    if (b > 0 && a > INT64_MAX - b) return true;
    if (b < 0 && a < INT64_MIN - b) return true;
    return false;
}

/**
 * @brief Check for multiplication overflow
 */
static bool mul_would_overflow(int64_t a, int64_t b) {
    if (a == 0 || b == 0) return false;
    if (a > 0) {
        if (b > 0) {
            if (a > INT64_MAX / b) return true;
        } else {
            if (b < INT64_MIN / a) return true;
        }
    } else {
        if (b > 0) {
            if (a < INT64_MIN / b) return true;
        } else {
            if (a != 0 && b < INT64_MAX / a) return true;
        }
    }
    return false;
}

/* ============================================================================
 * Currency Code Functions
 * ============================================================================ */

CurrencyCodeResult currency_parse_code(const char* str, size_t len) {
    CurrencyCodeResult result = { .status = CURRENCY_OK, .code = CURRENCY_UNKNOWN };

    if (str == NULL) {
        result.status = CURRENCY_ERR_NULL_POINTER;
        return result;
    }

    if (len != 3) {
        result.status = CURRENCY_ERR_INVALID_CODE;
        return result;
    }

    /* Convert to uppercase for comparison */
    char upper[4];
    for (size_t i = 0; i < 3; i++) {
        upper[i] = (char)toupper((unsigned char)str[i]);
    }
    upper[3] = '\0';

    /* Search database */
    for (size_t i = 0; i < CURRENCY_COUNT; i++) {
        if (strcmp(upper, CURRENCY_DATABASE[i].iso_code) == 0) {
            result.code = CURRENCY_DATABASE[i].code;
            return result;
        }
    }

    result.status = CURRENCY_ERR_INVALID_CODE;
    return result;
}

CurrencyCodeResult currency_parse_code_cstr(const char* str) {
    if (str == NULL) {
        CurrencyCodeResult result = { .status = CURRENCY_ERR_NULL_POINTER, .code = CURRENCY_UNKNOWN };
        return result;
    }
    return currency_parse_code(str, strlen(str));
}

bool currency_is_valid_code(const char* str, size_t len) {
    CurrencyCodeResult result = currency_parse_code(str, len);
    return result.status == CURRENCY_OK;
}

const CurrencyInfo* currency_get_info(CurrencyCode code) {
    if (code < 0 || code >= CURRENCY_COUNT) {
        return NULL;
    }
    return &CURRENCY_DATABASE[code];
}

uint8_t currency_get_decimals(CurrencyCode code) {
    const CurrencyInfo* info = currency_get_info(code);
    return info ? info->decimal_places : 2;
}

const char* currency_get_symbol(CurrencyCode code) {
    const CurrencyInfo* info = currency_get_info(code);
    return info ? info->symbol : "";
}

const char* currency_get_name(CurrencyCode code) {
    const CurrencyInfo* info = currency_get_info(code);
    return info ? info->name : "Unknown";
}

const char* currency_get_iso_code(CurrencyCode code) {
    const CurrencyInfo* info = currency_get_info(code);
    return info ? info->iso_code : "XXX";
}

/* ============================================================================
 * Money Creation
 * ============================================================================ */

Money money_from_major(int64_t amount, CurrencyCode currency) {
    Money m;
    m.currency = currency;
    m.minor_units = amount * get_multiplier(currency);
    return m;
}

Money money_from_minor(int64_t amount, CurrencyCode currency) {
    Money m;
    m.currency = currency;
    m.minor_units = amount;
    return m;
}

Money money_zero(CurrencyCode currency) {
    Money m;
    m.currency = currency;
    m.minor_units = 0;
    return m;
}

MoneyResult money_parse(const char* str, size_t len, CurrencyCode currency) {
    MoneyResult result = { .status = CURRENCY_OK, .value = money_zero(currency) };

    if (str == NULL) {
        result.status = CURRENCY_ERR_NULL_POINTER;
        return result;
    }

    if (len == 0) {
        result.status = CURRENCY_ERR_PARSE_FAILED;
        return result;
    }

    /* Find decimal point */
    const char* decimal_pos = NULL;
    for (size_t i = 0; i < len; i++) {
        if (str[i] == '.') {
            decimal_pos = &str[i];
            break;
        }
    }

    /* Parse major part */
    char* end_ptr;
    long long major = strtoll(str, &end_ptr, 10);

    if (end_ptr == str) {
        result.status = CURRENCY_ERR_PARSE_FAILED;
        return result;
    }

    /* Parse minor part if present */
    int64_t minor = 0;
    uint8_t decimals = currency_get_decimals(currency);

    if (decimal_pos != NULL && decimals > 0) {
        const char* frac_start = decimal_pos + 1;
        size_t frac_len = (str + len) - frac_start;

        if (frac_len > decimals) {
            frac_len = decimals;  /* Truncate extra digits */
        }

        for (size_t i = 0; i < frac_len; i++) {
            if (!isdigit((unsigned char)frac_start[i])) {
                result.status = CURRENCY_ERR_PARSE_FAILED;
                return result;
            }
            minor = minor * 10 + (frac_start[i] - '0');
        }

        /* Pad with zeros if needed */
        for (size_t i = frac_len; i < decimals; i++) {
            minor *= 10;
        }
    }

    int64_t multiplier = get_multiplier(currency);
    result.value.currency = currency;
    result.value.minor_units = major * multiplier + (major < 0 ? -minor : minor);

    return result;
}

/* ============================================================================
 * Money Arithmetic
 * ============================================================================ */

MoneyResult money_add(Money a, Money b) {
    MoneyResult result = { .status = CURRENCY_OK, .value = a };

    if (a.currency != b.currency) {
        result.status = CURRENCY_ERR_CURRENCY_MISMATCH;
        return result;
    }

    if (add_would_overflow(a.minor_units, b.minor_units)) {
        result.status = CURRENCY_ERR_OVERFLOW;
        return result;
    }

    result.value.minor_units = a.minor_units + b.minor_units;
    return result;
}

MoneyResult money_sub(Money a, Money b) {
    MoneyResult result = { .status = CURRENCY_OK, .value = a };

    if (a.currency != b.currency) {
        result.status = CURRENCY_ERR_CURRENCY_MISMATCH;
        return result;
    }

    if (add_would_overflow(a.minor_units, -b.minor_units)) {
        result.status = CURRENCY_ERR_UNDERFLOW;
        return result;
    }

    result.value.minor_units = a.minor_units - b.minor_units;
    return result;
}

MoneyResult money_mul(Money m, int64_t scalar) {
    MoneyResult result = { .status = CURRENCY_OK, .value = m };

    if (mul_would_overflow(m.minor_units, scalar)) {
        result.status = CURRENCY_ERR_OVERFLOW;
        return result;
    }

    result.value.minor_units = m.minor_units * scalar;
    return result;
}

MoneyResult money_div(Money m, int64_t divisor) {
    MoneyResult result = { .status = CURRENCY_OK, .value = m };

    if (divisor == 0) {
        result.status = CURRENCY_ERR_DIVISION_BY_ZERO;
        return result;
    }

    result.value.minor_units = m.minor_units / divisor;
    return result;
}

MoneyResult money_abs(Money m) {
    MoneyResult result = { .status = CURRENCY_OK, .value = m };

    if (m.minor_units == INT64_MIN) {
        result.status = CURRENCY_ERR_OVERFLOW;
        return result;
    }

    result.value.minor_units = m.minor_units < 0 ? -m.minor_units : m.minor_units;
    return result;
}

MoneyResult money_negate(Money m) {
    MoneyResult result = { .status = CURRENCY_OK, .value = m };

    if (m.minor_units == INT64_MIN) {
        result.status = CURRENCY_ERR_OVERFLOW;
        return result;
    }

    result.value.minor_units = -m.minor_units;
    return result;
}

/* ============================================================================
 * Money Queries
 * ============================================================================ */

int64_t money_get_major(Money m) {
    int64_t multiplier = get_multiplier(m.currency);
    return m.minor_units / multiplier;
}

int64_t money_get_minor(Money m) {
    return m.minor_units;
}

int64_t money_get_fraction(Money m) {
    int64_t multiplier = get_multiplier(m.currency);
    int64_t abs_minor = m.minor_units < 0 ? -m.minor_units : m.minor_units;
    return abs_minor % multiplier;
}

bool money_is_zero(Money m) {
    return m.minor_units == 0;
}

bool money_is_positive(Money m) {
    return m.minor_units > 0;
}

bool money_is_negative(Money m) {
    return m.minor_units < 0;
}

int money_compare(Money a, Money b) {
    if (a.currency != b.currency) {
        return 0;  /* Cannot compare different currencies */
    }

    if (a.minor_units < b.minor_units) return -1;
    if (a.minor_units > b.minor_units) return 1;
    return 0;
}

bool money_same_currency(Money a, Money b) {
    return a.currency == b.currency;
}

/* ============================================================================
 * Money Formatting
 * ============================================================================ */

CurrencyStatus money_format(Money m, char* buf, size_t buf_size) {
    if (buf == NULL) {
        return CURRENCY_ERR_NULL_POINTER;
    }

    const char* symbol = currency_get_symbol(m.currency);
    uint8_t decimals = currency_get_decimals(m.currency);
    int64_t multiplier = get_multiplier(m.currency);

    int64_t abs_minor = m.minor_units < 0 ? -m.minor_units : m.minor_units;
    int64_t major = abs_minor / multiplier;
    int64_t fraction = abs_minor % multiplier;

    int written;
    if (decimals == 0) {
        written = snprintf(buf, buf_size, "%s%s%lld",
                          m.minor_units < 0 ? "-" : "",
                          symbol,
                          (long long)major);
    } else {
        written = snprintf(buf, buf_size, "%s%s%lld.%0*lld",
                          m.minor_units < 0 ? "-" : "",
                          symbol,
                          (long long)major,
                          decimals,
                          (long long)fraction);
    }

    if (written < 0 || (size_t)written >= buf_size) {
        return CURRENCY_ERR_BUFFER_TOO_SMALL;
    }

    return CURRENCY_OK;
}

CurrencyStatus money_format_plain(Money m, char* buf, size_t buf_size) {
    if (buf == NULL) {
        return CURRENCY_ERR_NULL_POINTER;
    }

    uint8_t decimals = currency_get_decimals(m.currency);
    int64_t multiplier = get_multiplier(m.currency);

    int64_t abs_minor = m.minor_units < 0 ? -m.minor_units : m.minor_units;
    int64_t major = abs_minor / multiplier;
    int64_t fraction = abs_minor % multiplier;

    int written;
    if (decimals == 0) {
        written = snprintf(buf, buf_size, "%s%lld",
                          m.minor_units < 0 ? "-" : "",
                          (long long)major);
    } else {
        written = snprintf(buf, buf_size, "%s%lld.%0*lld",
                          m.minor_units < 0 ? "-" : "",
                          (long long)major,
                          decimals,
                          (long long)fraction);
    }

    if (written < 0 || (size_t)written >= buf_size) {
        return CURRENCY_ERR_BUFFER_TOO_SMALL;
    }

    return CURRENCY_OK;
}

CurrencyStatus money_format_iso(Money m, char* buf, size_t buf_size) {
    if (buf == NULL) {
        return CURRENCY_ERR_NULL_POINTER;
    }

    const char* iso = currency_get_iso_code(m.currency);
    uint8_t decimals = currency_get_decimals(m.currency);
    int64_t multiplier = get_multiplier(m.currency);

    int64_t abs_minor = m.minor_units < 0 ? -m.minor_units : m.minor_units;
    int64_t major = abs_minor / multiplier;
    int64_t fraction = abs_minor % multiplier;

    int written;
    if (decimals == 0) {
        written = snprintf(buf, buf_size, "%s%lld %s",
                          m.minor_units < 0 ? "-" : "",
                          (long long)major,
                          iso);
    } else {
        written = snprintf(buf, buf_size, "%s%lld.%0*lld %s",
                          m.minor_units < 0 ? "-" : "",
                          (long long)major,
                          decimals,
                          (long long)fraction,
                          iso);
    }

    if (written < 0 || (size_t)written >= buf_size) {
        return CURRENCY_ERR_BUFFER_TOO_SMALL;
    }

    return CURRENCY_OK;
}

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

const char* currency_status_string(CurrencyStatus status) {
    switch (status) {
        case CURRENCY_OK:                    return "Success";
        case CURRENCY_ERR_NULL_POINTER:      return "Null pointer";
        case CURRENCY_ERR_INVALID_CODE:      return "Invalid currency code";
        case CURRENCY_ERR_CURRENCY_MISMATCH: return "Currency mismatch";
        case CURRENCY_ERR_OVERFLOW:          return "Arithmetic overflow";
        case CURRENCY_ERR_UNDERFLOW:         return "Arithmetic underflow";
        case CURRENCY_ERR_DIVISION_BY_ZERO:  return "Division by zero";
        case CURRENCY_ERR_BUFFER_TOO_SMALL:  return "Buffer too small";
        case CURRENCY_ERR_PARSE_FAILED:      return "Parse failed";
        default:                             return "Unknown error";
    }
}
