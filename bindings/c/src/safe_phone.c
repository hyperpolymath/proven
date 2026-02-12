/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_phone.c
 * @brief Safe phone number validation implementation.
 */

#include "../include/safe_phone.h"
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/* ============================================================================
 * Country Database
 * ============================================================================ */

static const CountryInfo COUNTRY_DATABASE[] = {
    { COUNTRY_UNKNOWN, 0, "XX", "Unknown" },
    { COUNTRY_US, 1, "US", "United States" },
    { COUNTRY_RU, 7, "RU", "Russia" },
    { COUNTRY_EG, 20, "EG", "Egypt" },
    { COUNTRY_ZA, 27, "ZA", "South Africa" },
    { COUNTRY_GR, 30, "GR", "Greece" },
    { COUNTRY_NL, 31, "NL", "Netherlands" },
    { COUNTRY_BE, 32, "BE", "Belgium" },
    { COUNTRY_FR, 33, "FR", "France" },
    { COUNTRY_ES, 34, "ES", "Spain" },
    { COUNTRY_HU, 36, "HU", "Hungary" },
    { COUNTRY_IT, 39, "IT", "Italy" },
    { COUNTRY_RO, 40, "RO", "Romania" },
    { COUNTRY_CH, 41, "CH", "Switzerland" },
    { COUNTRY_AT, 43, "AT", "Austria" },
    { COUNTRY_GB, 44, "GB", "United Kingdom" },
    { COUNTRY_DK, 45, "DK", "Denmark" },
    { COUNTRY_SE, 46, "SE", "Sweden" },
    { COUNTRY_NO, 47, "NO", "Norway" },
    { COUNTRY_PL, 48, "PL", "Poland" },
    { COUNTRY_DE, 49, "DE", "Germany" },
    { COUNTRY_PE, 51, "PE", "Peru" },
    { COUNTRY_MX, 52, "MX", "Mexico" },
    { COUNTRY_AR, 54, "AR", "Argentina" },
    { COUNTRY_BR, 55, "BR", "Brazil" },
    { COUNTRY_CL, 56, "CL", "Chile" },
    { COUNTRY_CO, 57, "CO", "Colombia" },
    { COUNTRY_VE, 58, "VE", "Venezuela" },
    { COUNTRY_MY, 60, "MY", "Malaysia" },
    { COUNTRY_AU, 61, "AU", "Australia" },
    { COUNTRY_ID, 62, "ID", "Indonesia" },
    { COUNTRY_PH, 63, "PH", "Philippines" },
    { COUNTRY_NZ, 64, "NZ", "New Zealand" },
    { COUNTRY_SG, 65, "SG", "Singapore" },
    { COUNTRY_TH, 66, "TH", "Thailand" },
    { COUNTRY_JP, 81, "JP", "Japan" },
    { COUNTRY_KR, 82, "KR", "South Korea" },
    { COUNTRY_VN, 84, "VN", "Vietnam" },
    { COUNTRY_CN, 86, "CN", "China" },
    { COUNTRY_TR, 90, "TR", "Turkey" },
    { COUNTRY_IN, 91, "IN", "India" },
    { COUNTRY_PK, 92, "PK", "Pakistan" },
    { COUNTRY_AF, 93, "AF", "Afghanistan" },
    { COUNTRY_LK, 94, "LK", "Sri Lanka" },
    { COUNTRY_IR, 98, "IR", "Iran" },
    { COUNTRY_MA, 212, "MA", "Morocco" },
    { COUNTRY_DZ, 213, "DZ", "Algeria" },
    { COUNTRY_TN, 216, "TN", "Tunisia" },
    { COUNTRY_NG, 234, "NG", "Nigeria" },
    { COUNTRY_KE, 254, "KE", "Kenya" },
    { COUNTRY_TZ, 255, "TZ", "Tanzania" },
    { COUNTRY_SA, 966, "SA", "Saudi Arabia" },
    { COUNTRY_AE, 971, "AE", "United Arab Emirates" },
    { COUNTRY_IL, 972, "IL", "Israel" },
};

static const size_t COUNTRY_DATABASE_SIZE = sizeof(COUNTRY_DATABASE) / sizeof(COUNTRY_DATABASE[0]);

/* ============================================================================
 * Internal Helpers
 * ============================================================================ */

/**
 * @brief Find country info by calling code
 */
static const CountryInfo* find_country_by_calling_code(uint16_t calling_code) {
    for (size_t i = 0; i < COUNTRY_DATABASE_SIZE; i++) {
        if (COUNTRY_DATABASE[i].calling_code == calling_code) {
            return &COUNTRY_DATABASE[i];
        }
    }
    return NULL;
}

/**
 * @brief Find country info by code enum
 */
static const CountryInfo* find_country_by_code(CountryCode code) {
    for (size_t i = 0; i < COUNTRY_DATABASE_SIZE; i++) {
        if (COUNTRY_DATABASE[i].code == code) {
            return &COUNTRY_DATABASE[i];
        }
    }
    return &COUNTRY_DATABASE[0];  /* Return UNKNOWN */
}

/**
 * @brief Get number of digits in calling code
 */
static size_t calling_code_digit_count(uint16_t calling_code) {
    if (calling_code >= 100) return 3;
    if (calling_code >= 10) return 2;
    return 1;
}

/**
 * @brief Try to parse country code from digit string
 */
static bool try_parse_country_code(const char* digits, size_t len,
                                   CountryCode* out_code, size_t* out_consumed) {
    /* Try 3-digit codes first, then 2, then 1 */
    for (size_t cc_len = 3; cc_len >= 1; cc_len--) {
        if (len < cc_len) continue;

        /* Parse digits as number */
        uint16_t value = 0;
        for (size_t i = 0; i < cc_len; i++) {
            value = value * 10 + (digits[i] - '0');
        }

        const CountryInfo* info = find_country_by_calling_code(value);
        if (info != NULL && info->code != COUNTRY_UNKNOWN) {
            *out_code = info->code;
            *out_consumed = cc_len;
            return true;
        }
    }

    return false;
}

/* ============================================================================
 * Phone Number Parsing
 * ============================================================================ */

int phone_extract_digits(const char* str, size_t len, char* digits, size_t digits_size) {
    if (str == NULL || digits == NULL) {
        return -1;
    }

    size_t digit_count = 0;
    for (size_t i = 0; i < len && digit_count < digits_size - 1; i++) {
        if (isdigit((unsigned char)str[i])) {
            digits[digit_count++] = str[i];
        }
    }
    digits[digit_count] = '\0';

    return (int)digit_count;
}

PhoneResult phone_parse(const char* str, size_t len) {
    PhoneResult result = { .status = PHONE_OK };
    memset(&result.number, 0, sizeof(result.number));

    if (str == NULL) {
        result.status = PHONE_ERR_NULL_POINTER;
        return result;
    }

    /* Skip leading whitespace */
    while (len > 0 && isspace((unsigned char)*str)) {
        str++;
        len--;
    }

    if (len == 0) {
        result.status = PHONE_ERR_EMPTY_INPUT;
        return result;
    }

    /* Extract digits */
    char digits[PHONE_MAX_DIGITS + 1];
    int digit_count = phone_extract_digits(str, len, digits, sizeof(digits));

    if (digit_count < 0) {
        result.status = PHONE_ERR_INVALID_FORMAT;
        return result;
    }

    if (digit_count < PHONE_MIN_DIGITS) {
        result.status = PHONE_ERR_TOO_SHORT;
        return result;
    }

    if (digit_count > PHONE_MAX_DIGITS) {
        result.status = PHONE_ERR_TOO_LONG;
        return result;
    }

    /* Try to parse country code */
    CountryCode country_code = COUNTRY_UNKNOWN;
    size_t country_code_len = 0;

    if (try_parse_country_code(digits, digit_count, &country_code, &country_code_len)) {
        /* Found valid country code */
        result.number.country_code = country_code;

        /* Rest is national number */
        size_t national_len = digit_count - country_code_len;
        if (national_len < 4) {
            result.status = PHONE_ERR_TOO_SHORT;
            return result;
        }

        memcpy(result.number.national_number, digits + country_code_len, national_len);
        result.number.national_number[national_len] = '\0';
        result.number.national_number_len = (uint8_t)national_len;
    } else {
        result.status = PHONE_ERR_UNKNOWN_COUNTRY;
        return result;
    }

    return result;
}

PhoneResult phone_parse_cstr(const char* str) {
    if (str == NULL) {
        PhoneResult result = { .status = PHONE_ERR_NULL_POINTER };
        return result;
    }
    return phone_parse(str, strlen(str));
}

PhoneResult phone_parse_with_country(const char* str, size_t len, CountryCode country) {
    PhoneResult result = { .status = PHONE_OK };
    memset(&result.number, 0, sizeof(result.number));

    if (str == NULL) {
        result.status = PHONE_ERR_NULL_POINTER;
        return result;
    }

    /* Extract digits */
    char digits[PHONE_MAX_DIGITS + 1];
    int digit_count = phone_extract_digits(str, len, digits, sizeof(digits));

    if (digit_count < 0) {
        result.status = PHONE_ERR_INVALID_FORMAT;
        return result;
    }

    if (digit_count < 4) {
        result.status = PHONE_ERR_TOO_SHORT;
        return result;
    }

    /* Use provided country code */
    result.number.country_code = country;
    memcpy(result.number.national_number, digits, digit_count);
    result.number.national_number[digit_count] = '\0';
    result.number.national_number_len = (uint8_t)digit_count;

    /* Validate total length */
    uint16_t calling_code = phone_get_calling_code(country);
    size_t cc_digits = calling_code_digit_count(calling_code);

    if (cc_digits + digit_count > PHONE_MAX_DIGITS) {
        result.status = PHONE_ERR_TOO_LONG;
        return result;
    }

    return result;
}

/* ============================================================================
 * Phone Number Validation
 * ============================================================================ */

bool phone_is_valid(const char* str, size_t len) {
    PhoneResult result = phone_parse(str, len);
    return result.status == PHONE_OK;
}

bool phone_is_valid_cstr(const char* str) {
    if (str == NULL) return false;
    return phone_is_valid(str, strlen(str));
}

/* ============================================================================
 * Phone Number Formatting
 * ============================================================================ */

PhoneStatus phone_format_e164(const PhoneNumber* number, char* buf, size_t buf_size) {
    if (number == NULL || buf == NULL) {
        return PHONE_ERR_NULL_POINTER;
    }

    uint16_t calling_code = phone_get_calling_code(number->country_code);
    int written = snprintf(buf, buf_size, "+%u%s", calling_code, number->national_number);

    if (written < 0 || (size_t)written >= buf_size) {
        return PHONE_ERR_BUFFER_TOO_SMALL;
    }

    return PHONE_OK;
}

PhoneStatus phone_format_international(const PhoneNumber* number, char* buf, size_t buf_size) {
    if (number == NULL || buf == NULL) {
        return PHONE_ERR_NULL_POINTER;
    }

    uint16_t calling_code = phone_get_calling_code(number->country_code);
    const char* nat = number->national_number;
    size_t len = number->national_number_len;

    int written;
    if (len <= 4) {
        written = snprintf(buf, buf_size, "+%u %s", calling_code, nat);
    } else if (len <= 7) {
        written = snprintf(buf, buf_size, "+%u %.3s %s",
                          calling_code, nat, nat + 3);
    } else if (len <= 10) {
        written = snprintf(buf, buf_size, "+%u %.3s %.3s %s",
                          calling_code, nat, nat + 3, nat + 6);
    } else {
        /* Long number - just space after country code */
        written = snprintf(buf, buf_size, "+%u %s", calling_code, nat);
    }

    if (written < 0 || (size_t)written >= buf_size) {
        return PHONE_ERR_BUFFER_TOO_SMALL;
    }

    return PHONE_OK;
}

PhoneStatus phone_format_national(const PhoneNumber* number, char* buf, size_t buf_size) {
    if (number == NULL || buf == NULL) {
        return PHONE_ERR_NULL_POINTER;
    }

    const char* nat = number->national_number;
    size_t len = number->national_number_len;

    int written;

    /* US/Canada style for NANP numbers */
    if (number->country_code == COUNTRY_US && len == 10) {
        written = snprintf(buf, buf_size, "(%.3s) %.3s-%.4s",
                          nat, nat + 3, nat + 6);
    } else if (len <= 4) {
        written = snprintf(buf, buf_size, "%s", nat);
    } else if (len <= 7) {
        written = snprintf(buf, buf_size, "%.3s-%.4s", nat, nat + 3);
    } else if (len <= 10) {
        written = snprintf(buf, buf_size, "%.3s-%.3s-%.4s", nat, nat + 3, nat + 6);
    } else {
        written = snprintf(buf, buf_size, "%s", nat);
    }

    if (written < 0 || (size_t)written >= buf_size) {
        return PHONE_ERR_BUFFER_TOO_SMALL;
    }

    return PHONE_OK;
}

PhoneStatus phone_format_rfc3966(const PhoneNumber* number, char* buf, size_t buf_size) {
    if (number == NULL || buf == NULL) {
        return PHONE_ERR_NULL_POINTER;
    }

    uint16_t calling_code = phone_get_calling_code(number->country_code);
    const char* nat = number->national_number;
    size_t len = number->national_number_len;

    int written;
    if (len <= 4) {
        written = snprintf(buf, buf_size, "tel:+%u-%s", calling_code, nat);
    } else if (len <= 7) {
        written = snprintf(buf, buf_size, "tel:+%u-%.3s-%s",
                          calling_code, nat, nat + 3);
    } else if (len <= 10) {
        written = snprintf(buf, buf_size, "tel:+%u-%.3s-%.3s-%s",
                          calling_code, nat, nat + 3, nat + 6);
    } else {
        written = snprintf(buf, buf_size, "tel:+%u-%s", calling_code, nat);
    }

    if (written < 0 || (size_t)written >= buf_size) {
        return PHONE_ERR_BUFFER_TOO_SMALL;
    }

    return PHONE_OK;
}

/* ============================================================================
 * Country Code Functions
 * ============================================================================ */

CountryCode phone_country_from_calling_code(uint16_t calling_code) {
    const CountryInfo* info = find_country_by_calling_code(calling_code);
    return info ? info->code : COUNTRY_UNKNOWN;
}

CountryCode phone_country_from_iso(const char* iso_alpha2) {
    if (iso_alpha2 == NULL || strlen(iso_alpha2) != 2) {
        return COUNTRY_UNKNOWN;
    }

    char upper[3];
    upper[0] = (char)toupper((unsigned char)iso_alpha2[0]);
    upper[1] = (char)toupper((unsigned char)iso_alpha2[1]);
    upper[2] = '\0';

    for (size_t i = 0; i < COUNTRY_DATABASE_SIZE; i++) {
        if (strcmp(COUNTRY_DATABASE[i].iso_alpha2, upper) == 0) {
            return COUNTRY_DATABASE[i].code;
        }
    }

    return COUNTRY_UNKNOWN;
}

uint16_t phone_get_calling_code(CountryCode code) {
    const CountryInfo* info = find_country_by_code(code);
    return info ? info->calling_code : 0;
}

const CountryInfo* phone_get_country_info(CountryCode code) {
    return find_country_by_code(code);
}

const char* phone_get_country_name(CountryCode code) {
    const CountryInfo* info = find_country_by_code(code);
    return info ? info->name : "Unknown";
}

const char* phone_get_iso_alpha2(CountryCode code) {
    const CountryInfo* info = find_country_by_code(code);
    return info ? info->iso_alpha2 : "XX";
}

/* ============================================================================
 * Phone Number Queries
 * ============================================================================ */

size_t phone_digit_count(const PhoneNumber* number) {
    if (number == NULL) return 0;

    uint16_t calling_code = phone_get_calling_code(number->country_code);
    size_t cc_digits = calling_code_digit_count(calling_code);

    return cc_digits + number->national_number_len;
}

int phone_compare(const PhoneNumber* a, const PhoneNumber* b) {
    if (a == NULL && b == NULL) return 0;
    if (a == NULL) return -1;
    if (b == NULL) return 1;

    /* Compare country codes first */
    uint16_t cc_a = phone_get_calling_code(a->country_code);
    uint16_t cc_b = phone_get_calling_code(b->country_code);

    if (cc_a != cc_b) {
        return (cc_a < cc_b) ? -1 : 1;
    }

    /* Then compare national numbers */
    return strcmp(a->national_number, b->national_number);
}

bool phone_equals(const PhoneNumber* a, const PhoneNumber* b) {
    return phone_compare(a, b) == 0;
}

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

const char* phone_status_string(PhoneStatus status) {
    switch (status) {
        case PHONE_OK:                  return "Success";
        case PHONE_ERR_NULL_POINTER:    return "Null pointer";
        case PHONE_ERR_EMPTY_INPUT:     return "Empty input";
        case PHONE_ERR_TOO_SHORT:       return "Phone number too short";
        case PHONE_ERR_TOO_LONG:        return "Phone number too long";
        case PHONE_ERR_INVALID_CHARS:   return "Invalid characters";
        case PHONE_ERR_UNKNOWN_COUNTRY: return "Unknown country code";
        case PHONE_ERR_BUFFER_TOO_SMALL: return "Buffer too small";
        case PHONE_ERR_INVALID_FORMAT:  return "Invalid format";
        default:                        return "Unknown error";
    }
}
