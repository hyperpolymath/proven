/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/**
 * @file proven.h
 * @brief Proven FFI - Complete C header for libproven
 *
 * This is the single, complete header file declaring all exported functions
 * from libproven. The library is implemented in Idris 2 (formally verified)
 * with a Zig FFI bridge exposing a stable C ABI. Link against libproven.so
 * or libproven.a to use these functions.
 *
 * All computation is performed in verified Idris 2 code; this header is a
 * thin, type-safe declaration layer. Do NOT reimplement any logic in C.
 *
 * Memory management rules:
 *   - ProvenStringResult.value must be freed with proven_free_string()
 *   - ProvenUrlResult must be freed with proven_url_free()
 *   - ProvenContentTypeResult must be freed with proven_content_type_free()
 *   - ProvenHexDecodeResult must be freed with proven_hex_free()
 *   - Opaque pointer types (buffer, queue, etc.) have matching *_free() calls
 *   - Integer, boolean, and float results do not require freeing
 *
 * @example
 * @code
 * #include <proven.h>
 * #include <stdio.h>
 *
 * int main(void) {
 *     proven_init();
 *
 *     ProvenIntResult result = proven_math_div(10, 0);
 *     if (PROVEN_FAILED(result)) {
 *         printf("Division by zero handled safely: status=%d\n", result.status);
 *     }
 *
 *     proven_deinit();
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_H
#define PROVEN_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Version
 * ============================================================================ */

/** Library version string */
#define PROVEN_VERSION_STRING "0.9.0"

/** Total number of modules in the library */
#define PROVEN_MODULE_COUNT 41

/* ============================================================================
 * Status Codes
 * ============================================================================ */

/**
 * @brief Status codes returned by Proven operations.
 *
 * Zero indicates success; negative values indicate specific error conditions.
 */
typedef enum {
    PROVEN_OK                      =   0,
    PROVEN_ERR_NULL_POINTER        =  -1,
    PROVEN_ERR_INVALID_ARGUMENT    =  -2,
    PROVEN_ERR_OVERFLOW            =  -3,
    PROVEN_ERR_UNDERFLOW           =  -4,
    PROVEN_ERR_DIVISION_BY_ZERO    =  -5,
    PROVEN_ERR_PARSE_FAILURE       =  -6,
    PROVEN_ERR_VALIDATION_FAILED   =  -7,
    PROVEN_ERR_OUT_OF_BOUNDS       =  -8,
    PROVEN_ERR_ENCODING_ERROR      =  -9,
    PROVEN_ERR_ALLOCATION_FAILED   = -10,
    PROVEN_ERR_NOT_IMPLEMENTED     = -99
} ProvenStatus;

/* ============================================================================
 * Core Result Types
 * ============================================================================ */

/** @brief Result for integer operations */
typedef struct { int32_t status; int64_t value; } ProvenIntResult;

/** @brief Result for boolean operations */
typedef struct { int32_t status; bool value; } ProvenBoolResult;

/**
 * @brief Result for string operations.
 * @note Caller must free value using proven_free_string().
 */
typedef struct { int32_t status; char* value; size_t length; } ProvenStringResult;

/** @brief Result for floating-point operations */
typedef struct { int32_t status; double value; } ProvenFloatResult;

/* ============================================================================
 * Convenience Macros
 * ============================================================================ */

/** @brief Check if a result indicates success */
#define PROVEN_SUCCEEDED(result) ((result).status == PROVEN_OK)

/** @brief Check if a result indicates failure */
#define PROVEN_FAILED(result) ((result).status != PROVEN_OK)

/* ============================================================================
 * Memory Management (1 function)
 * ============================================================================ */

/**
 * @brief Free a string allocated by Proven functions.
 * @param ptr String to free (may be NULL).
 */
void proven_free_string(char* ptr);

/* ============================================================================
 * Lifecycle (3 functions)
 * ============================================================================ */

/**
 * @brief Initialize the Proven runtime (includes Idris 2 runtime).
 * @return PROVEN_OK on success, error code otherwise.
 *
 * Must be called before any other Proven function. Safe to call multiple times.
 */
int32_t proven_init(void);

/**
 * @brief Cleanup the Proven runtime.
 *
 * Call when done using Proven functions. All allocated resources should be
 * freed before calling this.
 */
void proven_deinit(void);

/**
 * @brief Check if the runtime is initialized.
 * @return true if initialized.
 */
bool proven_is_initialized(void);

/* ============================================================================
 * Version Information (4 functions)
 * ============================================================================ */

/** @brief Get FFI ABI version for compatibility checking. */
uint32_t proven_ffi_abi_version(void);

/** @brief Get major version number. */
uint32_t proven_version_major(void);

/** @brief Get minor version number. */
uint32_t proven_version_minor(void);

/** @brief Get patch version number. */
uint32_t proven_version_patch(void);

/** @brief Get total module count. */
uint32_t proven_module_count(void);

/* ============================================================================
 * SafeMath - Arithmetic that cannot crash (9 functions)
 * ============================================================================ */

/**
 * @brief Safe integer division.
 * @param numerator Dividend.
 * @param denominator Divisor.
 * @return Result with quotient; PROVEN_ERR_DIVISION_BY_ZERO if denominator is 0,
 *         PROVEN_ERR_OVERFLOW for INT64_MIN / -1.
 */
ProvenIntResult proven_math_div(int64_t numerator, int64_t denominator);

/**
 * @brief Safe modulo operation.
 * @return Result with remainder; PROVEN_ERR_DIVISION_BY_ZERO if denominator is 0.
 */
ProvenIntResult proven_math_mod(int64_t numerator, int64_t denominator);

/**
 * @brief Checked addition with overflow detection.
 * @return PROVEN_ERR_OVERFLOW if result would overflow int64_t.
 */
ProvenIntResult proven_math_add_checked(int64_t a, int64_t b);

/**
 * @brief Checked subtraction with underflow detection.
 * @return PROVEN_ERR_UNDERFLOW if result would underflow int64_t.
 */
ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b);

/**
 * @brief Checked multiplication with overflow detection.
 * @return PROVEN_ERR_OVERFLOW if result would overflow int64_t.
 */
ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b);

/**
 * @brief Safe absolute value.
 * @return PROVEN_ERR_OVERFLOW for INT64_MIN (cannot be represented as positive).
 */
ProvenIntResult proven_math_abs_safe(int64_t n);

/**
 * @brief Clamp value to [lo, hi] range.
 * @param lo Lower bound (inclusive).
 * @param hi Upper bound (inclusive).
 * @param value Value to clamp.
 * @return Clamped value.
 */
int64_t proven_math_clamp(int64_t lo, int64_t hi, int64_t value);

/**
 * @brief Integer exponentiation with overflow checking.
 * @param base Base value.
 * @param exp Exponent (must be non-negative).
 * @return PROVEN_ERR_OVERFLOW if result would overflow int64_t.
 */
ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp);

/* ============================================================================
 * SafeString - Text operations that handle encoding safely (4 functions)
 * ============================================================================ */

/**
 * @brief Check if bytes are valid UTF-8.
 * @param ptr Pointer to byte data.
 * @param len Length of data.
 * @return Result with validation status.
 */
ProvenBoolResult proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for SQL (single quotes).
 * @note Prefer parameterized queries over string escaping.
 * @return Result with escaped string (caller must free).
 */
ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for HTML (prevents XSS).
 * @return Result with escaped string (caller must free).
 */
ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for JavaScript string literals.
 * @return Result with escaped string (caller must free).
 */
ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafePath - Filesystem traversal prevention (2 functions)
 * ============================================================================ */

/**
 * @brief Check if path contains directory traversal sequences ("..").
 * @return Result with traversal detection status (true = traversal detected).
 */
ProvenBoolResult proven_path_has_traversal(const uint8_t* ptr, size_t len);

/**
 * @brief Sanitize a filename by removing dangerous characters.
 * @return Result with sanitized filename (caller must free).
 */
ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeCrypto - Cryptographic primitives (2 functions)
 * ============================================================================ */

/**
 * @brief Constant-time byte comparison (timing-attack safe).
 * @param ptr1 First buffer.
 * @param len1 Length of first buffer.
 * @param ptr2 Second buffer.
 * @param len2 Length of second buffer.
 * @return Result with comparison status. Returns false if lengths differ.
 */
ProvenBoolResult proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2
);

/**
 * @brief Fill buffer with cryptographically secure random bytes.
 * @param ptr Buffer to fill.
 * @param len Number of bytes to generate.
 * @return Status code.
 */
ProvenStatus proven_crypto_random_bytes(uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeUrl - URL parsing and validation (2 functions)
 * ============================================================================ */

/** @brief URL components structure returned by proven_url_parse(). */
typedef struct {
    char*    scheme;       size_t scheme_len;
    char*    host;         size_t host_len;
    uint16_t port;         bool   has_port;
    char*    path;         size_t path_len;
    char*    query;        size_t query_len;
    char*    fragment;     size_t fragment_len;
} ProvenUrlComponents;

/** @brief Result for URL parsing. */
typedef struct {
    int32_t             status;
    ProvenUrlComponents components;
} ProvenUrlResult;

/**
 * @brief Parse a URL into components.
 * @return Result with URL components (caller must free with proven_url_free).
 */
ProvenUrlResult proven_url_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Free URL components allocated by proven_url_parse().
 * @param components Pointer to URL components to free.
 */
void proven_url_free(ProvenUrlComponents* components);

/* ============================================================================
 * SafeEmail - Email validation (1 function)
 * ============================================================================ */

/**
 * @brief Validate email address (RFC 5321 simplified).
 * @return Result with validation status.
 */
ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeNetwork - IP address parsing and classification (3 functions)
 * ============================================================================ */

/** @brief IPv4 address structure. */
typedef struct { uint8_t octets[4]; } ProvenIPv4Address;

/** @brief Result for IPv4 parsing. */
typedef struct {
    int32_t          status;
    ProvenIPv4Address address;
} ProvenIPv4Result;

/**
 * @brief Parse IPv4 address string (e.g., "192.168.1.1").
 * @return Result with parsed address.
 */
ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);

/**
 * @brief Check if IPv4 address is private (RFC 1918).
 * @return true if private (10.x.x.x, 172.16-31.x.x, 192.168.x.x).
 */
bool proven_network_ipv4_is_private(ProvenIPv4Address addr);

/**
 * @brief Check if IPv4 address is loopback (127.0.0.0/8).
 * @return true if loopback.
 */
bool proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

/* ============================================================================
 * SafeHeader - HTTP header safety (6 functions)
 * ============================================================================ */

/**
 * @brief Check for CRLF injection characters in header value.
 * @return Result with detection status (true = CRLF found).
 */
ProvenBoolResult proven_header_has_crlf(const uint8_t* ptr, size_t len);

/**
 * @brief Check if header name is a valid token per RFC 7230.
 * @return Result with validation status.
 */
ProvenBoolResult proven_header_is_valid_name(const uint8_t* ptr, size_t len);

/**
 * @brief Check if header name is in the dangerous headers list.
 * @return Result with detection status (true = dangerous).
 */
ProvenBoolResult proven_header_is_dangerous(const uint8_t* ptr, size_t len);

/**
 * @brief Create validated header string "Name: Value".
 * @return Result with rendered header (caller must free).
 */
ProvenStringResult proven_header_render(
    const uint8_t* name_ptr, size_t name_len,
    const uint8_t* value_ptr, size_t value_len
);

/**
 * @brief Build Content-Security-Policy header value from JSON directives.
 * @param directives_json JSON-encoded CSP directives.
 * @param json_len Length of JSON string.
 * @return Result with CSP header value (caller must free).
 */
ProvenStringResult proven_header_build_csp(const uint8_t* directives_json, size_t json_len);

/**
 * @brief Build HSTS header value.
 * @param max_age Max-Age directive in seconds.
 * @param include_subdomains Whether to include includeSubDomains.
 * @param preload Whether to include preload directive.
 * @return Result with HSTS header value (caller must free).
 */
ProvenStringResult proven_header_build_hsts(int64_t max_age, bool include_subdomains, bool preload);

/* ============================================================================
 * SafeCookie - HTTP cookie safety (6 functions)
 * ============================================================================ */

/** @brief SameSite attribute values. */
typedef enum {
    PROVEN_SAMESITE_STRICT = 0,
    PROVEN_SAMESITE_LAX    = 1,
    PROVEN_SAMESITE_NONE   = 2
} ProvenSameSite;

/** @brief Cookie attributes for Set-Cookie header building. */
typedef struct {
    const uint8_t* domain;       size_t domain_len;
    const uint8_t* path;         size_t path_len;
    int64_t        max_age;      /**< -1 means not set */
    bool           secure;
    bool           http_only;
    ProvenSameSite same_site;
    bool           partitioned;
} ProvenCookieAttributes;

/**
 * @brief Check for cookie injection characters (semicolon, CR, LF).
 * @return Result with detection status (true = injection found).
 */
ProvenBoolResult proven_cookie_has_injection(const uint8_t* ptr, size_t len);

/**
 * @brief Validate cookie name.
 * @return Result with validation status.
 */
ProvenBoolResult proven_cookie_validate_name(const uint8_t* ptr, size_t len);

/**
 * @brief Validate cookie value.
 * @return Result with validation status.
 */
ProvenBoolResult proven_cookie_validate_value(const uint8_t* ptr, size_t len);

/**
 * @brief Get cookie prefix type.
 * @return Result with prefix type: 0=none, 1=__Secure-, 2=__Host-.
 */
ProvenIntResult proven_cookie_get_prefix(const uint8_t* ptr, size_t len);

/**
 * @brief Build Set-Cookie header value.
 * @param name_ptr Cookie name.
 * @param name_len Cookie name length.
 * @param value_ptr Cookie value.
 * @param value_len Cookie value length.
 * @param attrs Cookie attributes.
 * @return Result with Set-Cookie value (caller must free).
 */
ProvenStringResult proven_cookie_build_set_cookie(
    const uint8_t* name_ptr, size_t name_len,
    const uint8_t* value_ptr, size_t value_len,
    ProvenCookieAttributes attrs
);

/**
 * @brief Build delete cookie header value (sets expiry in the past).
 * @return Result with Set-Cookie value (caller must free).
 */
ProvenStringResult proven_cookie_build_delete(const uint8_t* name_ptr, size_t name_len);

/* ============================================================================
 * SafeContentType - Content-Type / MIME handling (6 functions)
 * ============================================================================ */

/** @brief Media type category. */
typedef enum {
    PROVEN_MEDIA_TEXT        = 0,
    PROVEN_MEDIA_IMAGE       = 1,
    PROVEN_MEDIA_AUDIO       = 2,
    PROVEN_MEDIA_VIDEO       = 3,
    PROVEN_MEDIA_APPLICATION = 4,
    PROVEN_MEDIA_MULTIPART   = 5,
    PROVEN_MEDIA_MESSAGE     = 6,
    PROVEN_MEDIA_FONT        = 7,
    PROVEN_MEDIA_MODEL       = 8,
    PROVEN_MEDIA_CUSTOM      = 9
} ProvenMediaCategory;

/** @brief Charset encoding. */
typedef enum {
    PROVEN_CHARSET_UTF8        = 0,
    PROVEN_CHARSET_UTF16LE     = 1,
    PROVEN_CHARSET_UTF16BE     = 2,
    PROVEN_CHARSET_ISO8859_1   = 3,
    PROVEN_CHARSET_ASCII       = 4,
    PROVEN_CHARSET_WINDOWS1252 = 5,
    PROVEN_CHARSET_OTHER       = 6
} ProvenCharset;

/** @brief Content-Type parse result. */
typedef struct {
    int32_t            status;
    char*              media_type;     size_t media_type_len;
    char*              subtype;        size_t subtype_len;
    char*              suffix;         size_t suffix_len;
    ProvenMediaCategory category;
    ProvenCharset       charset;
    bool                has_charset;
} ProvenContentTypeResult;

/**
 * @brief Parse Content-Type header.
 * @return Result with parsed components (caller must free with proven_content_type_free).
 */
ProvenContentTypeResult proven_content_type_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Free content type result allocated by proven_content_type_parse().
 */
void proven_content_type_free(ProvenContentTypeResult* result);

/**
 * @brief Check if content type can be sniffed to something dangerous.
 * @return Result with detection status (true = dangerous sniffing possible).
 */
ProvenBoolResult proven_content_type_can_sniff_dangerous(const uint8_t* ptr, size_t len);

/**
 * @brief Render content type to string.
 * @return Result with rendered content type (caller must free).
 */
ProvenStringResult proven_content_type_render(
    const uint8_t* type_ptr, size_t type_len,
    const uint8_t* subtype_ptr, size_t subtype_len,
    const uint8_t* suffix_ptr, size_t suffix_len,
    ProvenCharset charset, bool has_charset
);

/**
 * @brief Check if content type is JSON.
 * @return Result with detection status.
 */
ProvenBoolResult proven_content_type_is_json(
    const uint8_t* subtype_ptr, size_t subtype_len,
    const uint8_t* suffix_ptr, size_t suffix_len
);

/**
 * @brief Check if content type is XML.
 * @return Result with detection status.
 */
ProvenBoolResult proven_content_type_is_xml(
    const uint8_t* subtype_ptr, size_t subtype_len,
    const uint8_t* suffix_ptr, size_t suffix_len
);

/* ============================================================================
 * SafeUUID - UUID generation and validation (5 functions)
 * ============================================================================ */

/** @brief UUID structure (128-bit identifier). */
typedef struct { uint8_t bytes[16]; } ProvenUUID;

/** @brief UUID result. */
typedef struct { int32_t status; ProvenUUID uuid; } ProvenUUIDResult;

/**
 * @brief Generate a UUID v4 (random).
 * @return Result with generated UUID.
 */
ProvenUUIDResult proven_uuid_v4(void);

/**
 * @brief Format UUID as canonical string ("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx").
 * @return Result with formatted string (caller must free).
 */
ProvenStringResult proven_uuid_to_string(ProvenUUID uuid);

/**
 * @brief Parse UUID from string.
 * @return Result with parsed UUID.
 */
ProvenUUIDResult proven_uuid_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Check if UUID is nil (all zeros).
 * @return true if nil.
 */
bool proven_uuid_is_nil(ProvenUUID uuid);

/**
 * @brief Get UUID version.
 * @return Version number (4 for v4, etc.).
 */
uint8_t proven_uuid_version(ProvenUUID uuid);

/* ============================================================================
 * SafeJson - JSON validation and type detection (2 functions)
 * ============================================================================ */

/** @brief JSON value type. */
typedef enum {
    PROVEN_JSON_NULL    =  0,
    PROVEN_JSON_BOOL    =  1,
    PROVEN_JSON_NUMBER  =  2,
    PROVEN_JSON_STRING  =  3,
    PROVEN_JSON_ARRAY   =  4,
    PROVEN_JSON_OBJECT  =  5,
    PROVEN_JSON_INVALID = -1
} ProvenJsonType;

/**
 * @brief Check if string is valid JSON.
 * @return Result with validation status.
 */
ProvenBoolResult proven_json_is_valid(const uint8_t* ptr, size_t len);

/**
 * @brief Get JSON value type at root level.
 * @return JSON type (PROVEN_JSON_INVALID if not valid JSON).
 */
ProvenJsonType proven_json_get_type(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeDateTime - ISO 8601 date/time handling (4 functions)
 * ============================================================================ */

/** @brief DateTime components. */
typedef struct {
    int32_t  year;
    uint8_t  month;              /**< 1-12 */
    uint8_t  day;                /**< 1-31 */
    uint8_t  hour;               /**< 0-23 */
    uint8_t  minute;             /**< 0-59 */
    uint8_t  second;             /**< 0-59 */
    uint32_t nanosecond;
    int16_t  tz_offset_minutes;  /**< 0 for UTC, negative for west of UTC */
} ProvenDateTime;

/** @brief DateTime result. */
typedef struct {
    int32_t       status;
    ProvenDateTime datetime;
} ProvenDateTimeResult;

/**
 * @brief Parse ISO 8601 date string.
 *
 * Supports: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, YYYY-MM-DDTHH:MM:SSZ,
 * YYYY-MM-DDTHH:MM:SS+HH:MM
 */
ProvenDateTimeResult proven_datetime_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Format DateTime as ISO 8601 string.
 * @return Result with formatted string (caller must free).
 */
ProvenStringResult proven_datetime_format_iso8601(ProvenDateTime dt);

/** @brief Check if year is a leap year. */
bool proven_datetime_is_leap_year(int32_t year);

/** @brief Get number of days in a month. Returns 0 if invalid month. */
uint8_t proven_datetime_days_in_month(int32_t year, uint8_t month);

/* ============================================================================
 * SafeFloat - Safe floating-point operations (5 functions)
 * ============================================================================ */

/**
 * @brief Safe floating-point division.
 * @return PROVEN_ERR_DIVISION_BY_ZERO if b is 0, PROVEN_ERR_INVALID_ARGUMENT if NaN.
 */
ProvenFloatResult proven_float_div(double a, double b);

/** @brief Check if float is finite (not NaN or Inf). */
bool proven_float_is_finite(double x);

/** @brief Check if float is NaN. */
bool proven_float_is_nan(double x);

/**
 * @brief Safe square root.
 * @return PROVEN_ERR_INVALID_ARGUMENT if x is negative or NaN.
 */
ProvenFloatResult proven_float_sqrt(double x);

/**
 * @brief Safe natural logarithm.
 * @return PROVEN_ERR_INVALID_ARGUMENT if x <= 0 or NaN.
 */
ProvenFloatResult proven_float_ln(double x);

/* ============================================================================
 * SafePassword - Password validation (2 functions)
 * ============================================================================ */

/** @brief Password strength level. */
typedef enum {
    PROVEN_PASSWORD_VERY_WEAK  = 0,
    PROVEN_PASSWORD_WEAK       = 1,
    PROVEN_PASSWORD_FAIR       = 2,
    PROVEN_PASSWORD_STRONG     = 3,
    PROVEN_PASSWORD_VERY_STRONG = 4
} ProvenPasswordStrength;

/** @brief Password validation result. */
typedef struct {
    ProvenPasswordStrength strength;
    bool   has_lowercase;
    bool   has_uppercase;
    bool   has_digit;
    bool   has_special;
    size_t length;
} ProvenPasswordResult;

/**
 * @brief Validate password strength.
 * @return Password validation result with strength and character class flags.
 */
ProvenPasswordResult proven_password_validate(const uint8_t* ptr, size_t len);

/**
 * @brief Check if password is in common passwords list.
 * @return true if password appears in the common list.
 */
bool proven_password_is_common(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeHex - Hexadecimal encoding/decoding (3 functions)
 * ============================================================================ */

/** @brief Hex decode result with byte data. */
typedef struct {
    int32_t  status;
    uint8_t* data;
    size_t   length;
} ProvenHexDecodeResult;

/**
 * @brief Encode bytes to hex string.
 * @param ptr Input bytes.
 * @param len Number of bytes.
 * @param uppercase If true, use uppercase hex digits.
 * @return Result with hex string (caller must free).
 */
ProvenStringResult proven_hex_encode(const uint8_t* ptr, size_t len, bool uppercase);

/**
 * @brief Decode hex string to bytes.
 * @return Result with decoded bytes (caller must free with proven_hex_free).
 */
ProvenHexDecodeResult proven_hex_decode(const uint8_t* ptr, size_t len);

/**
 * @brief Free hex decode result allocated by proven_hex_decode().
 */
void proven_hex_free(ProvenHexDecodeResult* result);

/* ============================================================================
 * SafeCurrency - Monetary values with ISO 4217 codes (2 functions)
 * ============================================================================ */

/** @brief Currency parse result with minor units. */
typedef struct {
    int32_t  status;
    int64_t  amount_minor;     /**< Amount in minor units (cents, etc.) */
    uint8_t  currency_code[3]; /**< 3-letter ISO 4217 code */
    uint8_t  decimal_places;
} ProvenCurrencyResult;

/**
 * @brief Parse currency amount (e.g., "USD 123.45" or "123.45 EUR").
 * @return Result with parsed currency.
 */
ProvenCurrencyResult proven_currency_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Format currency amount.
 * @param amount_minor Amount in minor units.
 * @param code 3-letter ISO 4217 currency code.
 * @param decimal_places Number of decimal places for the currency.
 * @return Result with formatted string (caller must free).
 */
ProvenStringResult proven_currency_format(int64_t amount_minor, uint8_t code[3], uint8_t decimal_places);

/* ============================================================================
 * SafePhone - E.164 phone number handling (2 functions)
 * ============================================================================ */

/** @brief Phone number parse result. */
typedef struct {
    int32_t  status;
    uint16_t country_code;
    uint64_t national_number;
    bool     is_valid;
} ProvenPhoneResult;

/**
 * @brief Parse phone number to E.164 format.
 * @return Result with parsed phone number.
 */
ProvenPhoneResult proven_phone_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Format phone number as E.164 string.
 * @param country_code Numeric country calling code.
 * @param national_number National number digits.
 * @return Result with formatted string (caller must free).
 */
ProvenStringResult proven_phone_format_e164(uint16_t country_code, uint64_t national_number);

/* ============================================================================
 * SafeVersion - Semantic versioning (3 functions)
 * ============================================================================ */

/** @brief Semantic version structure. */
typedef struct {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;
    size_t   prerelease_len;
    char*    prerelease;  /**< Pre-release string (may be NULL) */
} ProvenSemanticVersion;

/** @brief Version parsing result. */
typedef struct {
    int32_t              status;
    ProvenSemanticVersion version;
} ProvenVersionResult;

/**
 * @brief Parse semantic version string (e.g., "1.2.3-alpha").
 *
 * Supports optional 'v' or 'V' prefix. Caller must free prerelease string
 * with proven_version_free() if present.
 */
ProvenVersionResult proven_version_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Compare two semantic versions.
 * @return Negative if a < b, 0 if equal, positive if a > b.
 */
int32_t proven_version_compare(ProvenSemanticVersion a, ProvenSemanticVersion b);

/**
 * @brief Free version result resources (prerelease string).
 */
void proven_version_free(ProvenSemanticVersion* version);

/* ============================================================================
 * SafeGeo - Geographic coordinate operations (3 functions)
 * ============================================================================ */

/** @brief Geographic coordinate. */
typedef struct {
    double latitude;   /**< -90 to 90 */
    double longitude;  /**< -180 to 180 */
} ProvenGeoCoordinate;

/** @brief Geo validation result. */
typedef struct {
    int32_t            status;
    ProvenGeoCoordinate coordinate;
} ProvenGeoResult;

/**
 * @brief Validate and normalize geographic coordinate.
 * @return Result with validated coordinate.
 */
ProvenGeoResult proven_geo_validate(double lat, double lon);

/**
 * @brief Calculate distance between two points (Haversine formula).
 * @return Result with distance in meters.
 */
ProvenFloatResult proven_geo_distance(ProvenGeoCoordinate a, ProvenGeoCoordinate b);

/**
 * @brief Check if coordinate is inside a bounding box.
 * @return true if coordinate is within bounds.
 */
bool proven_geo_in_bounds(
    ProvenGeoCoordinate coord,
    double min_lat, double max_lat,
    double min_lon, double max_lon
);

/* ============================================================================
 * SafeChecksum - CRC and hash verification (2 functions)
 * ============================================================================ */

/**
 * @brief Calculate CRC32 checksum.
 * @return Result with CRC32 value in the int64 field.
 */
ProvenIntResult proven_checksum_crc32(const uint8_t* ptr, size_t len);

/**
 * @brief Verify CRC32 matches expected value.
 * @return Result with verification status.
 */
ProvenBoolResult proven_checksum_verify_crc32(const uint8_t* ptr, size_t len, uint32_t expected);

/* ============================================================================
 * SafeProbability - Probability values clamped to [0, 1] (4 functions)
 * ============================================================================ */

/**
 * @brief Create probability value (clamped to [0, 1]).
 * @return Clamped probability.
 */
double proven_probability_create(double value);

/**
 * @brief Multiply probabilities (independent events: P(A and B) = P(A) * P(B)).
 * @return Product probability.
 */
double proven_probability_and(double a, double b);

/**
 * @brief Add probabilities (mutually exclusive events: P(A or B) = P(A) + P(B)).
 * @return Sum probability (clamped to 1.0).
 */
double proven_probability_or_exclusive(double a, double b);

/**
 * @brief Complement probability (P(not A) = 1 - P(A)).
 * @return Complement probability.
 */
double proven_probability_not(double p);

/* ============================================================================
 * SafeCalculator - Safe expression evaluation (1 function)
 * ============================================================================ */

/**
 * @brief Evaluate an arithmetic expression safely.
 *
 * Supports +, -, *, /, parentheses, negative and decimal numbers.
 * Returns PROVEN_ERR_PARSE_FAILURE for invalid expressions.
 * Returns PROVEN_ERR_DIVISION_BY_ZERO for division by zero.
 */
ProvenFloatResult proven_calculator_eval(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeBuffer - Bounded buffer operations (4 functions)
 * ============================================================================ */

/** @brief Opaque bounded buffer type. */
typedef struct {
    uint8_t* data;
    size_t   capacity;
    size_t   length;
} ProvenBoundedBuffer;

/** @brief Buffer creation result. */
typedef struct {
    int32_t              status;
    ProvenBoundedBuffer* buffer;
} ProvenBufferResult;

/**
 * @brief Create a bounded buffer.
 * @param capacity Maximum capacity in bytes (max 100MB).
 * @return Result with buffer pointer.
 */
ProvenBufferResult proven_buffer_create(size_t capacity);

/**
 * @brief Append data to buffer with bounds checking.
 * @return PROVEN_ERR_OUT_OF_BOUNDS if data would exceed capacity.
 */
ProvenStatus proven_buffer_append(ProvenBoundedBuffer* buffer, const uint8_t* ptr, size_t len);

/**
 * @brief Get buffer contents.
 * @param[out] out_ptr Receives pointer to buffer data.
 * @param[out] out_len Receives length of buffer data.
 * @return Status code.
 */
ProvenStatus proven_buffer_get(ProvenBoundedBuffer* buffer, const uint8_t** out_ptr, size_t* out_len);

/**
 * @brief Free buffer.
 * @param buffer Buffer to free (may be NULL).
 */
void proven_buffer_free(ProvenBoundedBuffer* buffer);

/* ============================================================================
 * SafeRateLimiter - Token bucket rate limiting (3 functions)
 * ============================================================================ */

/** @brief Rate limiter state. */
typedef struct {
    double  tokens;
    double  capacity;
    double  refill_rate;   /**< Tokens per second */
    int64_t last_refill;   /**< Timestamp in milliseconds */
} ProvenRateLimiter;

/**
 * @brief Create a rate limiter.
 * @param capacity Maximum token capacity.
 * @param refill_rate Tokens per second to refill.
 * @return Rate limiter pointer, or NULL on failure.
 */
ProvenRateLimiter* proven_rate_limiter_create(double capacity, double refill_rate);

/**
 * @brief Try to acquire tokens from the rate limiter.
 *
 * Automatically refills based on elapsed time before checking.
 * @return true if tokens were acquired, false if insufficient tokens.
 */
bool proven_rate_limiter_try_acquire(ProvenRateLimiter* limiter, double tokens);

/**
 * @brief Free rate limiter.
 * @param limiter Rate limiter to free (may be NULL).
 */
void proven_rate_limiter_free(ProvenRateLimiter* limiter);

/* ============================================================================
 * SafeCircuitBreaker - Fault tolerance pattern (6 functions)
 * ============================================================================ */

/** @brief Circuit breaker state. */
typedef enum {
    PROVEN_CIRCUIT_CLOSED    = 0,  /**< Normal operation, requests allowed */
    PROVEN_CIRCUIT_OPEN      = 1,  /**< Failing, requests rejected */
    PROVEN_CIRCUIT_HALF_OPEN = 2   /**< Testing recovery */
} ProvenCircuitState;

/** @brief Circuit breaker structure. */
typedef struct {
    ProvenCircuitState state;
    uint32_t failure_count;
    uint32_t failure_threshold;
    uint32_t success_count;
    uint32_t success_threshold;
    int64_t  last_failure;      /**< Timestamp of last failure (milliseconds) */
    int64_t  timeout_ms;        /**< Time before attempting recovery */
} ProvenCircuitBreaker;

/**
 * @brief Create a circuit breaker.
 * @param failure_threshold Failures before opening.
 * @param success_threshold Successes in half-open to close.
 * @param timeout_ms Time before trying half-open recovery.
 * @return Circuit breaker pointer, or NULL on failure.
 */
ProvenCircuitBreaker* proven_circuit_breaker_create(
    uint32_t failure_threshold,
    uint32_t success_threshold,
    int64_t timeout_ms
);

/**
 * @brief Check if a request should be allowed.
 * @return true if request is allowed, false if circuit is open.
 */
bool proven_circuit_breaker_allow(ProvenCircuitBreaker* cb);

/** @brief Record a successful operation. */
void proven_circuit_breaker_success(ProvenCircuitBreaker* cb);

/** @brief Record a failed operation. */
void proven_circuit_breaker_failure(ProvenCircuitBreaker* cb);

/**
 * @brief Get current circuit state.
 * @return Current state (returns OPEN if cb is NULL).
 */
ProvenCircuitState proven_circuit_breaker_state(ProvenCircuitBreaker* cb);

/** @brief Free circuit breaker (may be NULL). */
void proven_circuit_breaker_free(ProvenCircuitBreaker* cb);

/* ============================================================================
 * SafeRetry - Exponential backoff (2 functions)
 * ============================================================================ */

/** @brief Retry configuration. */
typedef struct {
    uint32_t max_attempts;
    uint64_t base_delay_ms;
    uint64_t max_delay_ms;
    double   multiplier;   /**< Exponential multiplier (e.g., 2.0) */
} ProvenRetryConfig;

/**
 * @brief Calculate delay for a given attempt.
 * @param config Retry configuration.
 * @param attempt Current attempt number (1-based).
 * @return Delay in milliseconds (with +/-25% jitter). 0 if attempt exceeds max.
 */
uint64_t proven_retry_delay(ProvenRetryConfig config, uint32_t attempt);

/**
 * @brief Check if retry should be attempted.
 * @return true if more attempts are allowed.
 */
bool proven_retry_should_retry(ProvenRetryConfig config, uint32_t attempt);

/* ============================================================================
 * SafeMonotonic - Monotonically increasing sequences (3 functions)
 * ============================================================================ */

/** @brief Monotonic counter structure. */
typedef struct {
    uint64_t value;
    uint64_t max_value;
} ProvenMonotonicCounter;

/**
 * @brief Create a monotonic counter.
 * @param initial Initial value.
 * @param max_value Maximum allowed value.
 * @return Counter pointer, or NULL if initial >= max_value.
 */
ProvenMonotonicCounter* proven_monotonic_create(uint64_t initial, uint64_t max_value);

/**
 * @brief Get next value and increment.
 * @return PROVEN_ERR_OVERFLOW if counter has reached max_value.
 */
ProvenIntResult proven_monotonic_next(ProvenMonotonicCounter* counter);

/** @brief Free monotonic counter (may be NULL). */
void proven_monotonic_free(ProvenMonotonicCounter* counter);

/* ============================================================================
 * SafeStateMachine - Type-safe state transitions (5 functions)
 * ============================================================================ */

/** @brief State machine structure. */
typedef struct {
    uint32_t current_state;
    uint32_t state_count;
    uint8_t* transitions;  /**< Packed transition matrix (state_count x state_count) */
} ProvenStateMachine;

/**
 * @brief Create a state machine.
 * @param state_count Total number of states (max 256).
 * @param initial_state Initial state index.
 * @return State machine pointer, or NULL on failure.
 */
ProvenStateMachine* proven_state_machine_create(uint32_t state_count, uint32_t initial_state);

/**
 * @brief Allow a state transition.
 * @return true if transition was allowed, false on error.
 */
bool proven_state_machine_allow(ProvenStateMachine* sm, uint32_t from, uint32_t to);

/**
 * @brief Attempt to transition to a new state.
 * @return true if transition succeeded, false if not allowed.
 */
bool proven_state_machine_transition(ProvenStateMachine* sm, uint32_t to);

/**
 * @brief Get current state index.
 * @return Current state (0 if sm is NULL).
 */
uint32_t proven_state_machine_state(ProvenStateMachine* sm);

/** @brief Free state machine (may be NULL). */
void proven_state_machine_free(ProvenStateMachine* sm);

/* ============================================================================
 * SafeTensor - Basic 2D tensor operations (5 functions)
 * ============================================================================ */

/** @brief 2D tensor (matrix). */
typedef struct {
    double* data;
    size_t  rows;
    size_t  cols;
} ProvenTensor2D;

/**
 * @brief Create a 2D tensor initialized to zero.
 * @return Tensor pointer, or NULL on failure.
 */
ProvenTensor2D* proven_tensor_create(size_t rows, size_t cols);

/**
 * @brief Set tensor value at (row, col).
 * @return PROVEN_ERR_OUT_OF_BOUNDS if indices are invalid.
 */
ProvenStatus proven_tensor_set(ProvenTensor2D* tensor, size_t row, size_t col, double value);

/**
 * @brief Get tensor value at (row, col).
 * @return Result with value; PROVEN_ERR_OUT_OF_BOUNDS if invalid.
 */
ProvenFloatResult proven_tensor_get(ProvenTensor2D* tensor, size_t row, size_t col);

/**
 * @brief Matrix multiplication (a * b).
 * @return New tensor, or NULL if dimensions mismatch.
 */
ProvenTensor2D* proven_tensor_matmul(ProvenTensor2D* a, ProvenTensor2D* b);

/** @brief Free tensor (may be NULL). */
void proven_tensor_free(ProvenTensor2D* tensor);

/* ============================================================================
 * SafeMl - Machine learning activation functions (5 functions)
 * ============================================================================ */

/**
 * @brief Softmax normalization over an array.
 * @param input Input array of doubles.
 * @param output Output array (same size as input).
 * @param len Length of arrays.
 * @return Status code.
 */
ProvenStatus proven_ml_softmax(const double* input, double* output, size_t len);

/** @brief Sigmoid function: 1 / (1 + exp(-x)). */
double proven_ml_sigmoid(double x);

/** @brief ReLU function: max(0, x). */
double proven_ml_relu(double x);

/** @brief Leaky ReLU: x >= 0 ? x : alpha * x. */
double proven_ml_leaky_relu(double x, double alpha);

/** @brief Clamp value to [min_val, max_val]. */
double proven_ml_clamp(double x, double min_val, double max_val);

/* ============================================================================
 * SafeLru - Least Recently Used cache (4 functions)
 * ============================================================================ */

/** @brief LRU cache entry. */
typedef struct {
    uint64_t key;
    int64_t  value;
    size_t   prev;
    size_t   next;
    bool     valid;
} ProvenLRUEntry;

/** @brief LRU cache structure. */
typedef struct {
    ProvenLRUEntry* entries;
    size_t          capacity;
    size_t          head;
    size_t          tail;
    size_t          count;
} ProvenLRUCache;

/**
 * @brief Create an LRU cache.
 * @param capacity Maximum entries (max 100,000).
 * @return Cache pointer, or NULL on failure.
 */
ProvenLRUCache* proven_lru_create(size_t capacity);

/**
 * @brief Get value from cache (promotes to most recently used).
 * @return PROVEN_ERR_OUT_OF_BOUNDS if key not found.
 */
ProvenIntResult proven_lru_get(ProvenLRUCache* cache, uint64_t key);

/**
 * @brief Put value in cache. Evicts LRU entry if full.
 * @return Status code.
 */
ProvenStatus proven_lru_put(ProvenLRUCache* cache, uint64_t key, int64_t value);

/** @brief Free LRU cache (may be NULL). */
void proven_lru_free(ProvenLRUCache* cache);

/* ============================================================================
 * SafeGraph - Directed graph with adjacency matrix (4 functions)
 * ============================================================================ */

/** @brief Graph structure (adjacency matrix). */
typedef struct {
    uint8_t* edges;      /**< Packed bit matrix (1 bit per edge) */
    size_t   node_count;
} ProvenGraph;

/**
 * @brief Create a directed graph.
 * @param node_count Number of nodes (max 10,000).
 * @return Graph pointer, or NULL on failure.
 */
ProvenGraph* proven_graph_create(size_t node_count);

/**
 * @brief Add directed edge from -> to.
 * @return PROVEN_ERR_OUT_OF_BOUNDS if node indices are invalid.
 */
ProvenStatus proven_graph_add_edge(ProvenGraph* graph, size_t from, size_t to);

/**
 * @brief Check if directed edge exists.
 * @return true if edge exists, false otherwise (or on error).
 */
bool proven_graph_has_edge(ProvenGraph* graph, size_t from, size_t to);

/** @brief Free graph (may be NULL). */
void proven_graph_free(ProvenGraph* graph);

/* ============================================================================
 * SafeRegistry - OCI image reference parsing (3 functions)
 * ============================================================================ */

/** @brief Parsed OCI image reference. */
typedef struct {
    char*  registry;      size_t registry_len;
    char*  repository;    size_t repository_len;
    char*  tag;           size_t tag_len;
    char*  digest;        size_t digest_len;
} ProvenImageReference;

/** @brief Image reference parse result. */
typedef struct {
    int32_t              status;
    ProvenImageReference reference;
} ProvenImageRefResult;

/**
 * @brief Parse OCI image reference (e.g., "ghcr.io/user/repo:v1.0").
 *
 * Format: [registry/]repository[:tag][@digest]
 * Handles Docker Hub conventions (library/ prefix, docker.io default).
 */
ProvenImageRefResult proven_registry_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Convert image reference back to string.
 * @return Result with string (caller must free).
 */
ProvenStringResult proven_registry_to_string(const ProvenImageReference* ref);

/**
 * @brief Check if image reference has an explicit registry hostname.
 * @return Result with detection status.
 */
ProvenBoolResult proven_registry_has_registry(const ProvenImageReference* ref);

/* ============================================================================
 * SafeDigest - Cryptographic digest operations (3 functions)
 * ============================================================================ */

/** @brief Hash algorithm types. */
typedef enum {
    PROVEN_HASH_SHA256 = 0,
    PROVEN_HASH_SHA384 = 1,
    PROVEN_HASH_SHA512 = 2,
    PROVEN_HASH_BLAKE3 = 3
} ProvenHashAlgorithm;

/** @brief Digest with algorithm and hex value. */
typedef struct {
    ProvenHashAlgorithm algorithm;
    char*               value;
    size_t              value_len;
} ProvenDigest;

/** @brief Digest parse result. */
typedef struct {
    int32_t      status;
    ProvenDigest digest;
} ProvenDigestResult;

/**
 * @brief Parse digest string (e.g., "sha256:abc123...").
 * @return Result with parsed digest.
 */
ProvenDigestResult proven_digest_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Constant-time digest comparison (timing-attack resistant).
 * @return Result with verification status.
 */
ProvenBoolResult proven_digest_verify(const ProvenDigest* expected, const ProvenDigest* actual);

/**
 * @brief Convert digest to string (algorithm:hex).
 * @return Result with string (caller must free).
 */
ProvenStringResult proven_digest_to_string(const ProvenDigest* digest);

/* ============================================================================
 * SafeHttp - HTTP URL encoding and header parsing (3 functions)
 * ============================================================================ */

/**
 * @brief URL-encode a string (RFC 3986 percent encoding).
 *
 * Unreserved chars (A-Za-z0-9-._~) pass through; others become %XX.
 * @return Result with encoded string (caller must free).
 */
ProvenStringResult proven_http_url_encode(const uint8_t* ptr, size_t len);

/**
 * @brief URL-decode a percent-encoded string.
 * @return Result with decoded string (caller must free).
 */
ProvenStringResult proven_http_url_decode(const uint8_t* ptr, size_t len);

/** @brief WWW-Authenticate challenge components. */
typedef struct {
    char*  scheme;   size_t scheme_len;
    char*  realm;    size_t realm_len;
    char*  service;  size_t service_len;
    char*  scope;    size_t scope_len;
} ProvenAuthChallenge;

/** @brief Auth challenge parse result. */
typedef struct {
    int32_t             status;
    ProvenAuthChallenge challenge;
} ProvenAuthChallengeResult;

/**
 * @brief Parse WWW-Authenticate header (for Docker Registry v2 OAuth2).
 *
 * Format: Bearer realm="...",service="...",scope="..."
 */
ProvenAuthChallengeResult proven_http_parse_www_authenticate(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeColor - Color space conversions (3 functions)
 * ============================================================================ */

/** @brief RGB color (8-bit per channel). */
typedef struct { uint8_t r; uint8_t g; uint8_t b; } ProvenRGBColor;

/** @brief HSL color. */
typedef struct {
    double h;  /**< Hue: 0-360 degrees */
    double s;  /**< Saturation: 0-1 */
    double l;  /**< Lightness: 0-1 */
} ProvenHSLColor;

/** @brief Color parse result. */
typedef struct {
    int32_t       status;
    ProvenRGBColor color;
} ProvenColorResult;

/**
 * @brief Parse hex color string (#RRGGBB or #RGB).
 *
 * Leading # is optional.
 */
ProvenColorResult proven_color_parse_hex(const uint8_t* ptr, size_t len);

/** @brief Convert RGB to HSL. */
ProvenHSLColor proven_color_rgb_to_hsl(ProvenRGBColor rgb);

/**
 * @brief Format RGB as hex string ("#rrggbb", lowercase).
 * @return Result with hex string (caller must free).
 */
ProvenStringResult proven_color_to_hex(ProvenRGBColor rgb);

/* ============================================================================
 * SafeAngle - Angle conversions and normalization (4 functions)
 * ============================================================================ */

/** @brief Convert degrees to radians. */
double proven_angle_deg_to_rad(double degrees);

/** @brief Convert radians to degrees. */
double proven_angle_rad_to_deg(double radians);

/** @brief Normalize angle to [0, 360) degrees. Returns 0 for NaN. */
double proven_angle_normalize_degrees(double degrees);

/** @brief Normalize angle to [0, 2*pi) radians. Returns 0 for NaN. */
double proven_angle_normalize_radians(double radians);

/* ============================================================================
 * SafeUnit - Physical unit conversions (2 functions)
 * ============================================================================ */

/** @brief Length units. */
typedef enum {
    PROVEN_LENGTH_METERS      = 0,
    PROVEN_LENGTH_KILOMETERS  = 1,
    PROVEN_LENGTH_CENTIMETERS = 2,
    PROVEN_LENGTH_MILLIMETERS = 3,
    PROVEN_LENGTH_FEET        = 4,
    PROVEN_LENGTH_INCHES      = 5,
    PROVEN_LENGTH_MILES       = 6,
    PROVEN_LENGTH_YARDS       = 7
} ProvenLengthUnit;

/** @brief Temperature units. */
typedef enum {
    PROVEN_TEMP_CELSIUS    = 0,
    PROVEN_TEMP_FAHRENHEIT = 1,
    PROVEN_TEMP_KELVIN     = 2
} ProvenTempUnit;

/**
 * @brief Convert length between units.
 * @return PROVEN_ERR_INVALID_ARGUMENT for NaN input.
 */
ProvenFloatResult proven_unit_convert_length(double value, ProvenLengthUnit from, ProvenLengthUnit to);

/**
 * @brief Convert temperature between units.
 * @return PROVEN_ERR_OUT_OF_BOUNDS for temperatures below absolute zero.
 */
ProvenFloatResult proven_unit_convert_temp(double value, ProvenTempUnit from, ProvenTempUnit to);

/* ============================================================================
 * SafeQueue - Bounded FIFO queue (5 functions)
 * ============================================================================ */

/** @brief Bounded FIFO queue. */
typedef struct {
    int64_t* data;
    size_t   capacity;
    size_t   head;
    size_t   tail;
    size_t   count;
} ProvenBoundedQueue;

/**
 * @brief Create a bounded queue.
 * @param capacity Maximum elements (max 1,000,000).
 * @return Queue pointer, or NULL on failure.
 */
ProvenBoundedQueue* proven_queue_create(size_t capacity);

/**
 * @brief Push value to queue.
 * @return true if successful, false if full or NULL.
 */
bool proven_queue_push(ProvenBoundedQueue* queue, int64_t value);

/**
 * @brief Pop value from queue (FIFO order).
 * @return PROVEN_ERR_OUT_OF_BOUNDS if empty.
 */
ProvenIntResult proven_queue_pop(ProvenBoundedQueue* queue);

/**
 * @brief Get queue element count.
 * @return Number of elements (0 if NULL).
 */
size_t proven_queue_size(ProvenBoundedQueue* queue);

/** @brief Free queue (may be NULL). */
void proven_queue_free(ProvenBoundedQueue* queue);

/* ============================================================================
 * SafeBloom - Bloom filter (4 functions)
 * ============================================================================ */

/** @brief Bloom filter structure. */
typedef struct {
    uint8_t* bits;
    size_t   bit_count;
    uint32_t hash_count;
} ProvenBloomFilter;

/**
 * @brief Create a Bloom filter.
 * @param expected_elements Expected number of elements.
 * @param false_positive_rate Desired false positive rate (0 < rate < 1).
 * @return Filter pointer, or NULL on failure.
 */
ProvenBloomFilter* proven_bloom_create(size_t expected_elements, double false_positive_rate);

/**
 * @brief Add element to Bloom filter.
 * @param filter Bloom filter (ignored if NULL).
 * @param ptr Element data.
 * @param len Length of element data.
 */
void proven_bloom_add(ProvenBloomFilter* filter, const uint8_t* ptr, size_t len);

/**
 * @brief Check if element might be in filter.
 * @return true if probably present (may be false positive), false if definitely absent.
 */
bool proven_bloom_contains(ProvenBloomFilter* filter, const uint8_t* ptr, size_t len);

/** @brief Free Bloom filter (may be NULL). */
void proven_bloom_free(ProvenBloomFilter* filter);

/* ============================================================================
 * Callbacks - Bidirectional FFI event system (6 functions)
 * ============================================================================ */

/** @brief Event types that can trigger callbacks. */
typedef enum {
    PROVEN_EVENT_VALIDATION_FAILED   = 1,
    PROVEN_EVENT_RESOURCE_ACQUIRED   = 2,
    PROVEN_EVENT_RESOURCE_RELEASED   = 3,
    PROVEN_EVENT_RATE_LIMIT_HIT      = 4,
    PROVEN_EVENT_CIRCUIT_STATE_CHANGE = 5,
    PROVEN_EVENT_SIGNAL_RECEIVED     = 6,
    PROVEN_EVENT_RETRY_ATTEMPT       = 7,
    PROVEN_EVENT_CRYPTO_OPERATION    = 8,
    PROVEN_EVENT_CUSTOM              = 100
} ProvenEventType;

/** @brief Event data passed to callbacks. */
typedef struct {
    ProvenEventType event_type;
    const char*     data_ptr;    /**< Optional null-terminated payload (may be NULL) */
    size_t          data_len;
    uint64_t        timestamp_us; /**< Monotonic microseconds since epoch */
    int32_t         code;         /**< Event-specific integer (e.g., signal number) */
} ProvenEvent;

/**
 * @brief Callback function signature.
 *
 * @param context User-provided context pointer (from registration).
 * @param event Event data.
 * @return 0 on success; non-zero to request automatic deregistration.
 */
typedef int32_t (*ProvenCallbackFn)(void* context, const ProvenEvent* event);

/** @brief Opaque handle for a registered callback. 0 = invalid. */
typedef uint32_t ProvenCallbackHandle;

/**
 * @brief Register a callback for a specific event type.
 * @param event_type Event type to listen for.
 * @param callback Function to invoke.
 * @param context User context passed to callback (may be NULL).
 * @return Non-zero handle on success, 0 on failure.
 */
ProvenCallbackHandle proven_callback_register(
    ProvenEventType event_type,
    ProvenCallbackFn callback,
    void* context
);

/**
 * @brief Unregister a callback by handle.
 * @return PROVEN_OK on success, error code if handle invalid.
 */
ProvenStatus proven_callback_unregister(ProvenCallbackHandle handle);

/**
 * @brief Fire an event, invoking all registered callbacks for that type.
 * @param event_type Event type to fire.
 * @param data_ptr Optional null-terminated data payload (may be NULL).
 * @param data_len Length of data payload.
 * @param code Event-specific integer code.
 * @return Number of callbacks invoked.
 */
int32_t proven_callback_fire(
    ProvenEventType event_type,
    const char* data_ptr, size_t data_len,
    int32_t code
);

/**
 * @brief Query how many callbacks are registered for an event type.
 * @return Number of active callbacks for the event type.
 */
uint32_t proven_callback_count(ProvenEventType event_type);

/**
 * @brief Unregister all callbacks.
 * @return Number of callbacks that were removed.
 */
uint32_t proven_callback_clear_all(void);

#ifdef __cplusplus
}
#endif

#endif /* PROVEN_H */
