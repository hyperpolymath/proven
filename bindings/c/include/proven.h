/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file proven.h
 * @brief Proven FFI - Safe operations from Zig with C ABI
 *
 * This header provides C declarations for the Proven library, which is
 * implemented in Zig and exposes a stable C ABI. Link against libproven.so
 * or libproven.a to use these functions.
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
 *     if (result.status != PROVEN_OK) {
 *         printf("Division by zero handled safely\n");
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
 * Status Codes
 * ============================================================================ */

/**
 * @brief Status codes returned by Proven operations
 */
typedef enum ProvenStatus {
    PROVEN_OK = 0,
    PROVEN_ERR_NULL_POINTER = -1,
    PROVEN_ERR_INVALID_ARGUMENT = -2,
    PROVEN_ERR_OVERFLOW = -3,
    PROVEN_ERR_UNDERFLOW = -4,
    PROVEN_ERR_DIVISION_BY_ZERO = -5,
    PROVEN_ERR_PARSE_FAILURE = -6,
    PROVEN_ERR_VALIDATION_FAILED = -7,
    PROVEN_ERR_OUT_OF_BOUNDS = -8,
    PROVEN_ERR_ENCODING_ERROR = -9,
    PROVEN_ERR_ALLOCATION_FAILED = -10,
    PROVEN_ERR_NOT_IMPLEMENTED = -99
} ProvenStatus;

/* ============================================================================
 * Result Types
 * ============================================================================ */

/**
 * @brief Result for integer operations
 */
typedef struct ProvenIntResult {
    ProvenStatus status;
    int64_t value;
} ProvenIntResult;

/**
 * @brief Result for boolean operations
 */
typedef struct ProvenBoolResult {
    ProvenStatus status;
    bool value;
} ProvenBoolResult;

/**
 * @brief Result for string operations
 * @note Caller must free the value using proven_free_string()
 */
typedef struct ProvenStringResult {
    ProvenStatus status;
    char* value;
    size_t length;
} ProvenStringResult;

/**
 * @brief IPv4 address structure
 */
typedef struct ProvenIPv4Address {
    uint8_t octets[4];
} ProvenIPv4Address;

/**
 * @brief Result for IPv4 parsing
 */
typedef struct ProvenIPv4Result {
    ProvenStatus status;
    ProvenIPv4Address address;
} ProvenIPv4Result;

/**
 * @brief URL components structure
 */
typedef struct ProvenUrlComponents {
    char* scheme;
    size_t scheme_len;
    char* host;
    size_t host_len;
    uint16_t port;
    bool has_port;
    char* path;
    size_t path_len;
    char* query;
    size_t query_len;
    char* fragment;
    size_t fragment_len;
} ProvenUrlComponents;

/**
 * @brief Result for URL parsing
 */
typedef struct ProvenUrlResult {
    ProvenStatus status;
    ProvenUrlComponents components;
} ProvenUrlResult;

/* ============================================================================
 * Runtime Management
 * ============================================================================ */

/**
 * @brief Initialize the Proven runtime
 * @return PROVEN_OK on success, error code otherwise
 *
 * Call this before using any other Proven functions.
 */
int32_t proven_init(void);

/**
 * @brief Cleanup the Proven runtime
 *
 * Call this when done using Proven functions.
 */
void proven_deinit(void);

/**
 * @brief Check if runtime is initialized
 * @return true if initialized
 */
bool proven_is_initialized(void);

/**
 * @brief Get FFI ABI version for compatibility checking
 * @return ABI version number
 */
uint32_t proven_ffi_abi_version(void);

/* ============================================================================
 * Memory Management
 * ============================================================================ */

/**
 * @brief Free a string allocated by Proven functions
 * @param ptr String to free (may be NULL)
 */
void proven_free_string(char* ptr);

/**
 * @brief Free URL components
 * @param components Pointer to URL components to free
 */
void proven_url_free(ProvenUrlComponents* components);

/* ============================================================================
 * SafeMath - Arithmetic operations that cannot crash
 * ============================================================================ */

/**
 * @brief Safe integer division
 * @param numerator Dividend
 * @param denominator Divisor
 * @return Result with status and quotient
 *
 * Returns PROVEN_ERR_DIVISION_BY_ZERO if denominator is 0.
 * Returns PROVEN_ERR_OVERFLOW for MIN_INT / -1.
 */
ProvenIntResult proven_math_div(int64_t numerator, int64_t denominator);

/**
 * @brief Safe modulo operation
 * @param numerator Dividend
 * @param denominator Divisor
 * @return Result with status and remainder
 *
 * Returns PROVEN_ERR_DIVISION_BY_ZERO if denominator is 0.
 */
ProvenIntResult proven_math_mod(int64_t numerator, int64_t denominator);

/**
 * @brief Checked addition with overflow detection
 * @param a First operand
 * @param b Second operand
 * @return Result with status and sum
 *
 * Returns PROVEN_ERR_OVERFLOW if result would overflow.
 */
ProvenIntResult proven_math_add_checked(int64_t a, int64_t b);

/**
 * @brief Checked subtraction with underflow detection
 * @param a First operand
 * @param b Second operand
 * @return Result with status and difference
 *
 * Returns PROVEN_ERR_UNDERFLOW if result would underflow.
 */
ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b);

/**
 * @brief Checked multiplication with overflow detection
 * @param a First operand
 * @param b Second operand
 * @return Result with status and product
 *
 * Returns PROVEN_ERR_OVERFLOW if result would overflow.
 */
ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b);

/**
 * @brief Safe absolute value
 * @param n Integer value
 * @return Result with status and absolute value
 *
 * Returns PROVEN_ERR_OVERFLOW for MIN_INT (cannot be represented).
 */
ProvenIntResult proven_math_abs_safe(int64_t n);

/**
 * @brief Clamp value to range
 * @param lo Lower bound (inclusive)
 * @param hi Upper bound (inclusive)
 * @param value Value to clamp
 * @return Clamped value
 */
int64_t proven_math_clamp(int64_t lo, int64_t hi, int64_t value);

/**
 * @brief Integer exponentiation with overflow checking
 * @param base Base value
 * @param exp Exponent (must be non-negative)
 * @return Result with status and power
 *
 * Returns PROVEN_ERR_OVERFLOW if result would overflow.
 */
ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp);

/* ============================================================================
 * SafeString - Text operations that handle encoding safely
 * ============================================================================ */

/**
 * @brief Check if bytes are valid UTF-8
 * @param ptr Pointer to byte data
 * @param len Length of data
 * @return Result with validation status
 */
ProvenBoolResult proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for SQL (single quotes)
 * @param ptr Pointer to string data
 * @param len Length of string
 * @return Result with escaped string (caller must free)
 *
 * @note Prefer parameterized queries over string escaping!
 */
ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for HTML
 * @param ptr Pointer to string data
 * @param len Length of string
 * @return Result with escaped string (caller must free)
 *
 * Escapes < > & " ' characters to prevent XSS.
 */
ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for JavaScript string literals
 * @param ptr Pointer to string data
 * @param len Length of string
 * @return Result with escaped string (caller must free)
 */
ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafePath - Filesystem operations that prevent traversal attacks
 * ============================================================================ */

/**
 * @brief Check if path contains directory traversal sequences
 * @param ptr Pointer to path string
 * @param len Length of path
 * @return Result with traversal detection status
 */
ProvenBoolResult proven_path_has_traversal(const uint8_t* ptr, size_t len);

/**
 * @brief Sanitize a filename by removing dangerous characters
 * @param ptr Pointer to filename
 * @param len Length of filename
 * @return Result with sanitized filename (caller must free)
 */
ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeCrypto - Cryptographic primitives
 * ============================================================================ */

/**
 * @brief Constant-time byte comparison (timing-safe)
 * @param ptr1 First buffer
 * @param len1 Length of first buffer
 * @param ptr2 Second buffer
 * @param len2 Length of second buffer
 * @return Result with comparison status
 *
 * Use this for comparing secrets to prevent timing attacks.
 */
ProvenBoolResult proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2
);

/**
 * @brief Fill buffer with cryptographically secure random bytes
 * @param ptr Buffer to fill
 * @param len Number of bytes to generate
 * @return Status code
 */
ProvenStatus proven_crypto_random_bytes(uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeUrl - URL parsing and validation
 * ============================================================================ */

/**
 * @brief Parse a URL into components
 * @param ptr Pointer to URL string
 * @param len Length of URL
 * @return Result with URL components (caller must free with proven_url_free)
 */
ProvenUrlResult proven_url_parse(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeEmail - Email validation
 * ============================================================================ */

/**
 * @brief Validate email address (RFC 5321 simplified)
 * @param ptr Pointer to email string
 * @param len Length of email
 * @return Result with validation status
 */
ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeNetwork - IP address parsing
 * ============================================================================ */

/**
 * @brief Parse IPv4 address string
 * @param ptr Pointer to address string
 * @param len Length of string
 * @return Result with parsed address
 */
ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);

/**
 * @brief Check if IPv4 address is private (RFC 1918)
 * @param addr IPv4 address
 * @return true if private
 */
bool proven_network_ipv4_is_private(ProvenIPv4Address addr);

/**
 * @brief Check if IPv4 address is loopback (127.0.0.0/8)
 * @param addr IPv4 address
 * @return true if loopback
 */
bool proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

/* ============================================================================
 * Version Information
 * ============================================================================ */

/**
 * @brief Get major version number
 * @return Major version
 */
uint32_t proven_version_major(void);

/**
 * @brief Get minor version number
 * @return Minor version
 */
uint32_t proven_version_minor(void);

/**
 * @brief Get patch version number
 * @return Patch version
 */
uint32_t proven_version_patch(void);

/* ============================================================================
 * Convenience Macros
 * ============================================================================ */

/**
 * @brief Check if a result indicates success
 */
#define PROVEN_SUCCEEDED(result) ((result).status == PROVEN_OK)

/**
 * @brief Check if a result indicates failure
 */
#define PROVEN_FAILED(result) ((result).status != PROVEN_OK)

/**
 * @brief Get Proven version as a string literal
 */
#define PROVEN_VERSION_STRING "0.3.0"

#ifdef __cplusplus
}
#endif

#endif /* PROVEN_H */
