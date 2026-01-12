// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file proven.h
 * @brief C API for the Proven verified safety library
 *
 * This header provides the C ABI for calling Proven functions from any
 * language that can interface with C (Python, Rust, Go, etc.).
 *
 * All functions are designed to:
 * - Never crash (no exceptions, no undefined behavior)
 * - Return explicit error codes via status fields
 * - Handle memory safely (caller-managed allocation where noted)
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
 * @brief Result status codes for all Proven operations
 */
typedef enum {
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
    PROVEN_ERR_NOT_IMPLEMENTED = -99,
} ProvenStatus;

/* ============================================================================
 * Result Types
 * ============================================================================ */

/**
 * @brief Result for integer operations
 */
typedef struct {
    ProvenStatus status;
    int64_t value;
} ProvenIntResult;

/**
 * @brief Result for boolean operations
 */
typedef struct {
    ProvenStatus status;
    bool value;
} ProvenBoolResult;

/**
 * @brief Result for string operations
 * @note Caller must free `value` using proven_free_string()
 */
typedef struct {
    ProvenStatus status;
    char* value;
    size_t length;
} ProvenStringResult;

/* ============================================================================
 * Memory Management
 * ============================================================================ */

/**
 * @brief Free a string allocated by Proven functions
 * @param ptr String pointer returned by a Proven function (may be NULL)
 */
void proven_free_string(char* ptr);

/* ============================================================================
 * SafeMath - Arithmetic operations that cannot crash
 * ============================================================================ */

/**
 * @brief Safe division with zero-check
 * @param numerator The dividend
 * @param denominator The divisor
 * @return Result with status and quotient
 * @retval PROVEN_ERR_DIVISION_BY_ZERO if denominator is 0
 * @retval PROVEN_ERR_OVERFLOW if numerator is INT64_MIN and denominator is -1
 */
ProvenIntResult proven_math_div(int64_t numerator, int64_t denominator);

/**
 * @brief Safe modulo with zero-check
 * @param numerator The dividend
 * @param denominator The divisor
 * @return Result with status and remainder
 */
ProvenIntResult proven_math_mod(int64_t numerator, int64_t denominator);

/**
 * @brief Addition with overflow detection
 * @return Result with PROVEN_ERR_OVERFLOW if overflow would occur
 */
ProvenIntResult proven_math_add_checked(int64_t a, int64_t b);

/**
 * @brief Subtraction with underflow detection
 * @return Result with PROVEN_ERR_UNDERFLOW if underflow would occur
 */
ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b);

/**
 * @brief Multiplication with overflow detection
 * @return Result with PROVEN_ERR_OVERFLOW if overflow would occur
 */
ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b);

/**
 * @brief Safe absolute value that handles INT64_MIN
 * @return Result with PROVEN_ERR_OVERFLOW for INT64_MIN (cannot represent |INT64_MIN|)
 */
ProvenIntResult proven_math_abs_safe(int64_t n);

/**
 * @brief Clamp value to range [lo, hi]
 */
int64_t proven_math_clamp(int64_t lo, int64_t hi, int64_t value);

/**
 * @brief Integer exponentiation with overflow detection
 */
ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp);

/* ============================================================================
 * SafeString - Text operations with encoding safety
 * ============================================================================ */

/**
 * @brief Check if byte sequence is valid UTF-8
 * @param ptr Pointer to byte data
 * @param len Length in bytes
 */
ProvenBoolResult proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for SQL (double single quotes)
 * @param ptr Input string
 * @param len Input length
 * @return Escaped string (caller must free)
 */
ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for HTML (< > & " ')
 * @param ptr Input string
 * @param len Input length
 * @return Escaped string (caller must free)
 */
ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for JavaScript
 * @param ptr Input string
 * @param len Input length
 * @return Escaped string (caller must free)
 */
ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafePath - Filesystem path safety
 * ============================================================================ */

/**
 * @brief Check if path contains traversal sequences (..)
 */
ProvenBoolResult proven_path_has_traversal(const uint8_t* ptr, size_t len);

/**
 * @brief Sanitize filename (remove unsafe characters)
 * @return Sanitized filename (caller must free)
 */
ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeCrypto - Cryptographic primitives
 * ============================================================================ */

/**
 * @brief Constant-time byte comparison (timing-safe)
 */
ProvenBoolResult proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2
);

/**
 * @brief Fill buffer with cryptographically secure random bytes
 * @param ptr Buffer to fill
 * @param len Number of bytes to generate
 * @return PROVEN_OK on success
 */
ProvenStatus proven_crypto_random_bytes(uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeUrl - URL parsing
 * ============================================================================ */

/**
 * @brief Parsed URL components
 */
typedef struct {
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
typedef struct {
    ProvenStatus status;
    ProvenUrlComponents components;
} ProvenUrlResult;

/**
 * @brief Parse URL into components
 * @param ptr URL string
 * @param len URL length
 * @return Parsed components (caller must free via proven_url_free)
 */
ProvenUrlResult proven_url_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Free URL components
 */
void proven_url_free(ProvenUrlComponents* components);

/* ============================================================================
 * SafeEmail - Email validation
 * ============================================================================ */

/**
 * @brief Validate email address (RFC 5321 simplified)
 */
ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeNetwork - IP address handling
 * ============================================================================ */

/**
 * @brief IPv4 address
 */
typedef struct {
    uint8_t octets[4];
} ProvenIPv4Address;

/**
 * @brief Result for IPv4 parsing
 */
typedef struct {
    ProvenStatus status;
    ProvenIPv4Address address;
} ProvenIPv4Result;

/**
 * @brief Parse IPv4 address string
 */
ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);

/**
 * @brief Check if IPv4 is private (RFC 1918)
 */
bool proven_network_ipv4_is_private(ProvenIPv4Address addr);

/**
 * @brief Check if IPv4 is loopback (127.0.0.0/8)
 */
bool proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

/* ============================================================================
 * Version Information
 * ============================================================================ */

uint32_t proven_version_major(void);
uint32_t proven_version_minor(void);
uint32_t proven_version_patch(void);

#ifdef __cplusplus
}
#endif

#endif /* PROVEN_H */
