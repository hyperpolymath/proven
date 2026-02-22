/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/*
 * proven_header.h - Minimal C header for MATLAB/Octave loadlibrary().
 *
 * This header declares the libproven C ABI types and functions so that
 * MATLAB's loadlibrary() / calllib() and GNU Octave's equivalent can
 * invoke the formally verified Proven library.  No logic is reimplemented
 * here; all computation is performed in the Idris 2 core via the Zig FFI
 * bridge.
 *
 * Usage:
 *   loadlibrary('proven', 'proven_header.h');
 *   result = calllib('proven', 'proven_math_add_checked', int64(2), int64(3));
 */

#ifndef PROVEN_MATLAB_HEADER_H
#define PROVEN_MATLAB_HEADER_H

#include <stdint.h>
#include <stddef.h>

/* ========================================================================
 * Status codes
 * ======================================================================== */

/* MATLAB loadlibrary does not support enums well, so we use defines. */
#define PROVEN_OK                      0
#define PROVEN_ERR_NULL_POINTER       -1
#define PROVEN_ERR_INVALID_ARGUMENT   -2
#define PROVEN_ERR_OVERFLOW           -3
#define PROVEN_ERR_UNDERFLOW          -4
#define PROVEN_ERR_DIVISION_BY_ZERO   -5
#define PROVEN_ERR_PARSE_FAILURE      -6
#define PROVEN_ERR_VALIDATION_FAILED  -7
#define PROVEN_ERR_OUT_OF_BOUNDS      -8
#define PROVEN_ERR_ENCODING_ERROR     -9
#define PROVEN_ERR_ALLOCATION_FAILED -10
#define PROVEN_ERR_NOT_IMPLEMENTED   -99

/* ========================================================================
 * Result structures
 * ======================================================================== */

struct IntResult   { int32_t status; int64_t value; };
struct BoolResult  { int32_t status; int32_t value; };
struct FloatResult { int32_t status; double  value; };

/*
 * StringResult - value must be freed with proven_free_string().
 * In MATLAB the char* is returned as a libpointer; use proven_free_string
 * when done.
 */
struct StringResult { int32_t status; char* value; int32_t length; };

/* ========================================================================
 * Lifecycle
 * ======================================================================== */

int32_t  proven_init(void);
void     proven_deinit(void);
int32_t  proven_is_initialized(void);
uint32_t proven_ffi_abi_version(void);
uint32_t proven_version_major(void);
uint32_t proven_version_minor(void);
uint32_t proven_version_patch(void);
uint32_t proven_module_count(void);

/* ========================================================================
 * Memory
 * ======================================================================== */

void proven_free_string(char* ptr);

/* ========================================================================
 * SafeMath
 * ======================================================================== */

struct IntResult proven_math_add_checked(int64_t a, int64_t b);
struct IntResult proven_math_sub_checked(int64_t a, int64_t b);
struct IntResult proven_math_mul_checked(int64_t a, int64_t b);
struct IntResult proven_math_div(int64_t a, int64_t b);
struct IntResult proven_math_mod(int64_t a, int64_t b);
struct IntResult proven_math_abs_safe(int64_t n);
int64_t          proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
struct IntResult proven_math_pow_checked(int64_t base, uint32_t exp);

/* ========================================================================
 * SafeString
 * ======================================================================== */

struct BoolResult   proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);
struct StringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);
struct StringResult proven_string_escape_html(const uint8_t* ptr, size_t len);
struct StringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

/* ========================================================================
 * SafePath
 * ======================================================================== */

struct BoolResult   proven_path_has_traversal(const uint8_t* ptr, size_t len);
struct StringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

/* ========================================================================
 * SafeEmail
 * ======================================================================== */

struct BoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

/* ========================================================================
 * SafeNetwork
 * ======================================================================== */

struct IntResult proven_network_parse_ipv4(const uint8_t* ptr, size_t len);
int32_t proven_network_ipv4_is_private(uint8_t o1, uint8_t o2, uint8_t o3, uint8_t o4);
int32_t proven_network_ipv4_is_loopback(uint8_t o1, uint8_t o2, uint8_t o3, uint8_t o4);

/* ========================================================================
 * SafeCrypto
 * ======================================================================== */

struct BoolResult   proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2
);
int32_t proven_crypto_random_bytes(uint8_t* ptr, size_t len);

/* ========================================================================
 * SafeUrl
 * ======================================================================== */

struct StringResult proven_http_url_encode(const uint8_t* ptr, size_t len);
struct StringResult proven_http_url_decode(const uint8_t* ptr, size_t len);

/* ========================================================================
 * SafeJson
 * ======================================================================== */

struct BoolResult proven_json_is_valid(const uint8_t* ptr, size_t len);
int32_t           proven_json_get_type(const uint8_t* ptr, size_t len);

/* ========================================================================
 * SafeDateTime
 * ======================================================================== */

struct StringResult proven_datetime_format_iso8601_simple(
    int32_t year, uint8_t month, uint8_t day,
    uint8_t hour, uint8_t minute, uint8_t second
);
int32_t proven_datetime_is_leap_year(int32_t year);
uint8_t proven_datetime_days_in_month(int32_t year, uint8_t month);

/* ========================================================================
 * SafeFloat
 * ======================================================================== */

struct FloatResult proven_float_div(double a, double b);
int32_t            proven_float_is_finite(double x);
int32_t            proven_float_is_nan(double x);
struct FloatResult proven_float_sqrt(double x);
struct FloatResult proven_float_ln(double x);

/* ========================================================================
 * SafeHex
 * ======================================================================== */

struct StringResult proven_hex_encode(const uint8_t* ptr, size_t len, int32_t uppercase);

/* ========================================================================
 * SafeColor
 * ======================================================================== */

struct StringResult proven_color_to_hex(uint8_t r, uint8_t g, uint8_t b);

/* ========================================================================
 * SafeAngle
 * ======================================================================== */

double proven_angle_deg_to_rad(double degrees);
double proven_angle_rad_to_deg(double radians);
double proven_angle_normalize_degrees(double degrees);
double proven_angle_normalize_radians(double radians);

/* ========================================================================
 * SafeCalculator
 * ======================================================================== */

struct FloatResult proven_calculator_eval(const uint8_t* ptr, size_t len);

#endif /* PROVEN_MATLAB_HEADER_H */
