// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file ffi.hpp
/// @brief Raw C function declarations for libproven FFI
///
/// This header declares all extern "C" functions exported by libproven.
/// It is the single source of truth for the C ABI surface used by the
/// C++ binding. All computation is performed by the Idris2 core via
/// the Zig FFI bridge; this header only declares the interface.

#pragma once

#include <cstdint>
#include <cstddef>
#include <cstdbool>

extern "C" {

// ============================================================================
// Status Codes
// ============================================================================

enum ProvenStatus : int32_t {
    PROVEN_OK                     =   0,
    PROVEN_ERR_NULL_POINTER       =  -1,
    PROVEN_ERR_INVALID_ARGUMENT   =  -2,
    PROVEN_ERR_OVERFLOW           =  -3,
    PROVEN_ERR_UNDERFLOW          =  -4,
    PROVEN_ERR_DIVISION_BY_ZERO   =  -5,
    PROVEN_ERR_PARSE_FAILURE      =  -6,
    PROVEN_ERR_VALIDATION_FAILED  =  -7,
    PROVEN_ERR_OUT_OF_BOUNDS      =  -8,
    PROVEN_ERR_ENCODING_ERROR     =  -9,
    PROVEN_ERR_ALLOCATION_FAILED  = -10,
    PROVEN_ERR_NOT_IMPLEMENTED    = -99
};

// ============================================================================
// Result Types
// ============================================================================

struct ProvenIntResult {
    ProvenStatus status;
    int64_t value;
};

struct ProvenBoolResult {
    ProvenStatus status;
    bool value;
};

struct ProvenStringResult {
    ProvenStatus status;
    char* value;
    size_t length;
};

struct ProvenFloatResult {
    ProvenStatus status;
    double value;
};

struct ProvenIPv4Address {
    uint8_t octets[4];
};

struct ProvenIPv4Result {
    ProvenStatus status;
    ProvenIPv4Address address;
};

struct ProvenUrlComponents {
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
};

struct ProvenUrlResult {
    ProvenStatus status;
    ProvenUrlComponents components;
};

struct ProvenRGBColor {
    uint8_t r;
    uint8_t g;
    uint8_t b;
};

struct ProvenHSLColor {
    double h;
    double s;
    double l;
};

struct ProvenColorResult {
    ProvenStatus status;
    ProvenRGBColor color;
};

struct ProvenSemanticVersion {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;
    size_t prerelease_len;
    char* prerelease;
};

struct ProvenVersionResult {
    ProvenStatus status;
    ProvenSemanticVersion version;
};

enum ProvenJsonType : int32_t {
    PROVEN_JSON_NULL    =  0,
    PROVEN_JSON_BOOL    =  1,
    PROVEN_JSON_NUMBER  =  2,
    PROVEN_JSON_STRING  =  3,
    PROVEN_JSON_ARRAY   =  4,
    PROVEN_JSON_OBJECT  =  5,
    PROVEN_JSON_INVALID = -1
};

// ============================================================================
// Runtime Management
// ============================================================================

int32_t  proven_init();
void     proven_deinit();
bool     proven_is_initialized();
uint32_t proven_ffi_abi_version();

// ============================================================================
// Memory Management
// ============================================================================

void proven_free_string(char* ptr);
void proven_url_free(ProvenUrlComponents* components);
void proven_version_free(ProvenSemanticVersion* version);

// ============================================================================
// SafeMath
// ============================================================================

ProvenIntResult proven_math_div(int64_t numerator, int64_t denominator);
ProvenIntResult proven_math_mod(int64_t numerator, int64_t denominator);
ProvenIntResult proven_math_add_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_abs_safe(int64_t n);
int64_t         proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp);

// ============================================================================
// SafeString
// ============================================================================

ProvenBoolResult   proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

// ============================================================================
// SafePath
// ============================================================================

ProvenBoolResult   proven_path_has_traversal(const uint8_t* ptr, size_t len);
ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

// ============================================================================
// SafeCrypto
// ============================================================================

ProvenBoolResult proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2
);
ProvenStatus proven_crypto_random_bytes(uint8_t* ptr, size_t len);

// ============================================================================
// SafeEmail
// ============================================================================

ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

// ============================================================================
// SafeNetwork
// ============================================================================

ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);
bool proven_network_ipv4_is_private(ProvenIPv4Address addr);
bool proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

// ============================================================================
// SafeUrl
// ============================================================================

ProvenUrlResult proven_url_parse(const uint8_t* ptr, size_t len);

// ============================================================================
// SafeFloat
// ============================================================================

ProvenFloatResult proven_float_div(double a, double b);
ProvenFloatResult proven_float_sqrt(double x);
ProvenFloatResult proven_float_ln(double x);
bool              proven_float_is_finite(double x);
bool              proven_float_is_nan(double x);

// ============================================================================
// SafeJson
// ============================================================================

ProvenBoolResult proven_json_is_valid(const uint8_t* ptr, size_t len);
ProvenJsonType   proven_json_get_type(const uint8_t* ptr, size_t len);

// ============================================================================
// SafeVersion
// ============================================================================

ProvenVersionResult proven_version_parse(const uint8_t* ptr, size_t len);
int32_t             proven_version_compare(ProvenSemanticVersion a, ProvenSemanticVersion b);

// ============================================================================
// SafeColor
// ============================================================================

ProvenColorResult  proven_color_parse_hex(const uint8_t* ptr, size_t len);
ProvenHSLColor     proven_color_rgb_to_hsl(ProvenRGBColor rgb);
ProvenStringResult proven_color_to_hex(ProvenRGBColor rgb);

// ============================================================================
// SafeAngle
// ============================================================================

double proven_angle_deg_to_rad(double degrees);
double proven_angle_rad_to_deg(double radians);
double proven_angle_normalize_degrees(double degrees);
double proven_angle_normalize_radians(double radians);

// ============================================================================
// Version Information
// ============================================================================

uint32_t proven_version_major();
uint32_t proven_version_minor();
uint32_t proven_version_patch();

} // extern "C"
