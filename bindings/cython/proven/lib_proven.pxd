# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cdef extern declarations for libproven.
#
# This file declares the C ABI types and functions exported by
# libproven. All computation is performed in the formally verified
# Idris 2 core via the Zig FFI bridge. This declaration file MUST
# NOT reimplement any logic.

from libc.stdint cimport int32_t, int64_t, uint8_t, uint16_t, uint32_t, uint64_t
from libc.stddef cimport size_t
from libc.string cimport const_char


cdef extern from "proven.h":

    # ----------------------------------------------------------------
    # Status codes
    # ----------------------------------------------------------------

    ctypedef enum ProvenStatus:
        PROVEN_OK                      =   0
        PROVEN_ERR_NULL_POINTER        =  -1
        PROVEN_ERR_INVALID_ARGUMENT    =  -2
        PROVEN_ERR_OVERFLOW            =  -3
        PROVEN_ERR_UNDERFLOW           =  -4
        PROVEN_ERR_DIVISION_BY_ZERO    =  -5
        PROVEN_ERR_PARSE_FAILURE       =  -6
        PROVEN_ERR_VALIDATION_FAILED   =  -7
        PROVEN_ERR_OUT_OF_BOUNDS       =  -8
        PROVEN_ERR_ENCODING_ERROR      =  -9
        PROVEN_ERR_ALLOCATION_FAILED   = -10
        PROVEN_ERR_NOT_IMPLEMENTED     = -99

    # ----------------------------------------------------------------
    # Core result types
    # ----------------------------------------------------------------

    ctypedef struct ProvenIntResult:
        int32_t status
        int64_t value

    ctypedef struct ProvenBoolResult:
        int32_t status
        bint value

    ctypedef struct ProvenStringResult:
        int32_t status
        char* value
        size_t length

    ctypedef struct ProvenFloatResult:
        int32_t status
        double value

    # ----------------------------------------------------------------
    # URL types
    # ----------------------------------------------------------------

    ctypedef struct ProvenUrlComponents:
        char* scheme
        size_t scheme_len
        char* host
        size_t host_len
        uint16_t port
        bint has_port
        char* path
        size_t path_len
        char* query
        size_t query_len
        char* fragment
        size_t fragment_len

    ctypedef struct ProvenUrlResult:
        int32_t status
        ProvenUrlComponents components

    # ----------------------------------------------------------------
    # Network types
    # ----------------------------------------------------------------

    ctypedef struct ProvenIPv4Address:
        uint8_t octets[4]

    ctypedef struct ProvenIPv4Result:
        int32_t status
        ProvenIPv4Address address

    # ----------------------------------------------------------------
    # DateTime types
    # ----------------------------------------------------------------

    ctypedef struct ProvenDateTime:
        int32_t year
        uint8_t month
        uint8_t day
        uint8_t hour
        uint8_t minute
        uint8_t second
        uint32_t nanosecond
        int16_t tz_offset_minutes

    ctypedef struct ProvenDateTimeResult:
        int32_t status
        ProvenDateTime datetime

    # ----------------------------------------------------------------
    # Hex decode type
    # ----------------------------------------------------------------

    ctypedef struct ProvenHexDecodeResult:
        int32_t status
        uint8_t* data
        size_t length

    # ----------------------------------------------------------------
    # Color types
    # ----------------------------------------------------------------

    ctypedef struct ProvenRGBColor:
        uint8_t r
        uint8_t g
        uint8_t b

    ctypedef struct ProvenHSLColor:
        double h
        double s
        double l

    ctypedef struct ProvenColorResult:
        int32_t status
        ProvenRGBColor color

    # ----------------------------------------------------------------
    # JSON type enum
    # ----------------------------------------------------------------

    ctypedef enum ProvenJsonType:
        PROVEN_JSON_NULL    =  0
        PROVEN_JSON_BOOL    =  1
        PROVEN_JSON_NUMBER  =  2
        PROVEN_JSON_STRING  =  3
        PROVEN_JSON_ARRAY   =  4
        PROVEN_JSON_OBJECT  =  5
        PROVEN_JSON_INVALID = -1

    # ----------------------------------------------------------------
    # Memory management
    # ----------------------------------------------------------------

    void proven_free_string(char* ptr) nogil
    void proven_url_free(ProvenUrlComponents* components) nogil
    void proven_hex_free(ProvenHexDecodeResult* result) nogil

    # ----------------------------------------------------------------
    # Lifecycle
    # ----------------------------------------------------------------

    int32_t proven_init() nogil
    void proven_deinit() nogil
    bint proven_is_initialized() nogil

    # ----------------------------------------------------------------
    # Version
    # ----------------------------------------------------------------

    uint32_t proven_ffi_abi_version() nogil
    uint32_t proven_version_major() nogil
    uint32_t proven_version_minor() nogil
    uint32_t proven_version_patch() nogil
    uint32_t proven_module_count() nogil

    # ----------------------------------------------------------------
    # SafeMath
    # ----------------------------------------------------------------

    ProvenIntResult proven_math_add_checked(int64_t a, int64_t b) nogil
    ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b) nogil
    ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b) nogil
    ProvenIntResult proven_math_div(int64_t numerator, int64_t denominator) nogil
    ProvenIntResult proven_math_mod(int64_t numerator, int64_t denominator) nogil
    ProvenIntResult proven_math_abs_safe(int64_t n) nogil
    int64_t proven_math_clamp(int64_t lo, int64_t hi, int64_t value) nogil
    ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp) nogil

    # ----------------------------------------------------------------
    # SafeString
    # ----------------------------------------------------------------

    ProvenBoolResult proven_string_is_valid_utf8(const uint8_t* ptr, size_t length) nogil
    ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t length) nogil
    ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t length) nogil
    ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t length) nogil

    # ----------------------------------------------------------------
    # SafePath
    # ----------------------------------------------------------------

    ProvenBoolResult proven_path_has_traversal(const uint8_t* ptr, size_t length) nogil
    ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t length) nogil

    # ----------------------------------------------------------------
    # SafeEmail
    # ----------------------------------------------------------------

    ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t length) nogil

    # ----------------------------------------------------------------
    # SafeUrl
    # ----------------------------------------------------------------

    ProvenUrlResult proven_url_parse(const uint8_t* ptr, size_t length) nogil

    # ----------------------------------------------------------------
    # SafeCrypto
    # ----------------------------------------------------------------

    ProvenBoolResult proven_crypto_constant_time_eq(
        const uint8_t* ptr1, size_t len1,
        const uint8_t* ptr2, size_t len2) nogil
    ProvenStatus proven_crypto_random_bytes(uint8_t* ptr, size_t length) nogil

    # ----------------------------------------------------------------
    # SafeJson
    # ----------------------------------------------------------------

    ProvenBoolResult proven_json_is_valid(const uint8_t* ptr, size_t length) nogil
    ProvenJsonType proven_json_get_type(const uint8_t* ptr, size_t length) nogil

    # ----------------------------------------------------------------
    # SafeFloat
    # ----------------------------------------------------------------

    ProvenFloatResult proven_float_div(double a, double b) nogil
    bint proven_float_is_finite(double x) nogil
    bint proven_float_is_nan(double x) nogil
    ProvenFloatResult proven_float_sqrt(double x) nogil
    ProvenFloatResult proven_float_ln(double x) nogil

    # ----------------------------------------------------------------
    # SafeDateTime
    # ----------------------------------------------------------------

    ProvenDateTimeResult proven_datetime_parse(const uint8_t* ptr, size_t length) nogil
    ProvenStringResult proven_datetime_format_iso8601(ProvenDateTime dt) nogil
    bint proven_datetime_is_leap_year(int32_t year) nogil
    uint8_t proven_datetime_days_in_month(int32_t year, uint8_t month) nogil

    # ----------------------------------------------------------------
    # SafeNetwork
    # ----------------------------------------------------------------

    ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t length) nogil
    bint proven_network_ipv4_is_private(ProvenIPv4Address addr) nogil
    bint proven_network_ipv4_is_loopback(ProvenIPv4Address addr) nogil

    # ----------------------------------------------------------------
    # SafeHex
    # ----------------------------------------------------------------

    ProvenStringResult proven_hex_encode(const uint8_t* ptr, size_t length, bint uppercase) nogil
    ProvenHexDecodeResult proven_hex_decode(const uint8_t* ptr, size_t length) nogil

    # ----------------------------------------------------------------
    # SafeChecksum
    # ----------------------------------------------------------------

    ProvenIntResult proven_checksum_crc32(const uint8_t* ptr, size_t length) nogil
    ProvenBoolResult proven_checksum_verify_crc32(
        const uint8_t* ptr, size_t length, uint32_t expected) nogil

    # ----------------------------------------------------------------
    # SafeColor
    # ----------------------------------------------------------------

    ProvenColorResult proven_color_parse_hex(const uint8_t* ptr, size_t length) nogil
    ProvenHSLColor proven_color_rgb_to_hsl(ProvenRGBColor rgb) nogil
    ProvenStringResult proven_color_to_hex(ProvenRGBColor rgb) nogil

    # ----------------------------------------------------------------
    # SafeCalculator
    # ----------------------------------------------------------------

    ProvenFloatResult proven_calculator_eval(const uint8_t* ptr, size_t length) nogil

    # ----------------------------------------------------------------
    # SafeVersion
    # ----------------------------------------------------------------

    int32_t proven_version_compare(
        const uint8_t* a_ptr, size_t a_len,
        const uint8_t* b_ptr, size_t b_len) nogil
