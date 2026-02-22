<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven FFI - PHP binding to libproven (formally verified safety library).
// All computation is performed in Idris 2 via the Zig FFI layer.
// This module is a thin wrapper; it does NOT reimplement any logic.

declare(strict_types=1);

namespace Proven;

/**
 * FFI loader for libproven.
 *
 * Provides the raw FFI handle to the C ABI exported by the Zig FFI layer.
 * All 103+ exported functions are declared here for use by the wrapper classes.
 */
final class FFI
{
    private static ?\FFI $lib = null;

    /**
     * Get the FFI handle to libproven.
     *
     * Lazily initializes the FFI declarations on first call.
     */
    public static function getLib(): \FFI
    {
        if (self::$lib === null) {
            self::$lib = \FFI::cdef(
                self::CDEF,
                'libproven.so'
            );
        }
        return self::$lib;
    }

    /**
     * Initialize the Proven runtime (Idris 2 + Zig).
     * Must be called before using any other functions.
     *
     * @return bool True on success.
     */
    public static function init(): bool
    {
        return self::getLib()->proven_init() === 0;
    }

    /**
     * Deinitialize the Proven runtime.
     */
    public static function deinit(): void
    {
        self::getLib()->proven_deinit();
    }

    /**
     * Check if the runtime is initialized.
     */
    public static function isInitialized(): bool
    {
        return self::getLib()->proven_is_initialized();
    }

    private const CDEF = '
        /* Result types */
        typedef struct { int32_t status; int64_t value; } IntResult;
        typedef struct { int32_t status; bool value; } BoolResult;
        typedef struct { int32_t status; char* value; size_t length; } StringResult;
        typedef struct { int32_t status; double value; } FloatResult;

        /* Runtime lifecycle */
        void proven_free_string(char* ptr);
        int32_t  proven_init(void);
        void     proven_deinit(void);
        bool     proven_is_initialized(void);
        uint32_t proven_ffi_abi_version(void);
        uint32_t proven_version_major(void);
        uint32_t proven_version_minor(void);
        uint32_t proven_version_patch(void);
        uint32_t proven_module_count(void);

        /* SafeMath */
        IntResult proven_math_div(int64_t a, int64_t b);
        IntResult proven_math_mod(int64_t a, int64_t b);
        IntResult proven_math_add_checked(int64_t a, int64_t b);
        IntResult proven_math_sub_checked(int64_t a, int64_t b);
        IntResult proven_math_mul_checked(int64_t a, int64_t b);
        IntResult proven_math_abs_safe(int64_t n);
        int64_t   proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
        IntResult proven_math_pow_checked(int64_t base, uint32_t exp);

        /* SafeString */
        BoolResult   proven_string_is_valid_utf8(const char* ptr, size_t len);
        StringResult proven_string_escape_sql(const char* ptr, size_t len);
        StringResult proven_string_escape_html(const char* ptr, size_t len);
        StringResult proven_string_escape_js(const char* ptr, size_t len);

        /* SafePath */
        BoolResult   proven_path_has_traversal(const char* ptr, size_t len);
        StringResult proven_path_sanitize_filename(const char* ptr, size_t len);

        /* SafeEmail */
        BoolResult proven_email_is_valid(const char* ptr, size_t len);

        /* SafeNetwork */
        typedef struct { uint8_t octets[4]; } IPv4Address;
        typedef struct { int32_t status; uint8_t octets[4]; } IPv4Result;
        IPv4Result proven_network_parse_ipv4(const char* ptr, size_t len);
        bool proven_network_ipv4_is_private(IPv4Address addr);
        bool proven_network_ipv4_is_loopback(IPv4Address addr);

        /* SafeCrypto */
        BoolResult proven_crypto_constant_time_eq(const char* p1, size_t l1,
                                                   const char* p2, size_t l2);
        int32_t proven_crypto_random_bytes(char* ptr, size_t len);

        /* SafeJson */
        BoolResult proven_json_is_valid(const char* ptr, size_t len);
        int32_t    proven_json_get_type(const char* ptr, size_t len);

        /* SafeHeader */
        BoolResult   proven_header_has_crlf(const char* ptr, size_t len);
        BoolResult   proven_header_is_valid_name(const char* ptr, size_t len);
        BoolResult   proven_header_is_dangerous(const char* ptr, size_t len);
        StringResult proven_header_render(const char* name, size_t nl,
                                          const char* value, size_t vl);
        StringResult proven_header_build_csp(const char* json, size_t len);
        StringResult proven_header_build_hsts(int64_t max_age,
                                              bool include_subdomains,
                                              bool preload);

        /* SafeCookie */
        BoolResult   proven_cookie_has_injection(const char* ptr, size_t len);
        BoolResult   proven_cookie_validate_name(const char* ptr, size_t len);
        BoolResult   proven_cookie_validate_value(const char* ptr, size_t len);
        IntResult    proven_cookie_get_prefix(const char* ptr, size_t len);
        StringResult proven_cookie_build_delete(const char* name, size_t nl);

        /* SafeContentType */
        BoolResult proven_content_type_can_sniff_dangerous(const char* ptr, size_t len);
        BoolResult proven_content_type_is_json(const char* sub, size_t sl,
                                                const char* suf, size_t fl);
        BoolResult proven_content_type_is_xml(const char* sub, size_t sl,
                                               const char* suf, size_t fl);

        /* SafeHex */
        StringResult proven_hex_encode(const char* ptr, size_t len, bool uppercase);
        typedef struct { int32_t status; char* data; size_t length; } HexDecodeResult;
        HexDecodeResult proven_hex_decode(const char* ptr, size_t len);
        void proven_hex_free(HexDecodeResult* result);

        /* SafeUUID */
        typedef struct { uint8_t bytes[16]; } UUID;
        typedef struct { int32_t status; UUID uuid; } UUIDResult;
        UUIDResult   proven_uuid_v4(void);
        StringResult proven_uuid_to_string(UUID uuid);
        UUIDResult   proven_uuid_parse(const char* ptr, size_t len);
        bool         proven_uuid_is_nil(UUID uuid);
        uint8_t      proven_uuid_version(UUID uuid);

        /* SafeCurrency */
        typedef struct {
            int32_t  status;
            int64_t  amount_minor;
            uint8_t  currency_code[3];
            uint8_t  decimal_places;
        } CurrencyResult;
        CurrencyResult proven_currency_parse(const char* ptr, size_t len);
        StringResult   proven_currency_format(int64_t amount_minor,
                                              uint8_t code[3], uint8_t dp);

        /* SafePhone */
        typedef struct {
            int32_t  status;
            uint16_t country_code;
            uint64_t national_number;
            bool     is_valid;
        } PhoneResult;
        PhoneResult  proven_phone_parse(const char* ptr, size_t len);
        StringResult proven_phone_format_e164(uint16_t cc, uint64_t nn);

        /* SafeDateTime */
        typedef struct {
            int32_t  year;
            uint8_t  month;
            uint8_t  day;
            uint8_t  hour;
            uint8_t  minute;
            uint8_t  second;
            uint32_t nanosecond;
            int16_t  tz_offset_minutes;
        } DateTime;
        typedef struct { int32_t status; DateTime datetime; } DateTimeResult;
        DateTimeResult proven_datetime_parse(const char* ptr, size_t len);
        StringResult   proven_datetime_format_iso8601(DateTime dt);
        bool           proven_datetime_is_leap_year(int32_t year);
        uint8_t        proven_datetime_days_in_month(int32_t year, uint8_t month);

        /* SafeFloat */
        FloatResult proven_float_div(double a, double b);
        bool        proven_float_is_finite(double x);
        bool        proven_float_is_nan(double x);
        FloatResult proven_float_sqrt(double x);
        FloatResult proven_float_ln(double x);

        /* SafeVersion */
        typedef struct {
            uint32_t major;
            uint32_t minor;
            uint32_t patch;
            size_t   prerelease_len;
            char*    prerelease;
        } SemanticVersion;
        typedef struct { int32_t status; SemanticVersion version; } VersionResult;
        VersionResult proven_version_parse(const char* ptr, size_t len);
        int32_t       proven_version_compare(SemanticVersion a, SemanticVersion b);
        void          proven_version_free(SemanticVersion* version);

        /* SafeGeo */
        typedef struct { double latitude; double longitude; } GeoCoordinate;
        typedef struct { int32_t status; GeoCoordinate coordinate; } GeoResult;
        GeoResult   proven_geo_validate(double lat, double lon);
        FloatResult proven_geo_distance(GeoCoordinate a, GeoCoordinate b);
        bool        proven_geo_in_bounds(GeoCoordinate coord,
                                         double min_lat, double max_lat,
                                         double min_lon, double max_lon);

        /* SafeChecksum */
        IntResult  proven_checksum_crc32(const char* ptr, size_t len);
        BoolResult proven_checksum_verify_crc32(const char* ptr, size_t len,
                                                uint32_t expected);

        /* SafeProbability */
        double proven_probability_create(double value);
        double proven_probability_and(double a, double b);
        double proven_probability_or_exclusive(double a, double b);
        double proven_probability_not(double p);

        /* SafeCalculator */
        FloatResult proven_calculator_eval(const char* ptr, size_t len);

        /* SafePassword */
        typedef struct {
            int32_t strength;
            bool    has_lowercase;
            bool    has_uppercase;
            bool    has_digit;
            bool    has_special;
            size_t  length;
        } PasswordResult;
        PasswordResult proven_password_validate(const char* ptr, size_t len);
        bool           proven_password_is_common(const char* ptr, size_t len);

        /* SafeColor */
        typedef struct { uint8_t r; uint8_t g; uint8_t b; } RGBColor;
        typedef struct { double h; double s; double l; } HSLColor;
        typedef struct { int32_t status; RGBColor color; } ColorParseResult;
        ColorParseResult proven_color_parse_hex(const char* ptr, size_t len);
        HSLColor         proven_color_rgb_to_hsl(RGBColor rgb);
        StringResult     proven_color_to_hex(RGBColor rgb);

        /* SafeAngle */
        double proven_angle_deg_to_rad(double degrees);
        double proven_angle_rad_to_deg(double radians);
        double proven_angle_normalize_degrees(double degrees);
        double proven_angle_normalize_radians(double radians);

        /* SafeUnit */
        FloatResult proven_unit_convert_length(double value, int32_t from, int32_t to);
        FloatResult proven_unit_convert_temp(double value, int32_t from, int32_t to);

        /* SafeML */
        int32_t proven_ml_softmax(const double* input, double* output, size_t len);
        double  proven_ml_sigmoid(double x);
        double  proven_ml_relu(double x);
        double  proven_ml_leaky_relu(double x, double alpha);
        double  proven_ml_clamp(double x, double min_val, double max_val);

        /* SafeHTTP */
        StringResult proven_http_url_encode(const char* ptr, size_t len);
        StringResult proven_http_url_decode(const char* ptr, size_t len);

        /* Callback infrastructure */
        uint32_t proven_callback_count(int32_t event_type);
        uint32_t proven_callback_clear_all(void);
    ';
}
