// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * lib_proven.vapi -- Vala VAPI declarations for libproven C ABI.
 *
 * This file declares every extern C function, struct, and enum exported by
 * libproven.  Vala compiles to C so these are direct C interop bindings --
 * no reimplementation of any logic occurs here.
 *
 * Link with: -lproven
 */

[CCode (cheader_filename = "proven.h")]
namespace LibProven {

    /* ================================================================
     * Status codes
     * ================================================================ */

    [CCode (cname = "ProvenStatus", cprefix = "PROVEN_", has_type_id = false)]
    public enum Status {
        [CCode (cname = "PROVEN_OK")]
        OK = 0,
        [CCode (cname = "PROVEN_ERR_NULL_POINTER")]
        ERR_NULL_POINTER = -1,
        [CCode (cname = "PROVEN_ERR_INVALID_ARGUMENT")]
        ERR_INVALID_ARGUMENT = -2,
        [CCode (cname = "PROVEN_ERR_OVERFLOW")]
        ERR_OVERFLOW = -3,
        [CCode (cname = "PROVEN_ERR_UNDERFLOW")]
        ERR_UNDERFLOW = -4,
        [CCode (cname = "PROVEN_ERR_DIVISION_BY_ZERO")]
        ERR_DIVISION_BY_ZERO = -5,
        [CCode (cname = "PROVEN_ERR_PARSE_FAILURE")]
        ERR_PARSE_FAILURE = -6,
        [CCode (cname = "PROVEN_ERR_VALIDATION_FAILED")]
        ERR_VALIDATION_FAILED = -7,
        [CCode (cname = "PROVEN_ERR_OUT_OF_BOUNDS")]
        ERR_OUT_OF_BOUNDS = -8,
        [CCode (cname = "PROVEN_ERR_ENCODING_ERROR")]
        ERR_ENCODING_ERROR = -9,
        [CCode (cname = "PROVEN_ERR_ALLOCATION_FAILED")]
        ERR_ALLOCATION_FAILED = -10,
        [CCode (cname = "PROVEN_ERR_NOT_IMPLEMENTED")]
        ERR_NOT_IMPLEMENTED = -99;

        /** Human-readable description of a status code. */
        public string to_description () {
            switch (this) {
                case OK:                    return "Success";
                case ERR_NULL_POINTER:      return "Null pointer";
                case ERR_INVALID_ARGUMENT:  return "Invalid argument";
                case ERR_OVERFLOW:          return "Overflow";
                case ERR_UNDERFLOW:         return "Underflow";
                case ERR_DIVISION_BY_ZERO:  return "Division by zero";
                case ERR_PARSE_FAILURE:     return "Parse failure";
                case ERR_VALIDATION_FAILED: return "Validation failed";
                case ERR_OUT_OF_BOUNDS:     return "Out of bounds";
                case ERR_ENCODING_ERROR:    return "Encoding error";
                case ERR_ALLOCATION_FAILED: return "Allocation failed";
                case ERR_NOT_IMPLEMENTED:   return "Not implemented";
                default:                    return "Unknown error (%d)".printf ((int) this);
            }
        }
    }

    /* ================================================================
     * Core result types
     * ================================================================ */

    [CCode (cname = "ProvenIntResult", has_type_id = false)]
    public struct IntResult {
        public int32 status;
        public int64  value;
    }

    [CCode (cname = "ProvenBoolResult", has_type_id = false)]
    public struct BoolResult {
        public int32 status;
        public bool  value;
    }

    [CCode (cname = "ProvenStringResult", has_type_id = false)]
    public struct StringResult {
        public int32  status;
        public char*  value;
        public size_t length;
    }

    [CCode (cname = "ProvenFloatResult", has_type_id = false)]
    public struct FloatResult {
        public int32  status;
        public double value;
    }

    /* ================================================================
     * Memory management
     * ================================================================ */

    [CCode (cname = "proven_free_string")]
    public static void free_string (char* ptr);

    /* ================================================================
     * Lifecycle
     * ================================================================ */

    [CCode (cname = "proven_init")]
    public static int32 init ();

    [CCode (cname = "proven_deinit")]
    public static void deinit ();

    [CCode (cname = "proven_is_initialized")]
    public static bool is_initialized ();

    /* ================================================================
     * Version information
     * ================================================================ */

    [CCode (cname = "proven_ffi_abi_version")]
    public static uint32 ffi_abi_version ();

    [CCode (cname = "proven_version_major")]
    public static uint32 version_major ();

    [CCode (cname = "proven_version_minor")]
    public static uint32 version_minor ();

    [CCode (cname = "proven_version_patch")]
    public static uint32 version_patch ();

    [CCode (cname = "proven_module_count")]
    public static uint32 module_count ();

    /* ================================================================
     * SafeMath
     * ================================================================ */

    [CCode (cname = "proven_math_div")]
    public static IntResult math_div (int64 numerator, int64 denominator);

    [CCode (cname = "proven_math_mod")]
    public static IntResult math_mod (int64 numerator, int64 denominator);

    [CCode (cname = "proven_math_add_checked")]
    public static IntResult math_add_checked (int64 a, int64 b);

    [CCode (cname = "proven_math_sub_checked")]
    public static IntResult math_sub_checked (int64 a, int64 b);

    [CCode (cname = "proven_math_mul_checked")]
    public static IntResult math_mul_checked (int64 a, int64 b);

    [CCode (cname = "proven_math_abs_safe")]
    public static IntResult math_abs_safe (int64 n);

    [CCode (cname = "proven_math_clamp")]
    public static int64 math_clamp (int64 lo, int64 hi, int64 value);

    [CCode (cname = "proven_math_pow_checked")]
    public static IntResult math_pow_checked (int64 base_val, uint32 exp);

    /* ================================================================
     * SafeString
     * ================================================================ */

    [CCode (cname = "proven_string_is_valid_utf8")]
    public static BoolResult string_is_valid_utf8 ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_string_escape_sql")]
    public static StringResult string_escape_sql ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_string_escape_html")]
    public static StringResult string_escape_html ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_string_escape_js")]
    public static StringResult string_escape_js ([CCode (array_length_type = "size_t")] uint8[] data);

    /* ================================================================
     * SafePath
     * ================================================================ */

    [CCode (cname = "proven_path_has_traversal")]
    public static BoolResult path_has_traversal ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_path_sanitize_filename")]
    public static StringResult path_sanitize_filename ([CCode (array_length_type = "size_t")] uint8[] data);

    /* ================================================================
     * SafeCrypto
     * ================================================================ */

    [CCode (cname = "proven_crypto_constant_time_eq")]
    public static BoolResult crypto_constant_time_eq (
        [CCode (array_length_type = "size_t")] uint8[] data1,
        [CCode (array_length_type = "size_t")] uint8[] data2
    );

    [CCode (cname = "proven_crypto_random_bytes")]
    public static int32 crypto_random_bytes ([CCode (array_length_type = "size_t")] uint8[] buffer);

    /* ================================================================
     * SafeUrl
     * ================================================================ */

    [CCode (cname = "ProvenUrlComponents", has_type_id = false, destroy_function = "")]
    public struct UrlComponents {
        public char*  scheme;       public size_t scheme_len;
        public char*  host;         public size_t host_len;
        public uint16 port;         public bool   has_port;
        public char*  path;         public size_t path_len;
        public char*  query;        public size_t query_len;
        public char*  fragment;     public size_t fragment_len;
    }

    [CCode (cname = "ProvenUrlResult", has_type_id = false, destroy_function = "")]
    public struct UrlResult {
        public int32         status;
        public UrlComponents components;
    }

    [CCode (cname = "proven_url_parse")]
    public static UrlResult url_parse ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_url_free")]
    public static void url_free (UrlComponents* components);

    /* ================================================================
     * SafeEmail
     * ================================================================ */

    [CCode (cname = "proven_email_is_valid")]
    public static BoolResult email_is_valid ([CCode (array_length_type = "size_t")] uint8[] data);

    /* ================================================================
     * SafeNetwork
     * ================================================================ */

    [CCode (cname = "ProvenIPv4Address", has_type_id = false)]
    public struct IPv4Address {
        [CCode (array_length = false)]
        public uint8 octets[4];
    }

    [CCode (cname = "ProvenIPv4Result", has_type_id = false)]
    public struct IPv4Result {
        public int32       status;
        public IPv4Address address;
    }

    [CCode (cname = "proven_network_parse_ipv4")]
    public static IPv4Result network_parse_ipv4 ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_network_ipv4_is_private")]
    public static bool network_ipv4_is_private (IPv4Address addr);

    [CCode (cname = "proven_network_ipv4_is_loopback")]
    public static bool network_ipv4_is_loopback (IPv4Address addr);

    /* ================================================================
     * SafeJson
     * ================================================================ */

    [CCode (cname = "ProvenJsonType", cprefix = "PROVEN_JSON_", has_type_id = false)]
    public enum JsonType {
        [CCode (cname = "PROVEN_JSON_NULL")]
        NULL = 0,
        [CCode (cname = "PROVEN_JSON_BOOL")]
        BOOL = 1,
        [CCode (cname = "PROVEN_JSON_NUMBER")]
        NUMBER = 2,
        [CCode (cname = "PROVEN_JSON_STRING")]
        STRING = 3,
        [CCode (cname = "PROVEN_JSON_ARRAY")]
        ARRAY = 4,
        [CCode (cname = "PROVEN_JSON_OBJECT")]
        OBJECT = 5,
        [CCode (cname = "PROVEN_JSON_INVALID")]
        INVALID = -1
    }

    [CCode (cname = "proven_json_is_valid")]
    public static BoolResult json_is_valid ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_json_get_type")]
    public static JsonType json_get_type ([CCode (array_length_type = "size_t")] uint8[] data);

    /* ================================================================
     * SafeDateTime
     * ================================================================ */

    [CCode (cname = "ProvenDateTime", has_type_id = false)]
    public struct DateTime {
        public int32  year;
        public uint8  month;
        public uint8  day;
        public uint8  hour;
        public uint8  minute;
        public uint8  second;
        public uint32 nanosecond;
        public int16  tz_offset_minutes;
    }

    [CCode (cname = "ProvenDateTimeResult", has_type_id = false)]
    public struct DateTimeResult {
        public int32    status;
        public DateTime datetime;
    }

    [CCode (cname = "proven_datetime_parse")]
    public static DateTimeResult datetime_parse ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_datetime_format_iso8601")]
    public static StringResult datetime_format_iso8601 (DateTime dt);

    [CCode (cname = "proven_datetime_is_leap_year")]
    public static bool datetime_is_leap_year (int32 year);

    [CCode (cname = "proven_datetime_days_in_month")]
    public static uint8 datetime_days_in_month (int32 year, uint8 month);

    /* ================================================================
     * SafeFloat
     * ================================================================ */

    [CCode (cname = "proven_float_div")]
    public static FloatResult float_div (double a, double b);

    [CCode (cname = "proven_float_is_finite")]
    public static bool float_is_finite (double x);

    [CCode (cname = "proven_float_is_nan")]
    public static bool float_is_nan (double x);

    [CCode (cname = "proven_float_sqrt")]
    public static FloatResult float_sqrt (double x);

    [CCode (cname = "proven_float_ln")]
    public static FloatResult float_ln (double x);

    /* ================================================================
     * SafeHex
     * ================================================================ */

    [CCode (cname = "ProvenHexDecodeResult", has_type_id = false, destroy_function = "")]
    public struct HexDecodeResult {
        public int32   status;
        public uint8*  data;
        public size_t  length;
    }

    [CCode (cname = "proven_hex_encode")]
    public static StringResult hex_encode ([CCode (array_length_type = "size_t")] uint8[] data, bool uppercase);

    [CCode (cname = "proven_hex_decode")]
    public static HexDecodeResult hex_decode ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_hex_free")]
    public static void hex_free (HexDecodeResult* result);

    /* ================================================================
     * SafeHeader
     * ================================================================ */

    [CCode (cname = "proven_header_has_crlf")]
    public static BoolResult header_has_crlf ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_header_is_valid_name")]
    public static BoolResult header_is_valid_name ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_header_is_dangerous")]
    public static BoolResult header_is_dangerous ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_header_render")]
    public static StringResult header_render (
        [CCode (array_length_type = "size_t")] uint8[] name_data,
        [CCode (array_length_type = "size_t")] uint8[] value_data
    );

    [CCode (cname = "proven_header_build_csp")]
    public static StringResult header_build_csp ([CCode (array_length_type = "size_t")] uint8[] json_data);

    [CCode (cname = "proven_header_build_hsts")]
    public static StringResult header_build_hsts (int64 max_age, bool include_subdomains, bool preload);

    /* ================================================================
     * SafePassword
     * ================================================================ */

    [CCode (cname = "ProvenPasswordStrength", cprefix = "PROVEN_PASSWORD_", has_type_id = false)]
    public enum PasswordStrength {
        [CCode (cname = "PROVEN_PASSWORD_VERY_WEAK")]
        VERY_WEAK = 0,
        [CCode (cname = "PROVEN_PASSWORD_WEAK")]
        WEAK = 1,
        [CCode (cname = "PROVEN_PASSWORD_FAIR")]
        FAIR = 2,
        [CCode (cname = "PROVEN_PASSWORD_STRONG")]
        STRONG = 3,
        [CCode (cname = "PROVEN_PASSWORD_VERY_STRONG")]
        VERY_STRONG = 4
    }

    [CCode (cname = "ProvenPasswordResult", has_type_id = false)]
    public struct PasswordResult {
        public PasswordStrength strength;
        public bool   has_lowercase;
        public bool   has_uppercase;
        public bool   has_digit;
        public bool   has_special;
        public size_t length;
    }

    [CCode (cname = "proven_password_validate")]
    public static PasswordResult password_validate ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_password_is_common")]
    public static bool password_is_common ([CCode (array_length_type = "size_t")] uint8[] data);

    /* ================================================================
     * SafeColor
     * ================================================================ */

    [CCode (cname = "ProvenRGBColor", has_type_id = false)]
    public struct RGBColor {
        public uint8 r;
        public uint8 g;
        public uint8 b;
    }

    [CCode (cname = "ProvenHSLColor", has_type_id = false)]
    public struct HSLColor {
        public double h;
        public double s;
        public double l;
    }

    [CCode (cname = "ProvenColorResult", has_type_id = false)]
    public struct ColorResult {
        public int32    status;
        public RGBColor color;
    }

    [CCode (cname = "proven_color_parse_hex")]
    public static ColorResult color_parse_hex ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_color_rgb_to_hsl")]
    public static HSLColor color_rgb_to_hsl (RGBColor rgb);

    [CCode (cname = "proven_color_to_hex")]
    public static StringResult color_to_hex (RGBColor rgb);

    /* ================================================================
     * SafeAngle
     * ================================================================ */

    [CCode (cname = "proven_angle_deg_to_rad")]
    public static double angle_deg_to_rad (double degrees);

    [CCode (cname = "proven_angle_rad_to_deg")]
    public static double angle_rad_to_deg (double radians);

    [CCode (cname = "proven_angle_normalize_degrees")]
    public static double angle_normalize_degrees (double degrees);

    [CCode (cname = "proven_angle_normalize_radians")]
    public static double angle_normalize_radians (double radians);

    /* ================================================================
     * SafeUnit
     * ================================================================ */

    [CCode (cname = "ProvenLengthUnit", cprefix = "PROVEN_LENGTH_", has_type_id = false)]
    public enum LengthUnit {
        [CCode (cname = "PROVEN_LENGTH_METERS")]
        METERS = 0,
        [CCode (cname = "PROVEN_LENGTH_KILOMETERS")]
        KILOMETERS = 1,
        [CCode (cname = "PROVEN_LENGTH_CENTIMETERS")]
        CENTIMETERS = 2,
        [CCode (cname = "PROVEN_LENGTH_MILLIMETERS")]
        MILLIMETERS = 3,
        [CCode (cname = "PROVEN_LENGTH_FEET")]
        FEET = 4,
        [CCode (cname = "PROVEN_LENGTH_INCHES")]
        INCHES = 5,
        [CCode (cname = "PROVEN_LENGTH_MILES")]
        MILES = 6,
        [CCode (cname = "PROVEN_LENGTH_YARDS")]
        YARDS = 7
    }

    [CCode (cname = "ProvenTempUnit", cprefix = "PROVEN_TEMP_", has_type_id = false)]
    public enum TempUnit {
        [CCode (cname = "PROVEN_TEMP_CELSIUS")]
        CELSIUS = 0,
        [CCode (cname = "PROVEN_TEMP_FAHRENHEIT")]
        FAHRENHEIT = 1,
        [CCode (cname = "PROVEN_TEMP_KELVIN")]
        KELVIN = 2
    }

    [CCode (cname = "proven_unit_convert_length")]
    public static FloatResult unit_convert_length (double value, LengthUnit from, LengthUnit to);

    [CCode (cname = "proven_unit_convert_temp")]
    public static FloatResult unit_convert_temp (double value, TempUnit from, TempUnit to);

    /* ================================================================
     * SafeChecksum
     * ================================================================ */

    [CCode (cname = "proven_checksum_crc32")]
    public static IntResult checksum_crc32 ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_checksum_verify_crc32")]
    public static BoolResult checksum_verify_crc32 ([CCode (array_length_type = "size_t")] uint8[] data, uint32 expected);

    /* ================================================================
     * SafeVersion
     * ================================================================ */

    [CCode (cname = "ProvenSemanticVersion", has_type_id = false, destroy_function = "")]
    public struct SemanticVersion {
        public uint32 major;
        public uint32 minor;
        public uint32 patch;
        public size_t prerelease_len;
        public char*  prerelease;
    }

    [CCode (cname = "ProvenVersionResult", has_type_id = false, destroy_function = "")]
    public struct VersionResult {
        public int32           status;
        public SemanticVersion version;
    }

    [CCode (cname = "proven_version_parse")]
    public static VersionResult version_parse ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_version_compare")]
    public static int32 version_compare (SemanticVersion a, SemanticVersion b);

    [CCode (cname = "proven_version_free")]
    public static void version_free (SemanticVersion* version);

    /* ================================================================
     * SafeGeo
     * ================================================================ */

    [CCode (cname = "ProvenGeoCoordinate", has_type_id = false)]
    public struct GeoCoordinate {
        public double latitude;
        public double longitude;
    }

    [CCode (cname = "ProvenGeoResult", has_type_id = false)]
    public struct GeoResult {
        public int32         status;
        public GeoCoordinate coordinate;
    }

    [CCode (cname = "proven_geo_validate")]
    public static GeoResult geo_validate (double lat, double lon);

    [CCode (cname = "proven_geo_distance")]
    public static FloatResult geo_distance (GeoCoordinate a, GeoCoordinate b);

    [CCode (cname = "proven_geo_in_bounds")]
    public static bool geo_in_bounds (
        GeoCoordinate coord,
        double min_lat, double max_lat,
        double min_lon, double max_lon
    );

    /* ================================================================
     * SafeProbability
     * ================================================================ */

    [CCode (cname = "proven_probability_create")]
    public static double probability_create (double value);

    [CCode (cname = "proven_probability_and")]
    public static double probability_and (double a, double b);

    [CCode (cname = "proven_probability_or_exclusive")]
    public static double probability_or_exclusive (double a, double b);

    [CCode (cname = "proven_probability_not")]
    public static double probability_not (double p);

    /* ================================================================
     * SafeCalculator
     * ================================================================ */

    [CCode (cname = "proven_calculator_eval")]
    public static FloatResult calculator_eval ([CCode (array_length_type = "size_t")] uint8[] data);

    /* ================================================================
     * SafeUUID
     * ================================================================ */

    [CCode (cname = "ProvenUUID", has_type_id = false)]
    public struct UUID {
        [CCode (array_length = false)]
        public uint8 bytes[16];
    }

    [CCode (cname = "ProvenUUIDResult", has_type_id = false)]
    public struct UUIDResult {
        public int32 status;
        public UUID  uuid;
    }

    [CCode (cname = "proven_uuid_v4")]
    public static UUIDResult uuid_v4 ();

    [CCode (cname = "proven_uuid_to_string")]
    public static StringResult uuid_to_string (UUID uuid);

    [CCode (cname = "proven_uuid_parse")]
    public static UUIDResult uuid_parse ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_uuid_is_nil")]
    public static bool uuid_is_nil (UUID uuid);

    [CCode (cname = "proven_uuid_version")]
    public static uint8 uuid_version (UUID uuid);

    /* ================================================================
     * SafeHttp
     * ================================================================ */

    [CCode (cname = "proven_http_url_encode")]
    public static StringResult http_url_encode ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_http_url_decode")]
    public static StringResult http_url_decode ([CCode (array_length_type = "size_t")] uint8[] data);

    /* ================================================================
     * SafeCookie
     * ================================================================ */

    [CCode (cname = "ProvenSameSite", cprefix = "PROVEN_SAMESITE_", has_type_id = false)]
    public enum SameSite {
        [CCode (cname = "PROVEN_SAMESITE_STRICT")]
        STRICT = 0,
        [CCode (cname = "PROVEN_SAMESITE_LAX")]
        LAX = 1,
        [CCode (cname = "PROVEN_SAMESITE_NONE")]
        NONE = 2
    }

    [CCode (cname = "ProvenCookieAttributes", has_type_id = false)]
    public struct CookieAttributes {
        public uint8*   domain;       public size_t domain_len;
        public uint8*   path;         public size_t path_len;
        public int64    max_age;
        public bool     secure;
        public bool     http_only;
        public SameSite same_site;
        public bool     partitioned;
    }

    [CCode (cname = "proven_cookie_has_injection")]
    public static BoolResult cookie_has_injection ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_cookie_validate_name")]
    public static BoolResult cookie_validate_name ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_cookie_validate_value")]
    public static BoolResult cookie_validate_value ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_cookie_get_prefix")]
    public static IntResult cookie_get_prefix ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_cookie_build_set_cookie")]
    public static StringResult cookie_build_set_cookie (
        [CCode (array_length_type = "size_t")] uint8[] name_data,
        [CCode (array_length_type = "size_t")] uint8[] value_data,
        CookieAttributes attrs
    );

    [CCode (cname = "proven_cookie_build_delete")]
    public static StringResult cookie_build_delete ([CCode (array_length_type = "size_t")] uint8[] name_data);

    /* ================================================================
     * SafeContentType
     * ================================================================ */

    [CCode (cname = "ProvenMediaCategory", cprefix = "PROVEN_MEDIA_", has_type_id = false)]
    public enum MediaCategory {
        [CCode (cname = "PROVEN_MEDIA_TEXT")]
        TEXT = 0,
        [CCode (cname = "PROVEN_MEDIA_IMAGE")]
        IMAGE = 1,
        [CCode (cname = "PROVEN_MEDIA_AUDIO")]
        AUDIO = 2,
        [CCode (cname = "PROVEN_MEDIA_VIDEO")]
        VIDEO = 3,
        [CCode (cname = "PROVEN_MEDIA_APPLICATION")]
        APPLICATION = 4,
        [CCode (cname = "PROVEN_MEDIA_MULTIPART")]
        MULTIPART = 5,
        [CCode (cname = "PROVEN_MEDIA_MESSAGE")]
        MESSAGE = 6,
        [CCode (cname = "PROVEN_MEDIA_FONT")]
        FONT = 7,
        [CCode (cname = "PROVEN_MEDIA_MODEL")]
        MODEL = 8,
        [CCode (cname = "PROVEN_MEDIA_CUSTOM")]
        CUSTOM = 9
    }

    [CCode (cname = "ProvenCharset", cprefix = "PROVEN_CHARSET_", has_type_id = false)]
    public enum Charset {
        [CCode (cname = "PROVEN_CHARSET_UTF8")]
        UTF8 = 0,
        [CCode (cname = "PROVEN_CHARSET_UTF16LE")]
        UTF16LE = 1,
        [CCode (cname = "PROVEN_CHARSET_UTF16BE")]
        UTF16BE = 2,
        [CCode (cname = "PROVEN_CHARSET_ISO8859_1")]
        ISO8859_1 = 3,
        [CCode (cname = "PROVEN_CHARSET_ASCII")]
        ASCII = 4,
        [CCode (cname = "PROVEN_CHARSET_WINDOWS1252")]
        WINDOWS1252 = 5,
        [CCode (cname = "PROVEN_CHARSET_OTHER")]
        OTHER = 6
    }

    [CCode (cname = "ProvenContentTypeResult", has_type_id = false, destroy_function = "")]
    public struct ContentTypeResult {
        public int32         status;
        public char*         media_type;     public size_t media_type_len;
        public char*         subtype;        public size_t subtype_len;
        public char*         suffix;         public size_t suffix_len;
        public MediaCategory category;
        public Charset       charset;
        public bool          has_charset;
    }

    [CCode (cname = "proven_content_type_parse")]
    public static ContentTypeResult content_type_parse ([CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_content_type_free")]
    public static void content_type_free (ContentTypeResult* result);

    [CCode (cname = "proven_content_type_can_sniff_dangerous")]
    public static BoolResult content_type_can_sniff_dangerous ([CCode (array_length_type = "size_t")] uint8[] data);

    /* ================================================================
     * SafeBuffer
     * ================================================================ */

    [CCode (cname = "ProvenBoundedBuffer", has_type_id = false, destroy_function = "")]
    public struct BoundedBuffer {
        public uint8* data;
        public size_t capacity;
        public size_t length;
    }

    [CCode (cname = "ProvenBufferResult", has_type_id = false)]
    public struct BufferResult {
        public int32          status;
        public BoundedBuffer* buffer;
    }

    [CCode (cname = "proven_buffer_create")]
    public static BufferResult buffer_create (size_t capacity);

    [CCode (cname = "proven_buffer_append")]
    public static int32 buffer_append (BoundedBuffer* buffer, [CCode (array_length_type = "size_t")] uint8[] data);

    [CCode (cname = "proven_buffer_free")]
    public static void buffer_free (BoundedBuffer* buffer);

    /* ================================================================
     * SafeRateLimiter
     * ================================================================ */

    [CCode (cname = "ProvenRateLimiter", has_type_id = false, destroy_function = "")]
    public struct RateLimiter {
        public double tokens;
        public double capacity;
        public double refill_rate;
        public int64  last_refill;
    }

    [CCode (cname = "proven_rate_limiter_create")]
    public static RateLimiter* rate_limiter_create (double capacity, double refill_rate);

    [CCode (cname = "proven_rate_limiter_try_acquire")]
    public static bool rate_limiter_try_acquire (RateLimiter* limiter, double tokens);

    [CCode (cname = "proven_rate_limiter_free")]
    public static void rate_limiter_free (RateLimiter* limiter);

    /* ================================================================
     * SafeCircuitBreaker
     * ================================================================ */

    [CCode (cname = "ProvenCircuitState", cprefix = "PROVEN_CIRCUIT_", has_type_id = false)]
    public enum CircuitState {
        [CCode (cname = "PROVEN_CIRCUIT_CLOSED")]
        CLOSED = 0,
        [CCode (cname = "PROVEN_CIRCUIT_OPEN")]
        OPEN = 1,
        [CCode (cname = "PROVEN_CIRCUIT_HALF_OPEN")]
        HALF_OPEN = 2
    }

    [CCode (cname = "ProvenCircuitBreaker", has_type_id = false, destroy_function = "")]
    public struct CircuitBreaker {
        public CircuitState state;
        public uint32 failure_count;
        public uint32 failure_threshold;
        public uint32 success_count;
        public uint32 success_threshold;
        public int64  last_failure;
        public int64  timeout_ms;
    }

    [CCode (cname = "proven_circuit_breaker_create")]
    public static CircuitBreaker* circuit_breaker_create (
        uint32 failure_threshold, uint32 success_threshold, int64 timeout_ms
    );

    [CCode (cname = "proven_circuit_breaker_allow")]
    public static bool circuit_breaker_allow (CircuitBreaker* cb);

    [CCode (cname = "proven_circuit_breaker_success")]
    public static void circuit_breaker_success (CircuitBreaker* cb);

    [CCode (cname = "proven_circuit_breaker_failure")]
    public static void circuit_breaker_failure (CircuitBreaker* cb);

    [CCode (cname = "proven_circuit_breaker_state")]
    public static CircuitState circuit_breaker_state (CircuitBreaker* cb);

    [CCode (cname = "proven_circuit_breaker_free")]
    public static void circuit_breaker_free (CircuitBreaker* cb);

    /* ================================================================
     * SafeRetry
     * ================================================================ */

    [CCode (cname = "ProvenRetryConfig", has_type_id = false)]
    public struct RetryConfig {
        public uint32 max_attempts;
        public uint64 base_delay_ms;
        public uint64 max_delay_ms;
        public double multiplier;
    }

    [CCode (cname = "proven_retry_delay")]
    public static uint64 retry_delay (RetryConfig config, uint32 attempt);

    [CCode (cname = "proven_retry_should_retry")]
    public static bool retry_should_retry (RetryConfig config, uint32 attempt);

    /* ================================================================
     * SafeMonotonic
     * ================================================================ */

    [CCode (cname = "ProvenMonotonicCounter", has_type_id = false, destroy_function = "")]
    public struct MonotonicCounter {
        public uint64 value;
        public uint64 max_value;
    }

    [CCode (cname = "proven_monotonic_create")]
    public static MonotonicCounter* monotonic_create (uint64 initial, uint64 max_value);

    [CCode (cname = "proven_monotonic_next")]
    public static IntResult monotonic_next (MonotonicCounter* counter);

    [CCode (cname = "proven_monotonic_free")]
    public static void monotonic_free (MonotonicCounter* counter);
}
