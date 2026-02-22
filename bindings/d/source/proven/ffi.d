// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Proven FFI declarations - extern(C) bindings to libproven.
 *
 * This module declares all C-ABI functions and types exported by libproven.
 * The library is implemented in Idris 2 (formally verified) with a Zig FFI
 * bridge exposing a stable C ABI. Do NOT reimplement any logic in D.
 *
 * Memory management rules:
 *   - ProvenStringResult.value must be freed with proven_free_string()
 *   - ProvenUrlResult must be freed with proven_url_free()
 *   - ProvenContentTypeResult must be freed with proven_content_type_free()
 *   - ProvenHexDecodeResult must be freed with proven_hex_free()
 *   - Integer, boolean, and float results do not require freeing
 */
module proven.ffi;

pragma(lib, "proven");

// ============================================================================
// Status Codes
// ============================================================================

/// Status codes returned by Proven operations.
enum ProvenStatus : int
{
    ok                   =   0,
    errNullPointer       =  -1,
    errInvalidArgument   =  -2,
    errOverflow          =  -3,
    errUnderflow         =  -4,
    errDivisionByZero    =  -5,
    errParseFailure      =  -6,
    errValidationFailed  =  -7,
    errOutOfBounds       =  -8,
    errEncodingError     =  -9,
    errAllocationFailed  = -10,
    errNotImplemented    = -99,
}

// ============================================================================
// Core Result Types
// ============================================================================

/// Result for integer operations.
struct ProvenIntResult
{
    int status;
    long value;
}

/// Result for boolean operations.
struct ProvenBoolResult
{
    int status;
    bool value;
}

/// Result for string operations. Caller must free value with proven_free_string().
struct ProvenStringResult
{
    int status;
    char* value;
    size_t length;
}

/// Result for floating-point operations.
struct ProvenFloatResult
{
    int status;
    double value;
}

// ============================================================================
// Complex Types
// ============================================================================

/// URL components structure returned by proven_url_parse().
struct ProvenUrlComponents
{
    char* scheme;       size_t scheme_len;
    char* host;         size_t host_len;
    ushort port;        bool has_port;
    char* path;         size_t path_len;
    char* query;        size_t query_len;
    char* fragment;     size_t fragment_len;
}

/// Result for URL parsing.
struct ProvenUrlResult
{
    int status;
    ProvenUrlComponents components;
}

/// IPv4 address structure.
struct ProvenIPv4Address
{
    ubyte[4] octets;
}

/// Result for IPv4 parsing.
struct ProvenIPv4Result
{
    int status;
    ProvenIPv4Address address;
}

/// UUID structure (128-bit identifier).
struct ProvenUUID
{
    ubyte[16] bytes;
}

/// UUID result.
struct ProvenUUIDResult
{
    int status;
    ProvenUUID uuid;
}

/// JSON value type.
enum ProvenJsonType : int
{
    null_   =  0,
    bool_   =  1,
    number  =  2,
    string_ =  3,
    array   =  4,
    object  =  5,
    invalid = -1,
}

/// DateTime components.
struct ProvenDateTime
{
    int year;
    ubyte month;
    ubyte day;
    ubyte hour;
    ubyte minute;
    ubyte second;
    uint nanosecond;
    short tz_offset_minutes;
}

/// DateTime result.
struct ProvenDateTimeResult
{
    int status;
    ProvenDateTime datetime;
}

/// Semantic version structure.
struct ProvenSemanticVersion
{
    uint major;
    uint minor;
    uint patch;
    size_t prerelease_len;
    char* prerelease;
}

/// Version parsing result.
struct ProvenVersionResult
{
    int status;
    ProvenSemanticVersion version_;
}

/// RGB color (8-bit per channel).
struct ProvenRGBColor
{
    ubyte r;
    ubyte g;
    ubyte b;
}

/// HSL color.
struct ProvenHSLColor
{
    double h;
    double s;
    double l;
}

/// Color parse result.
struct ProvenColorResult
{
    int status;
    ProvenRGBColor color;
}

/// Currency parse result.
struct ProvenCurrencyResult
{
    int status;
    long amount_minor;
    ubyte[3] currency_code;
    ubyte decimal_places;
}

/// Phone number parse result.
struct ProvenPhoneResult
{
    int status;
    ushort country_code;
    ulong national_number;
    bool is_valid;
}

/// Hex decode result with byte data.
struct ProvenHexDecodeResult
{
    int status;
    ubyte* data;
    size_t length;
}

// ============================================================================
// Extern(C) Function Declarations
// ============================================================================

extern (C) nothrow @nogc
{
    // -- Memory Management --
    void proven_free_string(char* ptr);

    // -- Lifecycle --
    int proven_init();
    void proven_deinit();
    bool proven_is_initialized();

    // -- Version Information --
    uint proven_ffi_abi_version();
    uint proven_version_major();
    uint proven_version_minor();
    uint proven_version_patch();
    uint proven_module_count();

    // -- SafeMath --
    ProvenIntResult proven_math_div(long numerator, long denominator);
    ProvenIntResult proven_math_mod(long numerator, long denominator);
    ProvenIntResult proven_math_add_checked(long a, long b);
    ProvenIntResult proven_math_sub_checked(long a, long b);
    ProvenIntResult proven_math_mul_checked(long a, long b);
    ProvenIntResult proven_math_abs_safe(long n);
    long proven_math_clamp(long lo, long hi, long value);
    ProvenIntResult proven_math_pow_checked(long base, uint exp);

    // -- SafeString --
    ProvenBoolResult proven_string_is_valid_utf8(const(ubyte)* ptr, size_t len);
    ProvenStringResult proven_string_escape_sql(const(ubyte)* ptr, size_t len);
    ProvenStringResult proven_string_escape_html(const(ubyte)* ptr, size_t len);
    ProvenStringResult proven_string_escape_js(const(ubyte)* ptr, size_t len);

    // -- SafePath --
    ProvenBoolResult proven_path_has_traversal(const(ubyte)* ptr, size_t len);
    ProvenStringResult proven_path_sanitize_filename(const(ubyte)* ptr, size_t len);

    // -- SafeCrypto --
    ProvenBoolResult proven_crypto_constant_time_eq(
        const(ubyte)* ptr1, size_t len1,
        const(ubyte)* ptr2, size_t len2
    );
    ProvenStatus proven_crypto_random_bytes(ubyte* ptr, size_t len);

    // -- SafeUrl --
    ProvenUrlResult proven_url_parse(const(ubyte)* ptr, size_t len);
    void proven_url_free(ProvenUrlComponents* components);

    // -- SafeEmail --
    ProvenBoolResult proven_email_is_valid(const(ubyte)* ptr, size_t len);

    // -- SafeNetwork --
    ProvenIPv4Result proven_network_parse_ipv4(const(ubyte)* ptr, size_t len);
    bool proven_network_ipv4_is_private(ProvenIPv4Address addr);
    bool proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

    // -- SafeUUID --
    ProvenUUIDResult proven_uuid_v4();
    ProvenStringResult proven_uuid_to_string(ProvenUUID uuid);
    ProvenUUIDResult proven_uuid_parse(const(ubyte)* ptr, size_t len);
    bool proven_uuid_is_nil(ProvenUUID uuid);
    ubyte proven_uuid_version(ProvenUUID uuid);

    // -- SafeJson --
    ProvenBoolResult proven_json_is_valid(const(ubyte)* ptr, size_t len);
    ProvenJsonType proven_json_get_type(const(ubyte)* ptr, size_t len);

    // -- SafeDateTime --
    ProvenDateTimeResult proven_datetime_parse(const(ubyte)* ptr, size_t len);
    ProvenStringResult proven_datetime_format_iso8601(ProvenDateTime dt);
    bool proven_datetime_is_leap_year(int year);
    ubyte proven_datetime_days_in_month(int year, ubyte month);

    // -- SafeFloat --
    ProvenFloatResult proven_float_div(double a, double b);
    bool proven_float_is_finite(double x);
    bool proven_float_is_nan(double x);
    ProvenFloatResult proven_float_sqrt(double x);
    ProvenFloatResult proven_float_ln(double x);

    // -- SafeVersion --
    ProvenVersionResult proven_version_parse(const(ubyte)* ptr, size_t len);
    int proven_version_compare(ProvenSemanticVersion a, ProvenSemanticVersion b);
    void proven_version_free(ProvenSemanticVersion* version_);

    // -- SafeColor --
    ProvenColorResult proven_color_parse_hex(const(ubyte)* ptr, size_t len);
    ProvenHSLColor proven_color_rgb_to_hsl(ProvenRGBColor rgb);
    ProvenStringResult proven_color_to_hex(ProvenRGBColor rgb);

    // -- SafeAngle --
    double proven_angle_deg_to_rad(double degrees);
    double proven_angle_rad_to_deg(double radians);
    double proven_angle_normalize_degrees(double degrees);
    double proven_angle_normalize_radians(double radians);

    // -- SafeHex --
    ProvenStringResult proven_hex_encode(const(ubyte)* ptr, size_t len, bool uppercase);
    ProvenHexDecodeResult proven_hex_decode(const(ubyte)* ptr, size_t len);
    void proven_hex_free(ProvenHexDecodeResult* result);

    // -- SafeCurrency --
    ProvenCurrencyResult proven_currency_parse(const(ubyte)* ptr, size_t len);
    ProvenStringResult proven_currency_format(long amount_minor, ref ubyte[3] code, ubyte decimal_places);

    // -- SafePhone --
    ProvenPhoneResult proven_phone_parse(const(ubyte)* ptr, size_t len);
    ProvenStringResult proven_phone_format_e164(ushort country_code, ulong national_number);

    // -- SafeHttp --
    ProvenStringResult proven_http_url_encode(const(ubyte)* ptr, size_t len);
    ProvenStringResult proven_http_url_decode(const(ubyte)* ptr, size_t len);
}

// ============================================================================
// Convenience Helpers
// ============================================================================

/// Check if a status code indicates success.
bool provenSucceeded(int status) pure nothrow @safe @nogc
{
    return status == ProvenStatus.ok;
}

/// Check if a status code indicates failure.
bool provenFailed(int status) pure nothrow @safe @nogc
{
    return status != ProvenStatus.ok;
}

/// Convert a ProvenStringResult to a D string, freeing the C memory.
/// Returns null string on failure.
string provenStringToD(ProvenStringResult result) @trusted nothrow
{
    if (provenFailed(result.status) || result.value is null)
        return null;

    // Copy the C string into a D-managed string
    string dstr;
    if (result.length > 0)
        dstr = result.value[0 .. result.length].idup;
    else
        dstr = "";

    // Free the C-allocated memory
    proven_free_string(result.value);
    return dstr;
}
