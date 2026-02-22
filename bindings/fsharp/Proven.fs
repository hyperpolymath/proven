// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>


/// Proven FFI bindings for F# - thin P/Invoke wrappers over libproven.
///
/// ALL computation is performed in verified Idris 2 code via the Zig FFI
/// bridge. This module contains ONLY marshaling logic. Do NOT reimplement
/// any algorithms here.
///
/// Memory management:
///   - StringResult values must be freed with proven_free_string
///   - Opaque pointer types have matching *_free functions
///   - Integer, boolean, and float results do not require freeing
namespace Proven

open System
open System.Runtime.InteropServices
open System.Text

// ============================================================================
// Status Codes
// ============================================================================

/// Status codes returned by Proven FFI operations.
/// Zero indicates success; negative values indicate specific error conditions.
type ProvenStatus =
    | Ok = 0
    | ErrNullPointer = -1
    | ErrInvalidArgument = -2
    | ErrOverflow = -3
    | ErrUnderflow = -4
    | ErrDivisionByZero = -5
    | ErrParseFailure = -6
    | ErrValidationFailed = -7
    | ErrOutOfBounds = -8
    | ErrEncodingError = -9
    | ErrAllocationFailed = -10
    | ErrNotImplemented = -99

// ============================================================================
// Core Result Structs (match C ABI layout)
// ============================================================================

/// Result for integer operations.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type IntResult =
    val mutable Status: int
    val mutable Value: int64

/// Result for boolean operations.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type BoolResult =
    val mutable Status: int
    val mutable Value: byte // bool in C ABI is 1 byte

/// Result for string operations. Caller must free Value with proven_free_string.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type StringResult =
    val mutable Status: int
    val mutable Value: nativeint
    val mutable Length: unativeint

/// Result for floating-point operations.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type FloatResult =
    val mutable Status: int
    val mutable Value: float

// ============================================================================
// Domain-specific Structs
// ============================================================================

/// IPv4 address (4 bytes).
[<Struct; StructLayout(LayoutKind.Sequential)>]
type IPv4Address =
    [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)>]
    val mutable Octets: byte array

/// Result for IPv4 parsing.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type IPv4Result =
    val mutable Status: int
    val mutable Address: IPv4Address

/// UUID (16 bytes).
[<Struct; StructLayout(LayoutKind.Sequential)>]
type ProvenUUID =
    [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 16)>]
    val mutable Bytes: byte array

/// UUID result.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type UUIDResult =
    val mutable Status: int
    val mutable UUID: ProvenUUID

/// Currency result.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type CurrencyResult =
    val mutable Status: int
    val mutable AmountMinor: int64
    [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)>]
    val mutable CurrencyCode: byte array
    val mutable DecimalPlaces: byte

/// Phone result.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type PhoneResult =
    val mutable Status: int
    val mutable CountryCode: uint16
    val mutable NationalNumber: uint64
    val mutable IsValid: byte

/// DateTime components.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type ProvenDateTime =
    val mutable Year: int32
    val mutable Month: byte
    val mutable Day: byte
    val mutable Hour: byte
    val mutable Minute: byte
    val mutable Second: byte
    val mutable Nanosecond: uint32
    val mutable TzOffsetMinutes: int16

/// DateTime result.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type DateTimeResult =
    val mutable Status: int
    val mutable DateTime: ProvenDateTime

/// Semantic version.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type SemanticVersion =
    val mutable Major: uint32
    val mutable Minor: uint32
    val mutable Patch: uint32
    val mutable PrereleaseLen: unativeint
    val mutable Prerelease: nativeint

/// Version result.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type VersionResult =
    val mutable Status: int
    val mutable Version: SemanticVersion

/// Geographic coordinate.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type GeoCoordinate =
    val mutable Latitude: float
    val mutable Longitude: float

/// Geo result.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type GeoResult =
    val mutable Status: int
    val mutable Coordinate: GeoCoordinate

/// Password strength level.
type PasswordStrength =
    | VeryWeak = 0
    | Weak = 1
    | Fair = 2
    | Strong = 3
    | VeryStrong = 4

/// Password validation result.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type PasswordResult =
    val mutable Strength: int
    val mutable HasLowercase: byte
    val mutable HasUppercase: byte
    val mutable HasDigit: byte
    val mutable HasSpecial: byte
    val mutable Length: unativeint

/// Hex decode result.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type HexDecodeResult =
    val mutable Status: int
    val mutable Data: nativeint
    val mutable Length: unativeint

/// RGB color.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type RGBColor =
    val mutable R: byte
    val mutable G: byte
    val mutable B: byte

/// HSL color.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type HSLColor =
    val mutable H: float
    val mutable S: float
    val mutable L: float

/// Color parse result.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type ColorParseResult =
    val mutable Status: int
    val mutable Color: RGBColor

/// Retry configuration.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type RetryConfig =
    val mutable MaxAttempts: uint32
    val mutable BaseDelayMs: uint64
    val mutable MaxDelayMs: uint64
    val mutable Multiplier: float

/// Circuit breaker state.
type CircuitState =
    | Closed = 0
    | Open = 1
    | HalfOpen = 2

/// SameSite attribute.
type SameSite =
    | Strict = 0
    | Lax = 1
    | None = 2

/// Charset encoding.
type Charset =
    | Utf8 = 0
    | Utf16LE = 1
    | Utf16BE = 2
    | Iso8859_1 = 3
    | Ascii = 4
    | Windows1252 = 5
    | Other = 6

/// Length unit.
type LengthUnit =
    | Meters = 0
    | Kilometers = 1
    | Centimeters = 2
    | Millimeters = 3
    | Feet = 4
    | Inches = 5
    | Miles = 6
    | Yards = 7

/// Temperature unit.
type TempUnit =
    | Celsius = 0
    | Fahrenheit = 1
    | Kelvin = 2

/// JSON value type.
type JsonType =
    | Null = 0
    | Bool = 1
    | Number = 2
    | String = 3
    | Array = 4
    | Object = 5
    | Invalid = -1

/// Cookie attributes for building Set-Cookie headers.
[<Struct; StructLayout(LayoutKind.Sequential)>]
type CookieAttributes =
    val mutable Domain: nativeint
    val mutable DomainLen: unativeint
    val mutable Path: nativeint
    val mutable PathLen: unativeint
    val mutable MaxAge: int64
    val mutable Secure: byte
    val mutable HttpOnly: byte
    val mutable SameSite: int
    val mutable Partitioned: byte

// ============================================================================
// Raw P/Invoke Declarations - ALL 103 exported functions from libproven
// ============================================================================

/// Low-level P/Invoke bindings to libproven. These should not be called
/// directly; use the safe wrapper modules below instead.
module FFI =

    // --- Memory Management (1) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_free_string(nativeint ptr)

    // --- Lifecycle (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_init()

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_deinit()

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_is_initialized()

    // --- Version Information (5) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 proven_ffi_abi_version()

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 proven_version_major()

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 proven_version_minor()

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 proven_version_patch()

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 proven_module_count()

    // --- SafeMath (8) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_math_div(int64 numerator, int64 denominator)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_math_mod(int64 numerator, int64 denominator)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_math_add_checked(int64 a, int64 b)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_math_sub_checked(int64 a, int64 b)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_math_mul_checked(int64 a, int64 b)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_math_abs_safe(int64 n)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int64 proven_math_clamp(int64 lo, int64 hi, int64 value)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_math_pow_checked(int64 base_, uint32 exp)

    // --- SafeString (4) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_string_is_valid_utf8(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_string_escape_sql(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_string_escape_html(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_string_escape_js(byte[] ptr, unativeint len)

    // --- SafePath (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_path_has_traversal(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_path_sanitize_filename(byte[] ptr, unativeint len)

    // --- SafeCrypto (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_crypto_constant_time_eq(byte[] ptr1, unativeint len1, byte[] ptr2, unativeint len2)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_crypto_random_bytes(byte[] ptr, unativeint len)

    // --- SafeUrl (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_url_parse(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_url_free(nativeint components)

    // --- SafeEmail (1) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_email_is_valid(byte[] ptr, unativeint len)

    // --- SafeNetwork (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IPv4Result proven_network_parse_ipv4(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_network_ipv4_is_private(IPv4Address addr)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_network_ipv4_is_loopback(IPv4Address addr)

    // --- SafeHeader (6) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_header_has_crlf(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_header_is_valid_name(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_header_is_dangerous(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_header_render(byte[] namePtr, unativeint nameLen, byte[] valuePtr, unativeint valueLen)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_header_build_csp(byte[] directivesJson, unativeint jsonLen)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_header_build_hsts(int64 maxAge, bool includeSubdomains, bool preload)

    // --- SafeCookie (6) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_cookie_has_injection(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_cookie_validate_name(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_cookie_validate_value(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_cookie_get_prefix(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_cookie_build_set_cookie(byte[] namePtr, unativeint nameLen, byte[] valuePtr, unativeint valueLen, CookieAttributes attrs)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_cookie_build_delete(byte[] namePtr, unativeint nameLen)

    // --- SafeContentType (5) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_content_type_parse(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_content_type_free(nativeint result)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_content_type_can_sniff_dangerous(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_content_type_render(byte[] typePtr, unativeint typeLen, byte[] subtypePtr, unativeint subtypeLen, byte[] suffixPtr, unativeint suffixLen, int charset, bool hasCharset)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_content_type_is_json(byte[] subtypePtr, unativeint subtypeLen, byte[] suffixPtr, unativeint suffixLen)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_content_type_is_xml(byte[] subtypePtr, unativeint subtypeLen, byte[] suffixPtr, unativeint suffixLen)

    // --- SafeUUID (5) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern UUIDResult proven_uuid_v4()

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_uuid_to_string(ProvenUUID uuid)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern UUIDResult proven_uuid_parse(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_uuid_is_nil(ProvenUUID uuid)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern byte proven_uuid_version(ProvenUUID uuid)

    // --- SafeJson (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_json_is_valid(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_json_get_type(byte[] ptr, unativeint len)

    // --- SafeDateTime (4) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern DateTimeResult proven_datetime_parse(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_datetime_format_iso8601(ProvenDateTime dt)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_datetime_is_leap_year(int32 year)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern byte proven_datetime_days_in_month(int32 year, byte month)

    // --- SafeFloat (5) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern FloatResult proven_float_div(float a, float b)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_float_is_finite(float x)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_float_is_nan(float x)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern FloatResult proven_float_sqrt(float x)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern FloatResult proven_float_ln(float x)

    // --- SafePassword (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern PasswordResult proven_password_validate(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_password_is_common(byte[] ptr, unativeint len)

    // --- SafeHex (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_hex_encode(byte[] ptr, unativeint len, bool uppercase)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern HexDecodeResult proven_hex_decode(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_hex_free(nativeint result)

    // --- SafeCurrency (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern CurrencyResult proven_currency_parse(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_currency_format(int64 amountMinor, byte[] code, byte decimalPlaces)

    // --- SafePhone (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern PhoneResult proven_phone_parse(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_phone_format_e164(uint16 countryCode, uint64 nationalNumber)

    // --- SafeVersion (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern VersionResult proven_version_parse(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_version_compare(SemanticVersion a, SemanticVersion b)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_version_free(nativeint version)

    // --- SafeGeo (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern GeoResult proven_geo_validate(float lat, float lon)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern FloatResult proven_geo_distance(GeoCoordinate a, GeoCoordinate b)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_geo_in_bounds(GeoCoordinate coord, float minLat, float maxLat, float minLon, float maxLon)

    // --- SafeChecksum (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_checksum_crc32(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_checksum_verify_crc32(byte[] ptr, unativeint len, uint32 expected)

    // --- SafeProbability (4) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_probability_create(float value)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_probability_and(float a, float b)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_probability_or_exclusive(float a, float b)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_probability_not(float p)

    // --- SafeCalculator (1) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern FloatResult proven_calculator_eval(byte[] ptr, unativeint len)

    // --- SafeBuffer (4) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_buffer_create(unativeint capacity)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_buffer_append(nativeint buffer, byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_buffer_get(nativeint buffer, nativeint& outPtr, unativeint& outLen)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_buffer_free(nativeint buffer)

    // --- SafeRateLimiter (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_rate_limiter_create(float capacity, float refillRate)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_rate_limiter_try_acquire(nativeint limiter, float tokens)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_rate_limiter_free(nativeint limiter)

    // --- SafeCircuitBreaker (6) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_circuit_breaker_create(uint32 failureThreshold, uint32 successThreshold, int64 timeoutMs)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_circuit_breaker_allow(nativeint cb)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_circuit_breaker_success(nativeint cb)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_circuit_breaker_failure(nativeint cb)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_circuit_breaker_state(nativeint cb)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_circuit_breaker_free(nativeint cb)

    // --- SafeRetry (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint64 proven_retry_delay(RetryConfig config, uint32 attempt)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_retry_should_retry(RetryConfig config, uint32 attempt)

    // --- SafeMonotonic (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_monotonic_create(uint64 initial, uint64 maxValue)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_monotonic_next(nativeint counter)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_monotonic_free(nativeint counter)

    // --- SafeStateMachine (5) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_state_machine_create(uint32 stateCount, uint32 initialState)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_state_machine_allow(nativeint sm, uint32 from_, uint32 to_)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_state_machine_transition(nativeint sm, uint32 to_)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 proven_state_machine_state(nativeint sm)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_state_machine_free(nativeint sm)

    // --- SafeTensor (5) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_tensor_create(unativeint rows, unativeint cols)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_tensor_set(nativeint tensor, unativeint row, unativeint col, float value)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern FloatResult proven_tensor_get(nativeint tensor, unativeint row, unativeint col)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_tensor_matmul(nativeint a, nativeint b)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_tensor_free(nativeint tensor)

    // --- SafeML (5) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_ml_softmax(float[] input, float[] output, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_ml_sigmoid(float x)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_ml_relu(float x)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_ml_leaky_relu(float x, float alpha)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_ml_clamp(float x, float minVal, float maxVal)

    // --- SafeLRU (4) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_lru_create(unativeint capacity)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_lru_get(nativeint cache, uint64 key)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_lru_put(nativeint cache, uint64 key, int64 value)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_lru_free(nativeint cache)

    // --- SafeGraph (4) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_graph_create(unativeint nodeCount)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_graph_add_edge(nativeint graph, unativeint from_, unativeint to_)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_graph_has_edge(nativeint graph, unativeint from_, unativeint to_)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_graph_free(nativeint graph)

    // --- SafeRegistry (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_registry_parse(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_registry_to_string(nativeint ref_)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_registry_has_registry(nativeint ref_)

    // --- SafeDigest (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_digest_parse(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern BoolResult proven_digest_verify(nativeint expected, nativeint actual)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_digest_to_string(nativeint digest)

    // --- SafeHTTP (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_http_url_encode(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_http_url_decode(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_http_parse_www_authenticate(byte[] ptr, unativeint len)

    // --- SafeColor (3) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern ColorParseResult proven_color_parse_hex(byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern HSLColor proven_color_rgb_to_hsl(RGBColor rgb)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern StringResult proven_color_to_hex(RGBColor rgb)

    // --- SafeAngle (4) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_angle_deg_to_rad(float degrees)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_angle_rad_to_deg(float radians)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_angle_normalize_degrees(float degrees)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern float proven_angle_normalize_radians(float radians)

    // --- SafeUnit (2) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern FloatResult proven_unit_convert_length(float value, int from_, int to_)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern FloatResult proven_unit_convert_temp(float value, int from_, int to_)

    // --- SafeQueue (5) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_queue_create(unativeint capacity)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_queue_push(nativeint queue, int64 value)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern IntResult proven_queue_pop(nativeint queue)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern unativeint proven_queue_size(nativeint queue)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_queue_free(nativeint queue)

    // --- SafeBloom (4) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint proven_bloom_create(unativeint expectedElements, float falsePositiveRate)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_bloom_add(nativeint filter, byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern bool proven_bloom_contains(nativeint filter, byte[] ptr, unativeint len)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern void proven_bloom_free(nativeint filter)

    // --- Callbacks (5) ---
    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 proven_callback_register(int eventType, nativeint callback, nativeint context)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_callback_unregister(uint32 handle)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern int proven_callback_fire(int eventType, nativeint dataPtr, unativeint dataLen, int code)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 proven_callback_count(int eventType)

    [<DllImport("proven", CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 proven_callback_clear_all()


// ============================================================================
// Internal Marshaling Helpers
// ============================================================================

/// Internal helpers for marshaling between .NET and C ABI types.
/// Not intended for direct use.
[<AutoOpen>]
module internal MarshalHelpers =

    /// Convert a .NET string to a UTF-8 byte array for passing to FFI.
    let toUtf8 (s: string) : byte array =
        Encoding.UTF8.GetBytes(s)

    /// Check if an IntResult succeeded and extract its value.
    let intResultToOption (r: IntResult) : int64 option =
        if r.Status = 0 then Some r.Value
        else None

    /// Check if a BoolResult succeeded and extract its value.
    let boolResultToOption (r: BoolResult) : bool option =
        if r.Status = 0 then Some(r.Value <> 0uy)
        else None

    /// Check if a FloatResult succeeded and extract its value.
    let floatResultToOption (r: FloatResult) : float option =
        if r.Status = 0 then Some r.Value
        else None

    /// Extract a managed string from a StringResult, freeing the native memory.
    let stringResultToOption (r: StringResult) : string option =
        if r.Status = 0 && r.Value <> nativeint 0 then
            let s = Marshal.PtrToStringUTF8(r.Value, int r.Length)
            FFI.proven_free_string(r.Value)
            Some s
        else
            if r.Value <> nativeint 0 then
                FFI.proven_free_string(r.Value)
            None

// ============================================================================
// Safe Wrapper Modules
// ============================================================================

/// Lifecycle management for the Proven runtime.
module Runtime =
    /// Initialize the Proven runtime (must call before any other function).
    let init () : bool =
        FFI.proven_init() = 0

    /// Cleanup the Proven runtime.
    let deinit () : unit =
        FFI.proven_deinit()

    /// Check if the runtime is initialized.
    let isInitialized () : bool =
        FFI.proven_is_initialized()

    /// Get the FFI ABI version for compatibility checking.
    let abiVersion () : uint32 =
        FFI.proven_ffi_abi_version()

    /// Get the library version as a tuple (major, minor, patch).
    let version () : uint32 * uint32 * uint32 =
        (FFI.proven_version_major(), FFI.proven_version_minor(), FFI.proven_version_patch())

    /// Get the total number of modules in the library.
    let moduleCount () : uint32 =
        FFI.proven_module_count()

/// Overflow-checked arithmetic. All operations return Option - None on error.
module SafeMath =
    /// Safe division. Returns None on division by zero or INT64_MIN / -1.
    let div (a: int64) (b: int64) : int64 option =
        FFI.proven_math_div(a, b) |> intResultToOption

    /// Safe modulo. Returns None on division by zero.
    let mod' (a: int64) (b: int64) : int64 option =
        FFI.proven_math_mod(a, b) |> intResultToOption

    /// Checked addition. Returns None on overflow.
    let add (a: int64) (b: int64) : int64 option =
        FFI.proven_math_add_checked(a, b) |> intResultToOption

    /// Checked subtraction. Returns None on underflow.
    let sub (a: int64) (b: int64) : int64 option =
        FFI.proven_math_sub_checked(a, b) |> intResultToOption

    /// Checked multiplication. Returns None on overflow.
    let mul (a: int64) (b: int64) : int64 option =
        FFI.proven_math_mul_checked(a, b) |> intResultToOption

    /// Safe absolute value. Returns None for INT64_MIN.
    let abs (n: int64) : int64 option =
        FFI.proven_math_abs_safe(n) |> intResultToOption

    /// Clamp value to [lo, hi] range.
    let clamp (lo: int64) (hi: int64) (value: int64) : int64 =
        FFI.proven_math_clamp(lo, hi, value)

    /// Integer power with overflow checking. Returns None on overflow.
    let pow (base_: int64) (exp: uint32) : int64 option =
        FFI.proven_math_pow_checked(base_, exp) |> intResultToOption

    /// Safely sum a list. Returns None if any addition overflows.
    let safeSum (values: int64 list) : int64 option =
        values |> List.fold (fun acc x ->
            match acc with
            | None -> None
            | Some a -> add a x
        ) (Some 0L)

    /// Safely compute product of a list. Returns None if any multiply overflows.
    let safeProduct (values: int64 list) : int64 option =
        values |> List.fold (fun acc x ->
            match acc with
            | None -> None
            | Some a -> mul a x
        ) (Some 1L)

/// Text operations that handle encoding safely, including XSS prevention.
module SafeString =
    /// Check if a byte sequence is valid UTF-8.
    let isValidUtf8 (data: byte array) : bool option =
        FFI.proven_string_is_valid_utf8(data, unativeint data.Length) |> boolResultToOption

    /// Escape string for SQL (single quotes). Prefer parameterized queries.
    let escapeSql (s: string) : string option =
        let bytes = toUtf8 s
        FFI.proven_string_escape_sql(bytes, unativeint bytes.Length) |> stringResultToOption

    /// Escape string for HTML (prevents XSS).
    let escapeHtml (s: string) : string option =
        let bytes = toUtf8 s
        FFI.proven_string_escape_html(bytes, unativeint bytes.Length) |> stringResultToOption

    /// Escape string for JavaScript string literals.
    let escapeJs (s: string) : string option =
        let bytes = toUtf8 s
        FFI.proven_string_escape_js(bytes, unativeint bytes.Length) |> stringResultToOption

/// Filesystem operations that prevent directory traversal attacks.
module SafePath =
    /// Check if a path contains directory traversal sequences ("..").
    let hasTraversal (path: string) : bool option =
        let bytes = toUtf8 path
        FFI.proven_path_has_traversal(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Sanitize a filename by removing dangerous characters.
    let sanitizeFilename (name: string) : string option =
        let bytes = toUtf8 name
        FFI.proven_path_sanitize_filename(bytes, unativeint bytes.Length) |> stringResultToOption

/// Cryptographic primitives.
module SafeCrypto =
    /// Constant-time byte comparison (timing-attack safe).
    let constantTimeEquals (a: byte array) (b: byte array) : bool option =
        FFI.proven_crypto_constant_time_eq(a, unativeint a.Length, b, unativeint b.Length) |> boolResultToOption

    /// Fill buffer with cryptographically secure random bytes.
    let randomBytes (length: int) : byte array option =
        let buf = Array.zeroCreate<byte> length
        let status = FFI.proven_crypto_random_bytes(buf, unativeint length)
        if status = 0 then Some buf
        else None

/// Email validation (RFC 5321 simplified).
module SafeEmail =
    /// Validate email address.
    let isValid (email: string) : bool option =
        let bytes = toUtf8 email
        FFI.proven_email_is_valid(bytes, unativeint bytes.Length) |> boolResultToOption

/// IP address parsing and classification.
module SafeNetwork =
    /// Parse IPv4 address string (e.g., "192.168.1.1").
    let parseIPv4 (address: string) : IPv4Address option =
        let bytes = toUtf8 address
        let result = FFI.proven_network_parse_ipv4(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result.Address
        else None

    /// Check if IPv4 address is private (RFC 1918).
    let isPrivate (addr: IPv4Address) : bool =
        FFI.proven_network_ipv4_is_private(addr)

    /// Check if IPv4 address is loopback (127.0.0.0/8).
    let isLoopback (addr: IPv4Address) : bool =
        FFI.proven_network_ipv4_is_loopback(addr)

/// HTTP header safety operations (CRLF injection prevention).
module SafeHeader =
    /// Check for CRLF injection characters in header value.
    let hasCrlf (value: string) : bool option =
        let bytes = toUtf8 value
        FFI.proven_header_has_crlf(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Check if header name is a valid token per RFC 7230.
    let isValidName (name: string) : bool option =
        let bytes = toUtf8 name
        FFI.proven_header_is_valid_name(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Check if header name is in the dangerous headers list.
    let isDangerous (name: string) : bool option =
        let bytes = toUtf8 name
        FFI.proven_header_is_dangerous(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Create validated header string "Name: Value".
    let render (name: string) (value: string) : string option =
        let nameBytes = toUtf8 name
        let valueBytes = toUtf8 value
        FFI.proven_header_render(nameBytes, unativeint nameBytes.Length, valueBytes, unativeint valueBytes.Length)
        |> stringResultToOption

    /// Build Content-Security-Policy header value from JSON directives.
    let buildCsp (directivesJson: string) : string option =
        let bytes = toUtf8 directivesJson
        FFI.proven_header_build_csp(bytes, unativeint bytes.Length) |> stringResultToOption

    /// Build HSTS header value.
    let buildHsts (maxAge: int64) (includeSubdomains: bool) (preload: bool) : string option =
        FFI.proven_header_build_hsts(maxAge, includeSubdomains, preload) |> stringResultToOption

/// HTTP cookie safety (injection prevention).
module SafeCookie =
    /// Check for cookie injection characters (semicolon, CR, LF).
    let hasInjection (value: string) : bool option =
        let bytes = toUtf8 value
        FFI.proven_cookie_has_injection(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Validate cookie name.
    let validateName (name: string) : bool option =
        let bytes = toUtf8 name
        FFI.proven_cookie_validate_name(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Validate cookie value.
    let validateValue (value: string) : bool option =
        let bytes = toUtf8 value
        FFI.proven_cookie_validate_value(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Get cookie prefix type (0=none, 1=__Secure-, 2=__Host-).
    let getPrefix (name: string) : int64 option =
        let bytes = toUtf8 name
        FFI.proven_cookie_get_prefix(bytes, unativeint bytes.Length) |> intResultToOption

    /// Build delete cookie header value (sets expiry in the past).
    let buildDelete (name: string) : string option =
        let bytes = toUtf8 name
        FFI.proven_cookie_build_delete(bytes, unativeint bytes.Length) |> stringResultToOption

/// Content-Type / MIME handling (sniffing prevention).
module SafeContentType =
    /// Check if content type can be sniffed to something dangerous.
    let canSniffDangerous (contentType: string) : bool option =
        let bytes = toUtf8 contentType
        FFI.proven_content_type_can_sniff_dangerous(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Check if content type is JSON.
    let isJson (subtype: string) (suffix: string) : bool option =
        let subtypeBytes = toUtf8 subtype
        let suffixBytes = toUtf8 suffix
        FFI.proven_content_type_is_json(subtypeBytes, unativeint subtypeBytes.Length, suffixBytes, unativeint suffixBytes.Length)
        |> boolResultToOption

    /// Check if content type is XML.
    let isXml (subtype: string) (suffix: string) : bool option =
        let subtypeBytes = toUtf8 subtype
        let suffixBytes = toUtf8 suffix
        FFI.proven_content_type_is_xml(subtypeBytes, unativeint subtypeBytes.Length, suffixBytes, unativeint suffixBytes.Length)
        |> boolResultToOption

    /// Render content type to string.
    let render (mediaType: string) (subtype: string) (suffix: string) (charset: Charset) (hasCharset: bool) : string option =
        let typeBytes = toUtf8 mediaType
        let subtypeBytes = toUtf8 subtype
        let suffixBytes = toUtf8 suffix
        FFI.proven_content_type_render(
            typeBytes, unativeint typeBytes.Length,
            subtypeBytes, unativeint subtypeBytes.Length,
            suffixBytes, unativeint suffixBytes.Length,
            int charset, hasCharset)
        |> stringResultToOption

/// UUID generation and validation.
module SafeUUID =
    /// Generate UUID v4 (random).
    let v4 () : ProvenUUID option =
        let result = FFI.proven_uuid_v4()
        if result.Status = 0 then Some result.UUID
        else None

    /// Format UUID as canonical string.
    let toString (uuid: ProvenUUID) : string option =
        FFI.proven_uuid_to_string(uuid) |> stringResultToOption

    /// Parse UUID from string.
    let parse (s: string) : ProvenUUID option =
        let bytes = toUtf8 s
        let result = FFI.proven_uuid_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result.UUID
        else None

    /// Check if UUID is nil (all zeros).
    let isNil (uuid: ProvenUUID) : bool =
        FFI.proven_uuid_is_nil(uuid)

    /// Get UUID version.
    let version (uuid: ProvenUUID) : byte =
        FFI.proven_uuid_version(uuid)

/// JSON validation and type detection.
module SafeJson =
    /// Check if string is valid JSON.
    let isValid (json: string) : bool option =
        let bytes = toUtf8 json
        FFI.proven_json_is_valid(bytes, unativeint bytes.Length) |> boolResultToOption

    /// Get JSON value type at root level.
    let getType (json: string) : JsonType =
        let bytes = toUtf8 json
        enum<JsonType>(FFI.proven_json_get_type(bytes, unativeint bytes.Length))

/// ISO 8601 date/time handling.
module SafeDateTime =
    /// Parse ISO 8601 date string.
    let parse (s: string) : ProvenDateTime option =
        let bytes = toUtf8 s
        let result = FFI.proven_datetime_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result.DateTime
        else None

    /// Format DateTime as ISO 8601 string.
    let formatIso8601 (dt: ProvenDateTime) : string option =
        FFI.proven_datetime_format_iso8601(dt) |> stringResultToOption

    /// Check if year is a leap year.
    let isLeapYear (year: int32) : bool =
        FFI.proven_datetime_is_leap_year(year)

    /// Get days in month.
    let daysInMonth (year: int32) (month: byte) : byte =
        FFI.proven_datetime_days_in_month(year, month)

/// Safe floating-point operations.
module SafeFloat =
    /// Safe floating-point division. Returns None for division by zero or NaN.
    let div (a: float) (b: float) : float option =
        FFI.proven_float_div(a, b) |> floatResultToOption

    /// Check if float is finite (not NaN or Inf).
    let isFinite (x: float) : bool =
        FFI.proven_float_is_finite(x)

    /// Check if float is NaN.
    let isNaN (x: float) : bool =
        FFI.proven_float_is_nan(x)

    /// Safe square root. Returns None for negative input.
    let sqrt (x: float) : float option =
        FFI.proven_float_sqrt(x) |> floatResultToOption

    /// Safe natural logarithm. Returns None for non-positive input.
    let ln (x: float) : float option =
        FFI.proven_float_ln(x) |> floatResultToOption

/// Password validation.
module SafePassword =
    /// Validate password strength.
    let validate (password: string) : PasswordResult =
        let bytes = toUtf8 password
        FFI.proven_password_validate(bytes, unativeint bytes.Length)

    /// Check if password is in common passwords list.
    let isCommon (password: string) : bool =
        let bytes = toUtf8 password
        FFI.proven_password_is_common(bytes, unativeint bytes.Length)

/// Hexadecimal encoding/decoding.
module SafeHex =
    /// Encode bytes to hex string.
    let encode (data: byte array) (uppercase: bool) : string option =
        FFI.proven_hex_encode(data, unativeint data.Length, uppercase) |> stringResultToOption

    /// Decode hex string to bytes.
    let decode (hex: string) : byte array option =
        let bytes = toUtf8 hex
        let result = FFI.proven_hex_decode(bytes, unativeint bytes.Length)
        if result.Status = 0 && result.Data <> nativeint 0 then
            let output = Array.zeroCreate<byte>(int result.Length)
            Marshal.Copy(result.Data, output, 0, int result.Length)
            FFI.proven_hex_free(nativeint &&result)
            Some output
        else
            None

/// Monetary values with ISO 4217 codes.
module SafeCurrency =
    /// Parse currency amount (e.g., "USD 123.45").
    let parse (s: string) : CurrencyResult option =
        let bytes = toUtf8 s
        let result = FFI.proven_currency_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result
        else None

    /// Format currency amount.
    let format (amountMinor: int64) (code: byte array) (decimalPlaces: byte) : string option =
        FFI.proven_currency_format(amountMinor, code, decimalPlaces) |> stringResultToOption

/// E.164 phone number handling.
module SafePhone =
    /// Parse phone number.
    let parse (s: string) : PhoneResult option =
        let bytes = toUtf8 s
        let result = FFI.proven_phone_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result
        else None

    /// Format phone number as E.164.
    let formatE164 (countryCode: uint16) (nationalNumber: uint64) : string option =
        FFI.proven_phone_format_e164(countryCode, nationalNumber) |> stringResultToOption

/// Semantic versioning.
module SafeVersion =
    /// Parse semantic version string (e.g., "1.2.3-alpha").
    let parse (s: string) : VersionResult option =
        let bytes = toUtf8 s
        let result = FFI.proven_version_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result
        else None

    /// Compare two semantic versions. Returns negative, 0, or positive.
    let compare (a: SemanticVersion) (b: SemanticVersion) : int =
        FFI.proven_version_compare(a, b)

/// Geographic coordinate operations.
module SafeGeo =
    /// Validate and normalize geographic coordinate.
    let validate (lat: float) (lon: float) : GeoCoordinate option =
        let result = FFI.proven_geo_validate(lat, lon)
        if result.Status = 0 then Some result.Coordinate
        else None

    /// Calculate distance between two points (Haversine, in meters).
    let distance (a: GeoCoordinate) (b: GeoCoordinate) : float option =
        FFI.proven_geo_distance(a, b) |> floatResultToOption

    /// Check if coordinate is inside a bounding box.
    let inBounds (coord: GeoCoordinate) (minLat: float) (maxLat: float) (minLon: float) (maxLon: float) : bool =
        FFI.proven_geo_in_bounds(coord, minLat, maxLat, minLon, maxLon)

/// CRC and hash verification.
module SafeChecksum =
    /// Calculate CRC32 checksum.
    let crc32 (data: byte array) : int64 option =
        FFI.proven_checksum_crc32(data, unativeint data.Length) |> intResultToOption

    /// Verify CRC32 matches expected value.
    let verifyCrc32 (data: byte array) (expected: uint32) : bool option =
        FFI.proven_checksum_verify_crc32(data, unativeint data.Length, expected) |> boolResultToOption

/// Probability values clamped to [0, 1].
module SafeProbability =
    /// Create probability (clamped to [0, 1]).
    let create (value: float) : float =
        FFI.proven_probability_create(value)

    /// Multiply probabilities (independent events).
    let andP (a: float) (b: float) : float =
        FFI.proven_probability_and(a, b)

    /// Add probabilities (mutually exclusive events).
    let orExclusive (a: float) (b: float) : float =
        FFI.proven_probability_or_exclusive(a, b)

    /// Complement probability.
    let notP (p: float) : float =
        FFI.proven_probability_not(p)

/// Safe expression evaluation.
module SafeCalculator =
    /// Evaluate an arithmetic expression safely.
    let eval (expr: string) : float option =
        let bytes = toUtf8 expr
        FFI.proven_calculator_eval(bytes, unativeint bytes.Length) |> floatResultToOption

/// Color space conversions.
module SafeColor =
    /// Parse hex color string (#RRGGBB or #RGB).
    let parseHex (hex: string) : RGBColor option =
        let bytes = toUtf8 hex
        let result = FFI.proven_color_parse_hex(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result.Color
        else None

    /// Convert RGB to HSL.
    let rgbToHsl (rgb: RGBColor) : HSLColor =
        FFI.proven_color_rgb_to_hsl(rgb)

    /// Format RGB as hex string.
    let toHex (rgb: RGBColor) : string option =
        FFI.proven_color_to_hex(rgb) |> stringResultToOption

/// Angle conversions and normalization.
module SafeAngle =
    /// Convert degrees to radians.
    let degToRad (degrees: float) : float =
        FFI.proven_angle_deg_to_rad(degrees)

    /// Convert radians to degrees.
    let radToDeg (radians: float) : float =
        FFI.proven_angle_rad_to_deg(radians)

    /// Normalize angle to [0, 360) degrees.
    let normalizeDegrees (degrees: float) : float =
        FFI.proven_angle_normalize_degrees(degrees)

    /// Normalize angle to [0, 2*pi) radians.
    let normalizeRadians (radians: float) : float =
        FFI.proven_angle_normalize_radians(radians)

/// Physical unit conversions.
module SafeUnit =
    /// Convert length between units.
    let convertLength (value: float) (from: LengthUnit) (to': LengthUnit) : float option =
        FFI.proven_unit_convert_length(value, int from, int to') |> floatResultToOption

    /// Convert temperature between units.
    let convertTemp (value: float) (from: TempUnit) (to': TempUnit) : float option =
        FFI.proven_unit_convert_temp(value, int from, int to') |> floatResultToOption

/// Exponential backoff retry configuration.
module SafeRetry =
    /// Calculate delay for a given attempt (with jitter).
    let delay (config: RetryConfig) (attempt: uint32) : uint64 =
        FFI.proven_retry_delay(config, attempt)

    /// Check if retry should be attempted.
    let shouldRetry (config: RetryConfig) (attempt: uint32) : bool =
        FFI.proven_retry_should_retry(config, attempt)

/// Machine learning activation functions.
module SafeML =
    /// Sigmoid function: 1 / (1 + exp(-x)).
    let sigmoid (x: float) : float =
        FFI.proven_ml_sigmoid(x)

    /// ReLU function: max(0, x).
    let relu (x: float) : float =
        FFI.proven_ml_relu(x)

    /// Leaky ReLU: x >= 0 ? x : alpha * x.
    let leakyRelu (x: float) (alpha: float) : float =
        FFI.proven_ml_leaky_relu(x, alpha)

    /// Clamp value to [min, max].
    let clamp (x: float) (minVal: float) (maxVal: float) : float =
        FFI.proven_ml_clamp(x, minVal, maxVal)

    /// Softmax normalization over an array.
    let softmax (input: float array) : float array option =
        let output = Array.zeroCreate<float> input.Length
        let status = FFI.proven_ml_softmax(input, output, unativeint input.Length)
        if status = 0 then Some output
        else None
