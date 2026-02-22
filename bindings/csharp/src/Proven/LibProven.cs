// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// LibProven.cs - P/Invoke declarations for all FFI functions exported by libproven.
//
// ALL computation is performed in verified Idris 2 code via the Zig FFI bridge.
// This file contains ONLY interop declarations. Do NOT reimplement any algorithms.
//
// Memory management rules:
//   - ProvenStringResult.Ptr must be freed with proven_free_string()
//   - Integer, boolean, and float results do not require freeing

using System;
using System.Runtime.InteropServices;

namespace Proven
{
    // ========================================================================
    // Status Codes
    // ========================================================================

    /// <summary>
    /// Status codes returned by Proven FFI operations.
    /// Zero indicates success; negative values indicate specific error conditions.
    /// </summary>
    public enum ProvenStatus : int
    {
        /// <summary>Operation completed successfully.</summary>
        Ok = 0,
        /// <summary>Null pointer passed to function.</summary>
        ErrNullPointer = -1,
        /// <summary>Invalid argument provided.</summary>
        ErrInvalidArgument = -2,
        /// <summary>Integer overflow detected.</summary>
        ErrOverflow = -3,
        /// <summary>Integer underflow detected.</summary>
        ErrUnderflow = -4,
        /// <summary>Division by zero attempted.</summary>
        ErrDivisionByZero = -5,
        /// <summary>Input could not be parsed.</summary>
        ErrParseFailure = -6,
        /// <summary>Validation check failed.</summary>
        ErrValidationFailed = -7,
        /// <summary>Index out of bounds.</summary>
        ErrOutOfBounds = -8,
        /// <summary>Encoding or decoding error.</summary>
        ErrEncodingError = -9,
        /// <summary>Memory allocation failed.</summary>
        ErrAllocationFailed = -10,
        /// <summary>Feature not implemented.</summary>
        ErrNotImplemented = -99
    }

    // ========================================================================
    // Core Result Structs (match C ABI layout)
    // ========================================================================

    /// <summary>
    /// Result for integer operations. Matches C ABI: { int32_t status; int64_t value; }.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct IntResult
    {
        /// <summary>Status code (0 = success).</summary>
        public int Status;
        /// <summary>Result value (valid only when Status == 0).</summary>
        public long Value;
    }

    /// <summary>
    /// Result for boolean operations. Matches C ABI: { int32_t status; bool value; }.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct BoolResult
    {
        /// <summary>Status code (0 = success).</summary>
        public int Status;
        /// <summary>Result value as byte (0 = false, non-zero = true). Valid only when Status == 0.</summary>
        [MarshalAs(UnmanagedType.U1)]
        public bool Value;
    }

    /// <summary>
    /// Result for string operations. Matches C ABI: { int32_t status; char* value; size_t length; }.
    /// Caller must free Value using proven_free_string() after reading.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct StringResult
    {
        /// <summary>Status code (0 = success).</summary>
        public int Status;
        /// <summary>Pointer to UTF-8 string data (must be freed with proven_free_string).</summary>
        public IntPtr Ptr;
        /// <summary>Length of string data in bytes.</summary>
        public nuint Length;
    }

    /// <summary>
    /// Result for floating-point operations. Matches C ABI: { int32_t status; double value; }.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct FloatResult
    {
        /// <summary>Status code (0 = success).</summary>
        public int Status;
        /// <summary>Result value (valid only when Status == 0).</summary>
        public double Value;
    }

    // ========================================================================
    // Domain-specific Structs
    // ========================================================================

    /// <summary>IPv4 address (4 bytes).</summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct IPv4Address
    {
        /// <summary>Four octets of the IPv4 address.</summary>
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
        public byte[] Octets;
    }

    /// <summary>Result for IPv4 parsing.</summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct IPv4Result
    {
        /// <summary>Status code (0 = success).</summary>
        public int Status;
        /// <summary>Parsed IPv4 address.</summary>
        public IPv4Address Address;
    }

    /// <summary>DateTime components matching the C ABI ProvenDateTime struct.</summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct ProvenDateTime
    {
        /// <summary>Year.</summary>
        public int Year;
        /// <summary>Month (1-12).</summary>
        public byte Month;
        /// <summary>Day (1-31).</summary>
        public byte Day;
        /// <summary>Hour (0-23).</summary>
        public byte Hour;
        /// <summary>Minute (0-59).</summary>
        public byte Minute;
        /// <summary>Second (0-59).</summary>
        public byte Second;
        /// <summary>Nanosecond component.</summary>
        public uint Nanosecond;
        /// <summary>Timezone offset in minutes (0 for UTC, negative for west).</summary>
        public short TzOffsetMinutes;
    }

    /// <summary>Result for DateTime parsing.</summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct DateTimeResult
    {
        /// <summary>Status code (0 = success).</summary>
        public int Status;
        /// <summary>Parsed DateTime components.</summary>
        public ProvenDateTime DateTime;
    }

    /// <summary>JSON value type enumeration.</summary>
    public enum JsonType : int
    {
        /// <summary>JSON null value.</summary>
        Null = 0,
        /// <summary>JSON boolean value.</summary>
        Bool = 1,
        /// <summary>JSON number value.</summary>
        Number = 2,
        /// <summary>JSON string value.</summary>
        String = 3,
        /// <summary>JSON array value.</summary>
        Array = 4,
        /// <summary>JSON object value.</summary>
        Object = 5,
        /// <summary>Invalid or unparseable JSON.</summary>
        Invalid = -1
    }

    // ========================================================================
    // P/Invoke Declarations
    // ========================================================================

    /// <summary>
    /// Low-level P/Invoke bindings to libproven. These should not be called
    /// directly in most cases; use the safe wrapper classes (SafeMath, SafeString,
    /// etc.) instead, which handle marshaling and memory management.
    /// </summary>
    public static class LibProven
    {
        /// <summary>Native library name for P/Invoke resolution.</summary>
        private const string LibName = "proven";

        // --------------------------------------------------------------------
        // Memory Management (1 function)
        // --------------------------------------------------------------------

        /// <summary>
        /// Free a string allocated by Proven functions.
        /// </summary>
        /// <param name="ptr">Pointer to the string to free (may be IntPtr.Zero).</param>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void proven_free_string(IntPtr ptr);

        // --------------------------------------------------------------------
        // Lifecycle (3 functions)
        // --------------------------------------------------------------------

        /// <summary>
        /// Initialize the Proven runtime (includes Idris 2 runtime).
        /// Must be called before any other Proven function.
        /// </summary>
        /// <returns>PROVEN_OK (0) on success, error code otherwise.</returns>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern int proven_init();

        /// <summary>
        /// Cleanup the Proven runtime.
        /// </summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void proven_deinit();

        /// <summary>
        /// Check if the runtime is initialized.
        /// </summary>
        /// <returns>true if initialized.</returns>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.U1)]
        public static extern bool proven_is_initialized();

        // --------------------------------------------------------------------
        // Version Information (5 functions)
        // --------------------------------------------------------------------

        /// <summary>Get FFI ABI version for compatibility checking.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint proven_ffi_abi_version();

        /// <summary>Get major version number.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint proven_version_major();

        /// <summary>Get minor version number.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint proven_version_minor();

        /// <summary>Get patch version number.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint proven_version_patch();

        /// <summary>Get total module count.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint proven_module_count();

        // --------------------------------------------------------------------
        // SafeMath - Arithmetic that cannot crash (8 functions)
        // --------------------------------------------------------------------

        /// <summary>Checked addition with overflow detection.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_add_checked(long a, long b);

        /// <summary>Checked subtraction with underflow detection.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_sub_checked(long a, long b);

        /// <summary>Checked multiplication with overflow detection.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_mul_checked(long a, long b);

        /// <summary>Safe integer division. Returns error on division by zero or INT64_MIN / -1.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_div(long numerator, long denominator);

        /// <summary>Safe modulo operation. Returns error on division by zero.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_mod(long numerator, long denominator);

        /// <summary>Safe absolute value. Returns error for INT64_MIN.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_abs_safe(long n);

        /// <summary>Clamp value to [lo, hi] range.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern long proven_math_clamp(long lo, long hi, long value);

        /// <summary>Integer exponentiation with overflow checking.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_pow_checked(long baseValue, uint exp);

        // --------------------------------------------------------------------
        // SafeString - Text operations (4 functions)
        // --------------------------------------------------------------------

        /// <summary>Check if bytes are valid UTF-8.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_string_is_valid_utf8(byte[] ptr, nuint len);

        /// <summary>Escape string for SQL (single quotes). Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_string_escape_sql(byte[] ptr, nuint len);

        /// <summary>Escape string for HTML (prevents XSS). Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_string_escape_html(byte[] ptr, nuint len);

        /// <summary>Escape string for JavaScript string literals. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_string_escape_js(byte[] ptr, nuint len);

        // --------------------------------------------------------------------
        // SafePath - Filesystem traversal prevention (2 functions)
        // --------------------------------------------------------------------

        /// <summary>Check if path contains directory traversal sequences.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_path_has_traversal(byte[] ptr, nuint len);

        /// <summary>Sanitize filename by removing dangerous characters. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_path_sanitize_filename(byte[] ptr, nuint len);

        // --------------------------------------------------------------------
        // SafeEmail - Email validation (1 function)
        // --------------------------------------------------------------------

        /// <summary>Validate email address (RFC 5321 simplified).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_email_is_valid(byte[] ptr, nuint len);

        // --------------------------------------------------------------------
        // SafeUrl - URL parsing (2 functions)
        // --------------------------------------------------------------------

        /// <summary>Parse a URL into components. Caller must free with proven_url_free.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_url_parse(byte[] ptr, nuint len);

        /// <summary>Free URL components allocated by proven_url_parse.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void proven_url_free(IntPtr components);

        // --------------------------------------------------------------------
        // SafeNetwork - IP address parsing (3 functions)
        // --------------------------------------------------------------------

        /// <summary>Parse IPv4 address string.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IPv4Result proven_network_parse_ipv4(byte[] ptr, nuint len);

        /// <summary>Check if IPv4 address is private (RFC 1918).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.U1)]
        public static extern bool proven_network_ipv4_is_private(IPv4Address addr);

        /// <summary>Check if IPv4 address is loopback (127.0.0.0/8).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.U1)]
        public static extern bool proven_network_ipv4_is_loopback(IPv4Address addr);

        // --------------------------------------------------------------------
        // SafeCrypto - Cryptographic primitives (2 functions)
        // --------------------------------------------------------------------

        /// <summary>Constant-time byte comparison (timing-attack safe).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_crypto_constant_time_eq(
            byte[] ptr1, nuint len1,
            byte[] ptr2, nuint len2);

        /// <summary>Fill buffer with cryptographically secure random bytes.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern int proven_crypto_random_bytes(byte[] ptr, nuint len);

        // --------------------------------------------------------------------
        // SafeJson - JSON validation (2 functions)
        // --------------------------------------------------------------------

        /// <summary>Check if string is valid JSON.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_json_is_valid(byte[] ptr, nuint len);

        /// <summary>Get JSON value type at root level.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern int proven_json_get_type(byte[] ptr, nuint len);

        // --------------------------------------------------------------------
        // SafeDateTime - ISO 8601 date/time handling (4 functions)
        // --------------------------------------------------------------------

        /// <summary>Parse ISO 8601 date string.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern DateTimeResult proven_datetime_parse(byte[] ptr, nuint len);

        /// <summary>Format DateTime as ISO 8601 string. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_datetime_format_iso8601(ProvenDateTime dt);

        /// <summary>Check if year is a leap year.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.U1)]
        public static extern bool proven_datetime_is_leap_year(int year);

        /// <summary>Get number of days in a month. Returns 0 for invalid month.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern byte proven_datetime_days_in_month(int year, byte month);

        // --------------------------------------------------------------------
        // SafeFloat - Safe floating-point operations (5 functions)
        // --------------------------------------------------------------------

        /// <summary>Safe floating-point division.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern FloatResult proven_float_div(double a, double b);

        /// <summary>Check if float is finite (not NaN or Inf).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.U1)]
        public static extern bool proven_float_is_finite(double x);

        /// <summary>Check if float is NaN.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.U1)]
        public static extern bool proven_float_is_nan(double x);

        /// <summary>Safe square root. Returns error for negative or NaN input.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern FloatResult proven_float_sqrt(double x);

        /// <summary>Safe natural logarithm. Returns error for non-positive or NaN input.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern FloatResult proven_float_ln(double x);

        // --------------------------------------------------------------------
        // SafeHex - Hexadecimal encoding/decoding (3 functions)
        // --------------------------------------------------------------------

        /// <summary>Encode bytes to hex string. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_hex_encode(
            byte[] ptr, nuint len,
            [MarshalAs(UnmanagedType.U1)] bool uppercase);

        /// <summary>Decode hex string to bytes. Caller must free with proven_hex_free.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_hex_decode(byte[] ptr, nuint len);

        /// <summary>Free hex decode result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void proven_hex_free(IntPtr result);

        // --------------------------------------------------------------------
        // SafeChecksum (2 functions)
        // --------------------------------------------------------------------

        /// <summary>Calculate CRC32 checksum.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_checksum_crc32(byte[] ptr, nuint len);

        /// <summary>Verify CRC32 matches expected value.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_checksum_verify_crc32(byte[] ptr, nuint len, uint expected);

        // --------------------------------------------------------------------
        // SafePassword (2 functions)
        // --------------------------------------------------------------------

        /// <summary>Validate password strength.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_password_validate(byte[] ptr, nuint len);

        /// <summary>Check if password is in common passwords list.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.U1)]
        public static extern bool proven_password_is_common(byte[] ptr, nuint len);

        // --------------------------------------------------------------------
        // SafeVersion (3 functions)
        // --------------------------------------------------------------------

        /// <summary>Parse semantic version string.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_version_parse(byte[] ptr, nuint len);

        /// <summary>Free version result resources.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void proven_version_free(IntPtr version);

        // --------------------------------------------------------------------
        // SafeColor (3 functions)
        // --------------------------------------------------------------------

        /// <summary>Parse hex color string (#RRGGBB or #RGB).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_color_parse_hex(byte[] ptr, nuint len);

        /// <summary>Format RGB as hex string. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_color_to_hex(byte r, byte g, byte b);

        // --------------------------------------------------------------------
        // SafeCalculator (1 function)
        // --------------------------------------------------------------------

        /// <summary>Evaluate an arithmetic expression safely.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern FloatResult proven_calculator_eval(byte[] ptr, nuint len);

        // --------------------------------------------------------------------
        // SafeHttp (2 functions)
        // --------------------------------------------------------------------

        /// <summary>URL-encode a string (RFC 3986 percent encoding). Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_http_url_encode(byte[] ptr, nuint len);

        /// <summary>URL-decode a percent-encoded string. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_http_url_decode(byte[] ptr, nuint len);

        // --------------------------------------------------------------------
        // SafePhone (2 functions)
        // --------------------------------------------------------------------

        /// <summary>Parse phone number to E.164 format.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_phone_parse(byte[] ptr, nuint len);

        /// <summary>Format phone number as E.164 string. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_phone_format_e164(ushort countryCode, ulong nationalNumber);

        // --------------------------------------------------------------------
        // SafeCurrency (2 functions)
        // --------------------------------------------------------------------

        /// <summary>Parse currency amount.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_currency_parse(byte[] ptr, nuint len);

        /// <summary>Format currency amount. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_currency_format(long amountMinor, byte[] code, byte decimalPlaces);

        // --------------------------------------------------------------------
        // SafeAngle (4 functions)
        // --------------------------------------------------------------------

        /// <summary>Convert degrees to radians.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_angle_deg_to_rad(double degrees);

        /// <summary>Convert radians to degrees.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_angle_rad_to_deg(double radians);

        /// <summary>Normalize angle to [0, 360) degrees.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_angle_normalize_degrees(double degrees);

        /// <summary>Normalize angle to [0, 2*pi) radians.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_angle_normalize_radians(double radians);

        // --------------------------------------------------------------------
        // SafeProbability (4 functions)
        // --------------------------------------------------------------------

        /// <summary>Create probability value (clamped to [0, 1]).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_probability_create(double value);

        /// <summary>Multiply probabilities (independent events).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_probability_and(double a, double b);

        /// <summary>Add probabilities (mutually exclusive events).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_probability_or_exclusive(double a, double b);

        /// <summary>Complement probability.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_probability_not(double p);

        // --------------------------------------------------------------------
        // SafeGeo (3 functions)
        // --------------------------------------------------------------------

        /// <summary>Validate and normalize geographic coordinate.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_geo_validate(double lat, double lon);

        /// <summary>Calculate distance between two points (Haversine, in metres).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern FloatResult proven_geo_distance(
            double lat1, double lon1,
            double lat2, double lon2);

        // --------------------------------------------------------------------
        // SafeHeader (6 functions)
        // --------------------------------------------------------------------

        /// <summary>Check for CRLF injection characters in header value.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_header_has_crlf(byte[] ptr, nuint len);

        /// <summary>Check if header name is a valid token per RFC 7230.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_header_is_valid_name(byte[] ptr, nuint len);

        /// <summary>Check if header name is dangerous.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_header_is_dangerous(byte[] ptr, nuint len);

        /// <summary>Create validated header string "Name: Value". Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_header_render(
            byte[] namePtr, nuint nameLen,
            byte[] valuePtr, nuint valueLen);

        /// <summary>Build CSP header value from JSON directives. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_header_build_csp(byte[] directivesJson, nuint jsonLen);

        /// <summary>Build HSTS header value. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_header_build_hsts(
            long maxAge,
            [MarshalAs(UnmanagedType.U1)] bool includeSubdomains,
            [MarshalAs(UnmanagedType.U1)] bool preload);

        // --------------------------------------------------------------------
        // SafeCookie (6 functions)
        // --------------------------------------------------------------------

        /// <summary>Check for cookie injection characters.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_cookie_has_injection(byte[] ptr, nuint len);

        /// <summary>Validate cookie name.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_cookie_validate_name(byte[] ptr, nuint len);

        /// <summary>Validate cookie value.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_cookie_validate_value(byte[] ptr, nuint len);

        /// <summary>Get cookie prefix type.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_cookie_get_prefix(byte[] ptr, nuint len);

        /// <summary>Build Set-Cookie header value. Caller must free result.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_cookie_build_delete(byte[] namePtr, nuint nameLen);

        // --------------------------------------------------------------------
        // SafeML (5 functions)
        // --------------------------------------------------------------------

        /// <summary>Sigmoid function: 1 / (1 + exp(-x)).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_ml_sigmoid(double x);

        /// <summary>ReLU function: max(0, x).</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_ml_relu(double x);

        /// <summary>Leaky ReLU: x >= 0 ? x : alpha * x.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_ml_leaky_relu(double x, double alpha);

        /// <summary>Clamp value to [minVal, maxVal].</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern double proven_ml_clamp(double x, double minVal, double maxVal);

        /// <summary>Softmax normalization over an array.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern int proven_ml_softmax(double[] input, double[] output, nuint len);

        // --------------------------------------------------------------------
        // SafeUnit (2 functions)
        // --------------------------------------------------------------------

        /// <summary>Convert length between units.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern FloatResult proven_unit_convert_length(double value, int from, int to);

        /// <summary>Convert temperature between units.</summary>
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern FloatResult proven_unit_convert_temp(double value, int from, int to);
    }

    // ========================================================================
    // Internal Marshaling Helpers
    // ========================================================================

    /// <summary>
    /// Internal helper methods for marshaling between .NET managed types and
    /// the C ABI used by libproven. Not intended for direct external use.
    /// </summary>
    internal static class MarshalHelpers
    {
        /// <summary>
        /// Convert a .NET string to a UTF-8 byte array for passing to FFI.
        /// </summary>
        /// <param name="s">The string to encode.</param>
        /// <returns>UTF-8 encoded byte array.</returns>
        public static byte[] ToUtf8(string s)
        {
            return System.Text.Encoding.UTF8.GetBytes(s);
        }

        /// <summary>
        /// Extract the integer value from an IntResult if it succeeded.
        /// </summary>
        /// <param name="result">The FFI result.</param>
        /// <returns>The value if successful, null otherwise.</returns>
        public static long? IntResultToNullable(IntResult result)
        {
            return result.Status == 0 ? result.Value : null;
        }

        /// <summary>
        /// Extract the boolean value from a BoolResult if it succeeded.
        /// </summary>
        /// <param name="result">The FFI result.</param>
        /// <returns>The value if successful, null otherwise.</returns>
        public static bool? BoolResultToNullable(BoolResult result)
        {
            return result.Status == 0 ? result.Value : null;
        }

        /// <summary>
        /// Extract the double value from a FloatResult if it succeeded.
        /// </summary>
        /// <param name="result">The FFI result.</param>
        /// <returns>The value if successful, null otherwise.</returns>
        public static double? FloatResultToNullable(FloatResult result)
        {
            return result.Status == 0 ? result.Value : null;
        }

        /// <summary>
        /// Extract a managed string from a StringResult, freeing the native memory.
        /// Always frees the native pointer, even on error.
        /// </summary>
        /// <param name="result">The FFI result containing a native string pointer.</param>
        /// <returns>The managed string if successful, null otherwise.</returns>
        public static string? StringResultToManaged(StringResult result)
        {
            if (result.Status == 0 && result.Ptr != IntPtr.Zero)
            {
                try
                {
                    string managed = Marshal.PtrToStringUTF8(result.Ptr, (int)result.Length)
                        ?? string.Empty;
                    return managed;
                }
                finally
                {
                    LibProven.proven_free_string(result.Ptr);
                }
            }

            // Free even on error, if a pointer was allocated
            if (result.Ptr != IntPtr.Zero)
            {
                LibProven.proven_free_string(result.Ptr);
            }

            return null;
        }

        /// <summary>
        /// Get the ProvenStatus enum from a raw integer status code.
        /// </summary>
        /// <param name="code">The raw integer status code.</param>
        /// <returns>The corresponding ProvenStatus value.</returns>
        public static ProvenStatus StatusFromCode(int code)
        {
            return (ProvenStatus)code;
        }
    }
}
