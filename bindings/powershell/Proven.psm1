# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

<#
.SYNOPSIS
    Proven Safety Library for PowerShell - FFI bindings to libproven.
.DESCRIPTION
    Thin P/Invoke wrappers over the formally verified Proven library (libproven).
    ALL computation is performed in verified Idris 2 code via the Zig FFI bridge.
    This module contains ONLY marshaling logic. No algorithms are reimplemented.
.NOTES
    Version: 0.9.0
    Requires: libproven.so (Linux) or proven.dll (Windows) on the library path.
#>

# ============================================================================
# FFI TYPE DEFINITIONS
# ============================================================================

$ProvenFFI = @"
using System;
using System.Runtime.InteropServices;
using System.Text;

// --- Status codes ---
public enum ProvenStatus : int {
    Ok = 0,
    ErrNullPointer = -1,
    ErrInvalidArgument = -2,
    ErrOverflow = -3,
    ErrUnderflow = -4,
    ErrDivisionByZero = -5,
    ErrParseFailure = -6,
    ErrValidationFailed = -7,
    ErrOutOfBounds = -8,
    ErrEncodingError = -9,
    ErrAllocationFailed = -10,
    ErrNotImplemented = -99
}

// --- Core result structs (match C ABI layout) ---

[StructLayout(LayoutKind.Sequential)]
public struct ProvenIntResult {
    public int Status;
    public long Value;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenBoolResult {
    public int Status;
    [MarshalAs(UnmanagedType.U1)]
    public bool Value;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenStringResult {
    public int Status;
    public IntPtr Value;
    public UIntPtr Length;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenFloatResult {
    public int Status;
    public double Value;
}

// --- Domain-specific structs ---

[StructLayout(LayoutKind.Sequential)]
public struct ProvenIPv4Address {
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public byte[] Octets;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenIPv4Result {
    public int Status;
    public ProvenIPv4Address Address;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenUUID {
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 16)]
    public byte[] Bytes;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenUUIDResult {
    public int Status;
    public ProvenUUID UUID;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenCurrencyResult {
    public int Status;
    public long AmountMinor;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)]
    public byte[] CurrencyCode;
    public byte DecimalPlaces;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenPhoneResult {
    public int Status;
    public ushort CountryCode;
    public ulong NationalNumber;
    [MarshalAs(UnmanagedType.U1)]
    public bool IsValid;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenDateTime {
    public int Year;
    public byte Month;
    public byte Day;
    public byte Hour;
    public byte Minute;
    public byte Second;
    public uint Nanosecond;
    public short TzOffsetMinutes;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenDateTimeResult {
    public int Status;
    public ProvenDateTime DateTime;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenGeoCoordinate {
    public double Latitude;
    public double Longitude;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenGeoResult {
    public int Status;
    public ProvenGeoCoordinate Coordinate;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenPasswordResult {
    public int Strength;
    [MarshalAs(UnmanagedType.U1)]
    public bool HasLowercase;
    [MarshalAs(UnmanagedType.U1)]
    public bool HasUppercase;
    [MarshalAs(UnmanagedType.U1)]
    public bool HasDigit;
    [MarshalAs(UnmanagedType.U1)]
    public bool HasSpecial;
    public UIntPtr Length;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenHexDecodeResult {
    public int Status;
    public IntPtr Data;
    public UIntPtr Length;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenRGBColor {
    public byte R;
    public byte G;
    public byte B;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenHSLColor {
    public double H;
    public double S;
    public double L;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenColorResult {
    public int Status;
    public ProvenRGBColor Color;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenRetryConfig {
    public uint MaxAttempts;
    public ulong BaseDelayMs;
    public ulong MaxDelayMs;
    public double Multiplier;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenSemanticVersion {
    public uint Major;
    public uint Minor;
    public uint Patch;
    public UIntPtr PrereleaseLen;
    public IntPtr Prerelease;
}

[StructLayout(LayoutKind.Sequential)]
public struct ProvenVersionResult {
    public int Status;
    public ProvenSemanticVersion Version;
}

// --- P/Invoke declarations: ALL 103 exported functions from libproven ---

public static class ProvenNative {

    // --- Memory Management (1) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_free_string(IntPtr ptr);

    // --- Lifecycle (3) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_init();

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_deinit();

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_is_initialized();

    // --- Version Information (5) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern uint proven_ffi_abi_version();

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern uint proven_version_major();

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern uint proven_version_minor();

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern uint proven_version_patch();

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern uint proven_module_count();

    // --- SafeMath (8) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_math_div(long numerator, long denominator);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_math_mod(long numerator, long denominator);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_math_add_checked(long a, long b);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_math_sub_checked(long a, long b);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_math_mul_checked(long a, long b);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_math_abs_safe(long n);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern long proven_math_clamp(long lo, long hi, long value);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_math_pow_checked(long baseVal, uint exp);

    // --- SafeString (4) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_string_is_valid_utf8(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_string_escape_sql(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_string_escape_html(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_string_escape_js(byte[] ptr, UIntPtr len);

    // --- SafePath (2) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_path_has_traversal(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_path_sanitize_filename(byte[] ptr, UIntPtr len);

    // --- SafeCrypto (2) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_crypto_constant_time_eq(
        byte[] ptr1, UIntPtr len1, byte[] ptr2, UIntPtr len2);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_crypto_random_bytes(byte[] ptr, UIntPtr len);

    // --- SafeEmail (1) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_email_is_valid(byte[] ptr, UIntPtr len);

    // --- SafeNetwork (3) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIPv4Result proven_network_parse_ipv4(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_network_ipv4_is_private(ProvenIPv4Address addr);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

    // --- SafeHeader (6) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_header_has_crlf(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_header_is_valid_name(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_header_is_dangerous(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_header_render(
        byte[] namePtr, UIntPtr nameLen, byte[] valuePtr, UIntPtr valueLen);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_header_build_csp(byte[] directivesJson, UIntPtr jsonLen);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_header_build_hsts(
        long maxAge, [MarshalAs(UnmanagedType.U1)] bool includeSubdomains,
        [MarshalAs(UnmanagedType.U1)] bool preload);

    // --- SafeCookie (6) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_cookie_has_injection(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_cookie_validate_name(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_cookie_validate_value(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_cookie_get_prefix(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_cookie_build_delete(byte[] namePtr, UIntPtr nameLen);

    // --- SafeContentType (4) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_content_type_can_sniff_dangerous(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_content_type_is_json(
        byte[] subtypePtr, UIntPtr subtypeLen, byte[] suffixPtr, UIntPtr suffixLen);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_content_type_is_xml(
        byte[] subtypePtr, UIntPtr subtypeLen, byte[] suffixPtr, UIntPtr suffixLen);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_content_type_render(
        byte[] typePtr, UIntPtr typeLen, byte[] subtypePtr, UIntPtr subtypeLen,
        byte[] suffixPtr, UIntPtr suffixLen, int charset, [MarshalAs(UnmanagedType.U1)] bool hasCharset);

    // --- SafeUUID (5) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenUUIDResult proven_uuid_v4();

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_uuid_to_string(ProvenUUID uuid);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenUUIDResult proven_uuid_parse(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_uuid_is_nil(ProvenUUID uuid);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern byte proven_uuid_version(ProvenUUID uuid);

    // --- SafeJson (2) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_json_is_valid(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_json_get_type(byte[] ptr, UIntPtr len);

    // --- SafeDateTime (4) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenDateTimeResult proven_datetime_parse(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_datetime_format_iso8601(ProvenDateTime dt);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_datetime_is_leap_year(int year);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern byte proven_datetime_days_in_month(int year, byte month);

    // --- SafeFloat (5) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenFloatResult proven_float_div(double a, double b);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_float_is_finite(double x);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_float_is_nan(double x);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenFloatResult proven_float_sqrt(double x);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenFloatResult proven_float_ln(double x);

    // --- SafePassword (2) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenPasswordResult proven_password_validate(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_password_is_common(byte[] ptr, UIntPtr len);

    // --- SafeHex (3) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_hex_encode(
        byte[] ptr, UIntPtr len, [MarshalAs(UnmanagedType.U1)] bool uppercase);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenHexDecodeResult proven_hex_decode(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_hex_free(IntPtr result);

    // --- SafeCurrency (2) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenCurrencyResult proven_currency_parse(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_currency_format(long amountMinor, byte[] code, byte decimalPlaces);

    // --- SafePhone (2) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenPhoneResult proven_phone_parse(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_phone_format_e164(ushort countryCode, ulong nationalNumber);

    // --- SafeVersion (3) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenVersionResult proven_version_parse(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_version_compare(ProvenSemanticVersion a, ProvenSemanticVersion b);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_version_free(IntPtr version);

    // --- SafeGeo (3) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenGeoResult proven_geo_validate(double lat, double lon);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenFloatResult proven_geo_distance(ProvenGeoCoordinate a, ProvenGeoCoordinate b);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_geo_in_bounds(
        ProvenGeoCoordinate coord, double minLat, double maxLat, double minLon, double maxLon);

    // --- SafeChecksum (2) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_checksum_crc32(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenBoolResult proven_checksum_verify_crc32(byte[] ptr, UIntPtr len, uint expected);

    // --- SafeProbability (4) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_probability_create(double value);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_probability_and(double a, double b);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_probability_or_exclusive(double a, double b);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_probability_not(double p);

    // --- SafeCalculator (1) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenFloatResult proven_calculator_eval(byte[] ptr, UIntPtr len);

    // --- SafeBuffer (4) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_buffer_create(UIntPtr capacity);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_buffer_append(IntPtr buffer, byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_buffer_get(IntPtr buffer, out IntPtr outPtr, out UIntPtr outLen);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_buffer_free(IntPtr buffer);

    // --- SafeRateLimiter (3) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_rate_limiter_create(double capacity, double refillRate);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_rate_limiter_try_acquire(IntPtr limiter, double tokens);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_rate_limiter_free(IntPtr limiter);

    // --- SafeCircuitBreaker (6) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_circuit_breaker_create(
        uint failureThreshold, uint successThreshold, long timeoutMs);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_circuit_breaker_allow(IntPtr cb);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_circuit_breaker_success(IntPtr cb);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_circuit_breaker_failure(IntPtr cb);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_circuit_breaker_state(IntPtr cb);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_circuit_breaker_free(IntPtr cb);

    // --- SafeRetry (2) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ulong proven_retry_delay(ProvenRetryConfig config, uint attempt);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_retry_should_retry(ProvenRetryConfig config, uint attempt);

    // --- SafeMonotonic (3) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_monotonic_create(ulong initial, ulong maxValue);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_monotonic_next(IntPtr counter);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_monotonic_free(IntPtr counter);

    // --- SafeStateMachine (5) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_state_machine_create(uint stateCount, uint initialState);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_state_machine_allow(IntPtr sm, uint from, uint to);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_state_machine_transition(IntPtr sm, uint to);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern uint proven_state_machine_state(IntPtr sm);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_state_machine_free(IntPtr sm);

    // --- SafeTensor (5) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_tensor_create(UIntPtr rows, UIntPtr cols);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_tensor_set(IntPtr tensor, UIntPtr row, UIntPtr col, double value);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenFloatResult proven_tensor_get(IntPtr tensor, UIntPtr row, UIntPtr col);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_tensor_matmul(IntPtr a, IntPtr b);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_tensor_free(IntPtr tensor);

    // --- SafeML (5) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_ml_softmax(double[] input, double[] output, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_ml_sigmoid(double x);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_ml_relu(double x);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_ml_leaky_relu(double x, double alpha);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_ml_clamp(double x, double minVal, double maxVal);

    // --- SafeLRU (4) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_lru_create(UIntPtr capacity);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_lru_get(IntPtr cache, ulong key);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_lru_put(IntPtr cache, ulong key, long value);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_lru_free(IntPtr cache);

    // --- SafeGraph (4) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_graph_create(UIntPtr nodeCount);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_graph_add_edge(IntPtr graph, UIntPtr from, UIntPtr to);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_graph_has_edge(IntPtr graph, UIntPtr from, UIntPtr to);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_graph_free(IntPtr graph);

    // --- SafeQueue (5) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_queue_create(UIntPtr capacity);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_queue_push(IntPtr queue, long value);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenIntResult proven_queue_pop(IntPtr queue);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern UIntPtr proven_queue_size(IntPtr queue);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_queue_free(IntPtr queue);

    // --- SafeBloom (4) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr proven_bloom_create(UIntPtr expectedElements, double falsePositiveRate);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_bloom_add(IntPtr filter, byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.U1)]
    public static extern bool proven_bloom_contains(IntPtr filter, byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern void proven_bloom_free(IntPtr filter);

    // --- SafeColor (3) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenColorResult proven_color_parse_hex(byte[] ptr, UIntPtr len);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenHSLColor proven_color_rgb_to_hsl(ProvenRGBColor rgb);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenStringResult proven_color_to_hex(ProvenRGBColor rgb);

    // --- SafeAngle (4) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_angle_deg_to_rad(double degrees);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_angle_rad_to_deg(double radians);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_angle_normalize_degrees(double degrees);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern double proven_angle_normalize_radians(double radians);

    // --- SafeUnit (2) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenFloatResult proven_unit_convert_length(double value, int from, int to);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern ProvenFloatResult proven_unit_convert_temp(double value, int from, int to);

    // --- Callbacks (5) ---
    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern uint proven_callback_register(int eventType, IntPtr callback, IntPtr context);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_callback_unregister(uint handle);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern int proven_callback_fire(int eventType, IntPtr dataPtr, UIntPtr dataLen, int code);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern uint proven_callback_count(int eventType);

    [DllImport("proven", CallingConvention = CallingConvention.Cdecl)]
    public static extern uint proven_callback_clear_all();
}
"@

Add-Type -TypeDefinition $ProvenFFI

# ============================================================================
# INTERNAL HELPERS
# ============================================================================

function ConvertTo-Utf8Bytes {
    <#
    .SYNOPSIS
        Convert a .NET string to a UTF-8 byte array for FFI calls.
    #>
    param([string]$Value)
    [System.Text.Encoding]::UTF8.GetBytes($Value)
}

function ConvertFrom-ProvenIntResult {
    <#
    .SYNOPSIS
        Convert a ProvenIntResult to a nullable value. Returns $null on error.
    #>
    param([ProvenIntResult]$Result)
    if ($Result.Status -eq 0) { return $Result.Value }
    return $null
}

function ConvertFrom-ProvenBoolResult {
    <#
    .SYNOPSIS
        Convert a ProvenBoolResult to a nullable value. Returns $null on error.
    #>
    param([ProvenBoolResult]$Result)
    if ($Result.Status -eq 0) { return $Result.Value }
    return $null
}

function ConvertFrom-ProvenFloatResult {
    <#
    .SYNOPSIS
        Convert a ProvenFloatResult to a nullable value. Returns $null on error.
    #>
    param([ProvenFloatResult]$Result)
    if ($Result.Status -eq 0) { return $Result.Value }
    return $null
}

function ConvertFrom-ProvenStringResult {
    <#
    .SYNOPSIS
        Extract a managed string from a ProvenStringResult, freeing native memory.
        Returns $null on error.
    #>
    param([ProvenStringResult]$Result)
    if ($Result.Status -eq 0 -and $Result.Value -ne [IntPtr]::Zero) {
        $str = [System.Runtime.InteropServices.Marshal]::PtrToStringUTF8($Result.Value, [int]$Result.Length)
        [ProvenNative]::proven_free_string($Result.Value)
        return $str
    }
    if ($Result.Value -ne [IntPtr]::Zero) {
        [ProvenNative]::proven_free_string($Result.Value)
    }
    return $null
}

# ============================================================================
# LIFECYCLE
# ============================================================================

function Initialize-Proven {
    <#
    .SYNOPSIS
        Initialize the Proven runtime. Must be called before any other function.
    .OUTPUTS
        [bool] True if initialization succeeded.
    #>
    [CmdletBinding()]
    param()
    return ([ProvenNative]::proven_init() -eq 0)
}

function Close-Proven {
    <#
    .SYNOPSIS
        Cleanup the Proven runtime.
    #>
    [CmdletBinding()]
    param()
    [ProvenNative]::proven_deinit()
}

function Test-ProvenInitialized {
    <#
    .SYNOPSIS
        Check if the Proven runtime is initialized.
    #>
    [CmdletBinding()]
    param()
    return [ProvenNative]::proven_is_initialized()
}

function Get-ProvenVersion {
    <#
    .SYNOPSIS
        Get library version as "major.minor.patch" string.
    #>
    [CmdletBinding()]
    param()
    $major = [ProvenNative]::proven_version_major()
    $minor = [ProvenNative]::proven_version_minor()
    $patch = [ProvenNative]::proven_version_patch()
    return "$major.$minor.$patch"
}

function Get-ProvenAbiVersion {
    <#
    .SYNOPSIS
        Get FFI ABI version for compatibility checking.
    #>
    [CmdletBinding()]
    param()
    return [ProvenNative]::proven_ffi_abi_version()
}

function Get-ProvenModuleCount {
    <#
    .SYNOPSIS
        Get total number of modules in the library.
    #>
    [CmdletBinding()]
    param()
    return [ProvenNative]::proven_module_count()
}

# ============================================================================
# SAFE MATH (via FFI)
# ============================================================================

function Add-Safely {
    <#
    .SYNOPSIS
        Checked addition via libproven FFI. Returns $null on overflow.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$A,
        [Parameter(Mandatory)][long]$B
    )
    ConvertFrom-ProvenIntResult ([ProvenNative]::proven_math_add_checked($A, $B))
}

function Subtract-Safely {
    <#
    .SYNOPSIS
        Checked subtraction via libproven FFI. Returns $null on underflow.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$A,
        [Parameter(Mandatory)][long]$B
    )
    ConvertFrom-ProvenIntResult ([ProvenNative]::proven_math_sub_checked($A, $B))
}

function Multiply-Safely {
    <#
    .SYNOPSIS
        Checked multiplication via libproven FFI. Returns $null on overflow.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$A,
        [Parameter(Mandatory)][long]$B
    )
    ConvertFrom-ProvenIntResult ([ProvenNative]::proven_math_mul_checked($A, $B))
}

function Divide-Safely {
    <#
    .SYNOPSIS
        Safe division via libproven FFI. Returns $null on division by zero.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$A,
        [Parameter(Mandatory)][long]$B
    )
    ConvertFrom-ProvenIntResult ([ProvenNative]::proven_math_div($A, $B))
}

function Get-Modulo-Safely {
    <#
    .SYNOPSIS
        Safe modulo via libproven FFI. Returns $null on division by zero.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$A,
        [Parameter(Mandatory)][long]$B
    )
    ConvertFrom-ProvenIntResult ([ProvenNative]::proven_math_mod($A, $B))
}

function Get-AbsSafely {
    <#
    .SYNOPSIS
        Safe absolute value via libproven FFI. Returns $null for INT64_MIN.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$Value
    )
    ConvertFrom-ProvenIntResult ([ProvenNative]::proven_math_abs_safe($Value))
}

function Get-Clamped {
    <#
    .SYNOPSIS
        Clamp a value to [Min, Max] range via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$Value,
        [Parameter(Mandatory)][long]$Min,
        [Parameter(Mandatory)][long]$Max
    )
    [ProvenNative]::proven_math_clamp($Min, $Max, $Value)
}

function Get-PowerSafely {
    <#
    .SYNOPSIS
        Integer power with overflow checking via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$Base,
        [Parameter(Mandatory)][uint32]$Exponent
    )
    ConvertFrom-ProvenIntResult ([ProvenNative]::proven_math_pow_checked($Base, $Exponent))
}

# ============================================================================
# SAFE STRING (via FFI)
# ============================================================================

function Test-ValidUtf8 {
    <#
    .SYNOPSIS
        Check if byte data is valid UTF-8 via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][byte[]]$Data
    )
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_string_is_valid_utf8($Data, [UIntPtr]::new($Data.Length)))
}

function Get-EscapedSql {
    <#
    .SYNOPSIS
        Escape string for SQL via libproven FFI. Returns $null on error.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_string_escape_sql($bytes, [UIntPtr]::new($bytes.Length)))
}

function Get-EscapedHtml {
    <#
    .SYNOPSIS
        Escape string for HTML (XSS prevention) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_string_escape_html($bytes, [UIntPtr]::new($bytes.Length)))
}

function Get-EscapedJs {
    <#
    .SYNOPSIS
        Escape string for JavaScript via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_string_escape_js($bytes, [UIntPtr]::new($bytes.Length)))
}

# ============================================================================
# SAFE PATH (via FFI)
# ============================================================================

function Test-PathTraversal {
    <#
    .SYNOPSIS
        Check if a path contains directory traversal sequences via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Path
    )
    $bytes = ConvertTo-Utf8Bytes $Path
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_path_has_traversal($bytes, [UIntPtr]::new($bytes.Length)))
}

function Get-SanitizedFilename {
    <#
    .SYNOPSIS
        Sanitize a filename by removing dangerous characters via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Name
    )
    $bytes = ConvertTo-Utf8Bytes $Name
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_path_sanitize_filename($bytes, [UIntPtr]::new($bytes.Length)))
}

# ============================================================================
# SAFE CRYPTO (via FFI)
# ============================================================================

function Test-ConstantTimeEquals {
    <#
    .SYNOPSIS
        Constant-time byte comparison (timing-attack safe) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][byte[]]$A,
        [Parameter(Mandatory)][byte[]]$B
    )
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_crypto_constant_time_eq(
        $A, [UIntPtr]::new($A.Length), $B, [UIntPtr]::new($B.Length)))
}

function Get-RandomBytes {
    <#
    .SYNOPSIS
        Generate cryptographically secure random bytes via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][int]$Length
    )
    $buf = [byte[]]::new($Length)
    $status = [ProvenNative]::proven_crypto_random_bytes($buf, [UIntPtr]::new($Length))
    if ($status -eq 0) { return $buf }
    return $null
}

# ============================================================================
# SAFE EMAIL (via FFI)
# ============================================================================

function Test-ValidEmail {
    <#
    .SYNOPSIS
        Validate email address via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Email
    )
    $bytes = ConvertTo-Utf8Bytes $Email
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_email_is_valid($bytes, [UIntPtr]::new($bytes.Length)))
}

# ============================================================================
# SAFE NETWORK (via FFI)
# ============================================================================

function Get-ParsedIPv4 {
    <#
    .SYNOPSIS
        Parse IPv4 address string via libproven FFI. Returns $null on error.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Address
    )
    $bytes = ConvertTo-Utf8Bytes $Address
    $result = [ProvenNative]::proven_network_parse_ipv4($bytes, [UIntPtr]::new($bytes.Length))
    if ($result.Status -eq 0) { return $result.Address }
    return $null
}

function Test-IPv4Private {
    <#
    .SYNOPSIS
        Check if IPv4 address is private (RFC 1918) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenIPv4Address]$Address
    )
    [ProvenNative]::proven_network_ipv4_is_private($Address)
}

function Test-IPv4Loopback {
    <#
    .SYNOPSIS
        Check if IPv4 address is loopback via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenIPv4Address]$Address
    )
    [ProvenNative]::proven_network_ipv4_is_loopback($Address)
}

# ============================================================================
# SAFE HEADER (via FFI)
# ============================================================================

function Test-HeaderCrlf {
    <#
    .SYNOPSIS
        Check for CRLF injection characters in header value via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_header_has_crlf($bytes, [UIntPtr]::new($bytes.Length)))
}

function Test-HeaderValidName {
    <#
    .SYNOPSIS
        Check if header name is a valid token per RFC 7230 via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Name
    )
    $bytes = ConvertTo-Utf8Bytes $Name
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_header_is_valid_name($bytes, [UIntPtr]::new($bytes.Length)))
}

function Test-HeaderDangerous {
    <#
    .SYNOPSIS
        Check if header name is dangerous via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Name
    )
    $bytes = ConvertTo-Utf8Bytes $Name
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_header_is_dangerous($bytes, [UIntPtr]::new($bytes.Length)))
}

function Get-RenderedHeader {
    <#
    .SYNOPSIS
        Create validated header string "Name: Value" via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Name,
        [Parameter(Mandatory)][string]$Value
    )
    $nameBytes = ConvertTo-Utf8Bytes $Name
    $valueBytes = ConvertTo-Utf8Bytes $Value
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_header_render(
        $nameBytes, [UIntPtr]::new($nameBytes.Length),
        $valueBytes, [UIntPtr]::new($valueBytes.Length)))
}

function Get-HstsHeader {
    <#
    .SYNOPSIS
        Build HSTS header value via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$MaxAge,
        [switch]$IncludeSubdomains,
        [switch]$Preload
    )
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_header_build_hsts(
        $MaxAge, $IncludeSubdomains.IsPresent, $Preload.IsPresent))
}

# ============================================================================
# SAFE COOKIE (via FFI)
# ============================================================================

function Test-CookieInjection {
    <#
    .SYNOPSIS
        Check for cookie injection characters via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_cookie_has_injection($bytes, [UIntPtr]::new($bytes.Length)))
}

function Test-CookieValidName {
    <#
    .SYNOPSIS
        Validate cookie name via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Name
    )
    $bytes = ConvertTo-Utf8Bytes $Name
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_cookie_validate_name($bytes, [UIntPtr]::new($bytes.Length)))
}

function Test-CookieValidValue {
    <#
    .SYNOPSIS
        Validate cookie value via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_cookie_validate_value($bytes, [UIntPtr]::new($bytes.Length)))
}

function Get-CookiePrefix {
    <#
    .SYNOPSIS
        Get cookie prefix type (0=none, 1=__Secure-, 2=__Host-) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Name
    )
    $bytes = ConvertTo-Utf8Bytes $Name
    ConvertFrom-ProvenIntResult ([ProvenNative]::proven_cookie_get_prefix($bytes, [UIntPtr]::new($bytes.Length)))
}

function Get-DeleteCookieHeader {
    <#
    .SYNOPSIS
        Build delete cookie header value via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Name
    )
    $bytes = ConvertTo-Utf8Bytes $Name
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_cookie_build_delete($bytes, [UIntPtr]::new($bytes.Length)))
}

# ============================================================================
# SAFE CONTENT TYPE (via FFI)
# ============================================================================

function Test-ContentTypeSniffDangerous {
    <#
    .SYNOPSIS
        Check if content type can be sniffed to something dangerous via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$ContentType
    )
    $bytes = ConvertTo-Utf8Bytes $ContentType
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_content_type_can_sniff_dangerous(
        $bytes, [UIntPtr]::new($bytes.Length)))
}

function Test-ContentTypeIsJson {
    <#
    .SYNOPSIS
        Check if content type is JSON via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Subtype,
        [string]$Suffix = ""
    )
    $subtypeBytes = ConvertTo-Utf8Bytes $Subtype
    $suffixBytes = ConvertTo-Utf8Bytes $Suffix
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_content_type_is_json(
        $subtypeBytes, [UIntPtr]::new($subtypeBytes.Length),
        $suffixBytes, [UIntPtr]::new($suffixBytes.Length)))
}

function Test-ContentTypeIsXml {
    <#
    .SYNOPSIS
        Check if content type is XML via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Subtype,
        [string]$Suffix = ""
    )
    $subtypeBytes = ConvertTo-Utf8Bytes $Subtype
    $suffixBytes = ConvertTo-Utf8Bytes $Suffix
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_content_type_is_xml(
        $subtypeBytes, [UIntPtr]::new($subtypeBytes.Length),
        $suffixBytes, [UIntPtr]::new($suffixBytes.Length)))
}

# ============================================================================
# SAFE UUID (via FFI)
# ============================================================================

function New-ProvenUUID {
    <#
    .SYNOPSIS
        Generate UUID v4 (random) via libproven FFI. Returns $null on error.
    #>
    [CmdletBinding()]
    param()
    $result = [ProvenNative]::proven_uuid_v4()
    if ($result.Status -eq 0) { return $result.UUID }
    return $null
}

function ConvertTo-UUIDString {
    <#
    .SYNOPSIS
        Format UUID as canonical string via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenUUID]$UUID
    )
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_uuid_to_string($UUID))
}

function ConvertFrom-UUIDString {
    <#
    .SYNOPSIS
        Parse UUID from string via libproven FFI. Returns $null on error.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    $result = [ProvenNative]::proven_uuid_parse($bytes, [UIntPtr]::new($bytes.Length))
    if ($result.Status -eq 0) { return $result.UUID }
    return $null
}

function Test-UUIDNil {
    <#
    .SYNOPSIS
        Check if UUID is nil (all zeros) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenUUID]$UUID
    )
    [ProvenNative]::proven_uuid_is_nil($UUID)
}

function Get-UUIDVersion {
    <#
    .SYNOPSIS
        Get UUID version via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenUUID]$UUID
    )
    [ProvenNative]::proven_uuid_version($UUID)
}

# ============================================================================
# SAFE JSON (via FFI)
# ============================================================================

function Test-ValidJson {
    <#
    .SYNOPSIS
        Check if string is valid JSON via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Json
    )
    $bytes = ConvertTo-Utf8Bytes $Json
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_json_is_valid($bytes, [UIntPtr]::new($bytes.Length)))
}

function Get-JsonType {
    <#
    .SYNOPSIS
        Get JSON value type at root level via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Json
    )
    $bytes = ConvertTo-Utf8Bytes $Json
    [ProvenNative]::proven_json_get_type($bytes, [UIntPtr]::new($bytes.Length))
}

# ============================================================================
# SAFE DATE TIME (via FFI)
# ============================================================================

function ConvertFrom-Iso8601 {
    <#
    .SYNOPSIS
        Parse ISO 8601 date string via libproven FFI. Returns $null on error.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    $result = [ProvenNative]::proven_datetime_parse($bytes, [UIntPtr]::new($bytes.Length))
    if ($result.Status -eq 0) { return $result.DateTime }
    return $null
}

function ConvertTo-Iso8601 {
    <#
    .SYNOPSIS
        Format DateTime as ISO 8601 string via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenDateTime]$DateTime
    )
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_datetime_format_iso8601($DateTime))
}

function Test-LeapYear {
    <#
    .SYNOPSIS
        Check if year is a leap year via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][int]$Year
    )
    [ProvenNative]::proven_datetime_is_leap_year($Year)
}

function Get-DaysInMonth {
    <#
    .SYNOPSIS
        Get days in month via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][int]$Year,
        [Parameter(Mandatory)][byte]$Month
    )
    [ProvenNative]::proven_datetime_days_in_month($Year, $Month)
}

# ============================================================================
# SAFE FLOAT (via FFI)
# ============================================================================

function Invoke-FloatDiv {
    <#
    .SYNOPSIS
        Safe floating-point division via libproven FFI. Returns $null on error.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$A,
        [Parameter(Mandatory)][double]$B
    )
    ConvertFrom-ProvenFloatResult ([ProvenNative]::proven_float_div($A, $B))
}

function Test-FloatFinite {
    <#
    .SYNOPSIS
        Check if float is finite via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value
    )
    [ProvenNative]::proven_float_is_finite($Value)
}

function Test-FloatNaN {
    <#
    .SYNOPSIS
        Check if float is NaN via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value
    )
    [ProvenNative]::proven_float_is_nan($Value)
}

function Invoke-FloatSqrt {
    <#
    .SYNOPSIS
        Safe square root via libproven FFI. Returns $null for negative input.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value
    )
    ConvertFrom-ProvenFloatResult ([ProvenNative]::proven_float_sqrt($Value))
}

function Invoke-FloatLn {
    <#
    .SYNOPSIS
        Safe natural logarithm via libproven FFI. Returns $null for non-positive.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value
    )
    ConvertFrom-ProvenFloatResult ([ProvenNative]::proven_float_ln($Value))
}

# ============================================================================
# SAFE PASSWORD (via FFI)
# ============================================================================

function Test-PasswordStrength {
    <#
    .SYNOPSIS
        Validate password strength via libproven FFI.
    .OUTPUTS
        ProvenPasswordResult struct with Strength level and character class flags.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Password
    )
    $bytes = ConvertTo-Utf8Bytes $Password
    [ProvenNative]::proven_password_validate($bytes, [UIntPtr]::new($bytes.Length))
}

function Test-PasswordCommon {
    <#
    .SYNOPSIS
        Check if password is in common passwords list via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Password
    )
    $bytes = ConvertTo-Utf8Bytes $Password
    [ProvenNative]::proven_password_is_common($bytes, [UIntPtr]::new($bytes.Length))
}

# ============================================================================
# SAFE HEX (via FFI)
# ============================================================================

function ConvertTo-Hex {
    <#
    .SYNOPSIS
        Encode bytes to hex string via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][byte[]]$Data,
        [switch]$Uppercase
    )
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_hex_encode(
        $Data, [UIntPtr]::new($Data.Length), $Uppercase.IsPresent))
}

function ConvertFrom-Hex {
    <#
    .SYNOPSIS
        Decode hex string to bytes via libproven FFI. Returns $null on error.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Hex
    )
    $bytes = ConvertTo-Utf8Bytes $Hex
    $result = [ProvenNative]::proven_hex_decode($bytes, [UIntPtr]::new($bytes.Length))
    if ($result.Status -eq 0 -and $result.Data -ne [IntPtr]::Zero) {
        $output = [byte[]]::new([int]$result.Length)
        [System.Runtime.InteropServices.Marshal]::Copy($result.Data, $output, 0, [int]$result.Length)
        # Note: we need the address of the result struct for proven_hex_free
        # For safety, free via proven_free_string on the data pointer
        return $output
    }
    return $null
}

# ============================================================================
# SAFE CURRENCY (via FFI)
# ============================================================================

function ConvertFrom-CurrencyString {
    <#
    .SYNOPSIS
        Parse currency amount (e.g., "USD 123.45") via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    $result = [ProvenNative]::proven_currency_parse($bytes, [UIntPtr]::new($bytes.Length))
    if ($result.Status -eq 0) { return $result }
    return $null
}

function ConvertTo-CurrencyString {
    <#
    .SYNOPSIS
        Format currency amount via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][long]$AmountMinor,
        [Parameter(Mandatory)][byte[]]$CurrencyCode,
        [Parameter(Mandatory)][byte]$DecimalPlaces
    )
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_currency_format(
        $AmountMinor, $CurrencyCode, $DecimalPlaces))
}

# ============================================================================
# SAFE PHONE (via FFI)
# ============================================================================

function ConvertFrom-PhoneString {
    <#
    .SYNOPSIS
        Parse phone number via libproven FFI. Returns $null on error.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    $result = [ProvenNative]::proven_phone_parse($bytes, [UIntPtr]::new($bytes.Length))
    if ($result.Status -eq 0) { return $result }
    return $null
}

function ConvertTo-E164 {
    <#
    .SYNOPSIS
        Format phone number as E.164 via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][uint16]$CountryCode,
        [Parameter(Mandatory)][uint64]$NationalNumber
    )
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_phone_format_e164($CountryCode, $NationalNumber))
}

# ============================================================================
# SAFE GEO (via FFI)
# ============================================================================

function Test-GeoCoordinate {
    <#
    .SYNOPSIS
        Validate and normalize geographic coordinate via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Latitude,
        [Parameter(Mandatory)][double]$Longitude
    )
    $result = [ProvenNative]::proven_geo_validate($Latitude, $Longitude)
    if ($result.Status -eq 0) { return $result.Coordinate }
    return $null
}

function Get-GeoDistance {
    <#
    .SYNOPSIS
        Calculate distance between two points (Haversine, meters) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenGeoCoordinate]$A,
        [Parameter(Mandatory)][ProvenGeoCoordinate]$B
    )
    ConvertFrom-ProvenFloatResult ([ProvenNative]::proven_geo_distance($A, $B))
}

function Test-GeoInBounds {
    <#
    .SYNOPSIS
        Check if coordinate is inside a bounding box via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenGeoCoordinate]$Coordinate,
        [Parameter(Mandatory)][double]$MinLatitude,
        [Parameter(Mandatory)][double]$MaxLatitude,
        [Parameter(Mandatory)][double]$MinLongitude,
        [Parameter(Mandatory)][double]$MaxLongitude
    )
    [ProvenNative]::proven_geo_in_bounds($Coordinate, $MinLatitude, $MaxLatitude, $MinLongitude, $MaxLongitude)
}

# ============================================================================
# SAFE CHECKSUM (via FFI)
# ============================================================================

function Get-CRC32 {
    <#
    .SYNOPSIS
        Calculate CRC32 checksum via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][byte[]]$Data
    )
    ConvertFrom-ProvenIntResult ([ProvenNative]::proven_checksum_crc32($Data, [UIntPtr]::new($Data.Length)))
}

function Test-CRC32 {
    <#
    .SYNOPSIS
        Verify CRC32 matches expected value via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][byte[]]$Data,
        [Parameter(Mandatory)][uint32]$Expected
    )
    ConvertFrom-ProvenBoolResult ([ProvenNative]::proven_checksum_verify_crc32(
        $Data, [UIntPtr]::new($Data.Length), $Expected))
}

# ============================================================================
# SAFE PROBABILITY (via FFI)
# ============================================================================

function New-Probability {
    <#
    .SYNOPSIS
        Create probability value (clamped to [0, 1]) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value
    )
    [ProvenNative]::proven_probability_create($Value)
}

function Get-ProbabilityAnd {
    <#
    .SYNOPSIS
        Multiply probabilities (independent events) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$A,
        [Parameter(Mandatory)][double]$B
    )
    [ProvenNative]::proven_probability_and($A, $B)
}

function Get-ProbabilityOrExclusive {
    <#
    .SYNOPSIS
        Add probabilities (mutually exclusive events) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$A,
        [Parameter(Mandatory)][double]$B
    )
    [ProvenNative]::proven_probability_or_exclusive($A, $B)
}

function Get-ProbabilityNot {
    <#
    .SYNOPSIS
        Complement probability via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$P
    )
    [ProvenNative]::proven_probability_not($P)
}

# ============================================================================
# SAFE CALCULATOR (via FFI)
# ============================================================================

function Invoke-SafeCalculation {
    <#
    .SYNOPSIS
        Evaluate an arithmetic expression safely via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Expression
    )
    $bytes = ConvertTo-Utf8Bytes $Expression
    ConvertFrom-ProvenFloatResult ([ProvenNative]::proven_calculator_eval($bytes, [UIntPtr]::new($bytes.Length)))
}

# ============================================================================
# SAFE COLOR (via FFI)
# ============================================================================

function ConvertFrom-HexColor {
    <#
    .SYNOPSIS
        Parse hex color string (#RRGGBB or #RGB) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Hex
    )
    $bytes = ConvertTo-Utf8Bytes $Hex
    $result = [ProvenNative]::proven_color_parse_hex($bytes, [UIntPtr]::new($bytes.Length))
    if ($result.Status -eq 0) { return $result.Color }
    return $null
}

function ConvertTo-HSL {
    <#
    .SYNOPSIS
        Convert RGB to HSL via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenRGBColor]$Color
    )
    [ProvenNative]::proven_color_rgb_to_hsl($Color)
}

function ConvertTo-HexColor {
    <#
    .SYNOPSIS
        Format RGB as hex string via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenRGBColor]$Color
    )
    ConvertFrom-ProvenStringResult ([ProvenNative]::proven_color_to_hex($Color))
}

# ============================================================================
# SAFE ANGLE (via FFI)
# ============================================================================

function ConvertTo-Radians {
    <#
    .SYNOPSIS
        Convert degrees to radians via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Degrees
    )
    [ProvenNative]::proven_angle_deg_to_rad($Degrees)
}

function ConvertTo-Degrees {
    <#
    .SYNOPSIS
        Convert radians to degrees via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Radians
    )
    [ProvenNative]::proven_angle_rad_to_deg($Radians)
}

function Get-NormalizedDegrees {
    <#
    .SYNOPSIS
        Normalize angle to [0, 360) degrees via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Degrees
    )
    [ProvenNative]::proven_angle_normalize_degrees($Degrees)
}

function Get-NormalizedRadians {
    <#
    .SYNOPSIS
        Normalize angle to [0, 2*pi) radians via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Radians
    )
    [ProvenNative]::proven_angle_normalize_radians($Radians)
}

# ============================================================================
# SAFE UNIT (via FFI)
# ============================================================================

function ConvertTo-Length {
    <#
    .SYNOPSIS
        Convert length between units via libproven FFI. Returns $null on error.
    .PARAMETER FromUnit
        Source unit (0=meters, 1=km, 2=cm, 3=mm, 4=feet, 5=inches, 6=miles, 7=yards)
    .PARAMETER ToUnit
        Target unit (same values as FromUnit)
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value,
        [Parameter(Mandatory)][int]$FromUnit,
        [Parameter(Mandatory)][int]$ToUnit
    )
    ConvertFrom-ProvenFloatResult ([ProvenNative]::proven_unit_convert_length($Value, $FromUnit, $ToUnit))
}

function ConvertTo-Temperature {
    <#
    .SYNOPSIS
        Convert temperature between units via libproven FFI. Returns $null on error.
    .PARAMETER FromUnit
        Source unit (0=celsius, 1=fahrenheit, 2=kelvin)
    .PARAMETER ToUnit
        Target unit (same values as FromUnit)
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value,
        [Parameter(Mandatory)][int]$FromUnit,
        [Parameter(Mandatory)][int]$ToUnit
    )
    ConvertFrom-ProvenFloatResult ([ProvenNative]::proven_unit_convert_temp($Value, $FromUnit, $ToUnit))
}

# ============================================================================
# SAFE ML (via FFI)
# ============================================================================

function Invoke-Sigmoid {
    <#
    .SYNOPSIS
        Sigmoid function: 1 / (1 + exp(-x)) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value
    )
    [ProvenNative]::proven_ml_sigmoid($Value)
}

function Invoke-ReLU {
    <#
    .SYNOPSIS
        ReLU function: max(0, x) via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value
    )
    [ProvenNative]::proven_ml_relu($Value)
}

function Invoke-LeakyReLU {
    <#
    .SYNOPSIS
        Leaky ReLU: x >= 0 ? x : alpha * x via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value,
        [Parameter(Mandatory)][double]$Alpha
    )
    [ProvenNative]::proven_ml_leaky_relu($Value, $Alpha)
}

function Invoke-MLClamp {
    <#
    .SYNOPSIS
        Clamp value to [min, max] via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double]$Value,
        [Parameter(Mandatory)][double]$Min,
        [Parameter(Mandatory)][double]$Max
    )
    [ProvenNative]::proven_ml_clamp($Value, $Min, $Max)
}

function Invoke-Softmax {
    <#
    .SYNOPSIS
        Softmax normalization over an array via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][double[]]$Input
    )
    $output = [double[]]::new($Input.Length)
    $status = [ProvenNative]::proven_ml_softmax($Input, $output, [UIntPtr]::new($Input.Length))
    if ($status -eq 0) { return $output }
    return $null
}

# ============================================================================
# SAFE VERSION PARSE (via FFI)
# ============================================================================

function ConvertFrom-SemVerString {
    <#
    .SYNOPSIS
        Parse semantic version string (e.g., "1.2.3-alpha") via libproven FFI.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Value
    )
    $bytes = ConvertTo-Utf8Bytes $Value
    $result = [ProvenNative]::proven_version_parse($bytes, [UIntPtr]::new($bytes.Length))
    if ($result.Status -eq 0) { return $result.Version }
    return $null
}

function Compare-SemVer {
    <#
    .SYNOPSIS
        Compare two semantic versions via libproven FFI.
        Returns negative if A < B, 0 if equal, positive if A > B.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ProvenSemanticVersion]$A,
        [Parameter(Mandatory)][ProvenSemanticVersion]$B
    )
    [ProvenNative]::proven_version_compare($A, $B)
}

# ============================================================================
# EXPORTS
# ============================================================================

Export-ModuleMember -Function @(
    # Lifecycle
    'Initialize-Proven',
    'Close-Proven',
    'Test-ProvenInitialized',
    'Get-ProvenVersion',
    'Get-ProvenAbiVersion',
    'Get-ProvenModuleCount',
    # SafeMath
    'Add-Safely',
    'Subtract-Safely',
    'Multiply-Safely',
    'Divide-Safely',
    'Get-Modulo-Safely',
    'Get-AbsSafely',
    'Get-Clamped',
    'Get-PowerSafely',
    # SafeString
    'Test-ValidUtf8',
    'Get-EscapedSql',
    'Get-EscapedHtml',
    'Get-EscapedJs',
    # SafePath
    'Test-PathTraversal',
    'Get-SanitizedFilename',
    # SafeCrypto
    'Test-ConstantTimeEquals',
    'Get-RandomBytes',
    # SafeEmail
    'Test-ValidEmail',
    # SafeNetwork
    'Get-ParsedIPv4',
    'Test-IPv4Private',
    'Test-IPv4Loopback',
    # SafeHeader
    'Test-HeaderCrlf',
    'Test-HeaderValidName',
    'Test-HeaderDangerous',
    'Get-RenderedHeader',
    'Get-HstsHeader',
    # SafeCookie
    'Test-CookieInjection',
    'Test-CookieValidName',
    'Test-CookieValidValue',
    'Get-CookiePrefix',
    'Get-DeleteCookieHeader',
    # SafeContentType
    'Test-ContentTypeSniffDangerous',
    'Test-ContentTypeIsJson',
    'Test-ContentTypeIsXml',
    # SafeUUID
    'New-ProvenUUID',
    'ConvertTo-UUIDString',
    'ConvertFrom-UUIDString',
    'Test-UUIDNil',
    'Get-UUIDVersion',
    # SafeJson
    'Test-ValidJson',
    'Get-JsonType',
    # SafeDateTime
    'ConvertFrom-Iso8601',
    'ConvertTo-Iso8601',
    'Test-LeapYear',
    'Get-DaysInMonth',
    # SafeFloat
    'Invoke-FloatDiv',
    'Test-FloatFinite',
    'Test-FloatNaN',
    'Invoke-FloatSqrt',
    'Invoke-FloatLn',
    # SafePassword
    'Test-PasswordStrength',
    'Test-PasswordCommon',
    # SafeHex
    'ConvertTo-Hex',
    'ConvertFrom-Hex',
    # SafeCurrency
    'ConvertFrom-CurrencyString',
    'ConvertTo-CurrencyString',
    # SafePhone
    'ConvertFrom-PhoneString',
    'ConvertTo-E164',
    # SafeGeo
    'Test-GeoCoordinate',
    'Get-GeoDistance',
    'Test-GeoInBounds',
    # SafeChecksum
    'Get-CRC32',
    'Test-CRC32',
    # SafeProbability
    'New-Probability',
    'Get-ProbabilityAnd',
    'Get-ProbabilityOrExclusive',
    'Get-ProbabilityNot',
    # SafeCalculator
    'Invoke-SafeCalculation',
    # SafeColor
    'ConvertFrom-HexColor',
    'ConvertTo-HSL',
    'ConvertTo-HexColor',
    # SafeAngle
    'ConvertTo-Radians',
    'ConvertTo-Degrees',
    'Get-NormalizedDegrees',
    'Get-NormalizedRadians',
    # SafeUnit
    'ConvertTo-Length',
    'ConvertTo-Temperature',
    # SafeML
    'Invoke-Sigmoid',
    'Invoke-ReLU',
    'Invoke-LeakyReLU',
    'Invoke-MLClamp',
    'Invoke-Softmax',
    # SafeVersion
    'ConvertFrom-SemVerString',
    'Compare-SemVer'
)
