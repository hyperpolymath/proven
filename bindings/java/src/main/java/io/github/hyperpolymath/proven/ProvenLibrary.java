// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;

import java.util.List;

/**
 * JNA interface to libproven (the Zig/Idris2 verified safety library).
 *
 * This interface declares every exported C function from ffi/zig/src/main.zig.
 * No logic is reimplemented; all calls delegate directly to the native library.
 *
 * C ABI result structs are mapped as JNA Structure subclasses.
 * Callers MUST call {@link #proven_free_string(Pointer)} for any non-null
 * string pointer returned by the library.
 */
public interface ProvenLibrary extends Library {

    /**
     * Singleton instance of the loaded native library.
     * Loads "libproven" (proven.dll on Windows, libproven.so on Linux,
     * libproven.dylib on macOS).
     */
    ProvenLibrary INSTANCE = Native.load("proven", ProvenLibrary.class);

    // ========================================================================
    // C ABI result structs (mirrors ffi/zig/src/main.zig)
    // ========================================================================

    /**
     * Result for integer operations.
     * Maps to: extern struct { status: ProvenStatus, value: i64 }
     */
    @Structure.FieldOrder({"status", "value"})
    class IntResult extends Structure implements Structure.ByValue {
        public int status;
        public long value;
    }

    /**
     * Result for boolean operations.
     * Maps to: extern struct { status: ProvenStatus, value: bool }
     */
    @Structure.FieldOrder({"status", "value"})
    class BoolResult extends Structure implements Structure.ByValue {
        public int status;
        public boolean value;
    }

    /**
     * Result for string operations.
     * Maps to: extern struct { status: ProvenStatus, value: ?[*:0]u8, length: usize }
     *
     * Callers MUST free the string via proven_free_string when done.
     */
    @Structure.FieldOrder({"status", "value", "length"})
    class StringResult extends Structure implements Structure.ByValue {
        public int status;
        public Pointer value;
        public long length;
    }

    /**
     * Result for float operations.
     * Maps to: extern struct { status: ProvenStatus, value: f64 }
     */
    @Structure.FieldOrder({"status", "value"})
    class FloatResult extends Structure implements Structure.ByValue {
        public int status;
        public double value;
    }

    // ========================================================================
    // Lifecycle
    // ========================================================================

    /** Initialize the proven library. Returns 0 on success. */
    int proven_init();

    /** Deinitialize the proven library. */
    void proven_deinit();

    /** Check if the library has been initialized. */
    boolean proven_is_initialized();

    /** Get the FFI ABI version. */
    int proven_ffi_abi_version();

    // ========================================================================
    // Memory management
    // ========================================================================

    /** Free a string allocated by any proven function. */
    void proven_free_string(Pointer ptr);

    // ========================================================================
    // Version info
    // ========================================================================

    int proven_version_major();
    int proven_version_minor();
    int proven_version_patch();
    int proven_module_count();

    // ========================================================================
    // SafeMath
    // ========================================================================

    IntResult proven_math_add_checked(long a, long b);
    IntResult proven_math_sub_checked(long a, long b);
    IntResult proven_math_mul_checked(long a, long b);
    IntResult proven_math_div(long numerator, long denominator);
    IntResult proven_math_mod(long numerator, long denominator);
    IntResult proven_math_abs_safe(long n);
    long proven_math_clamp(long lo, long hi, long value);
    IntResult proven_math_pow_checked(long base, int exp);

    // ========================================================================
    // SafeString
    // ========================================================================

    BoolResult proven_string_is_valid_utf8(Pointer ptr, long len);
    StringResult proven_string_escape_sql(Pointer ptr, long len);
    StringResult proven_string_escape_html(Pointer ptr, long len);
    StringResult proven_string_escape_js(Pointer ptr, long len);

    // ========================================================================
    // SafeCrypto
    // ========================================================================

    BoolResult proven_crypto_constant_time_eq(Pointer ptr1, long len1, Pointer ptr2, long len2);
    int proven_crypto_random_bytes(Pointer ptr, long len);

    // ========================================================================
    // SafePath
    // ========================================================================

    BoolResult proven_path_has_traversal(Pointer ptr, long len);
    StringResult proven_path_sanitize_filename(Pointer ptr, long len);

    // ========================================================================
    // SafeEmail
    // ========================================================================

    BoolResult proven_email_is_valid(Pointer ptr, long len);

    // ========================================================================
    // SafeNetwork
    // ========================================================================

    // IPv4Result has a different shape, but the core validation functions
    // return BoolResult or use the parse function which we handle specially.

    // ========================================================================
    // SafeHex
    // ========================================================================

    StringResult proven_hex_encode(Pointer ptr, long len, boolean uppercase);

    // ========================================================================
    // SafeUUID
    // ========================================================================

    // UUID is [16]u8, returned by value.
    // UUIDResult has status + 16-byte UUID. We handle via raw memory.

    // ========================================================================
    // SafeURL (HTTP URL encoding)
    // ========================================================================

    StringResult proven_http_url_encode(Pointer ptr, long len);
    StringResult proven_http_url_decode(Pointer ptr, long len);

    // ========================================================================
    // SafeFloat
    // ========================================================================

    FloatResult proven_float_div(double a, double b);
    boolean proven_float_is_finite(double x);
    boolean proven_float_is_nan(double x);
    FloatResult proven_float_sqrt(double x);
    FloatResult proven_float_ln(double x);

    // ========================================================================
    // SafeHeader
    // ========================================================================

    BoolResult proven_header_has_crlf(Pointer ptr, long len);
    BoolResult proven_header_is_valid_name(Pointer ptr, long len);
    BoolResult proven_header_is_dangerous(Pointer ptr, long len);
    StringResult proven_header_render(Pointer namePtr, long nameLen, Pointer valuePtr, long valueLen);
    StringResult proven_header_build_hsts(long maxAge, boolean includeSubdomains, boolean preload);

    // ========================================================================
    // SafeCookie
    // ========================================================================

    BoolResult proven_cookie_has_injection(Pointer ptr, long len);
    BoolResult proven_cookie_validate_name(Pointer ptr, long len);
    BoolResult proven_cookie_validate_value(Pointer ptr, long len);
    IntResult proven_cookie_get_prefix(Pointer ptr, long len);

    // ========================================================================
    // SafeJson
    // ========================================================================

    BoolResult proven_json_is_valid(Pointer ptr, long len);

    // ========================================================================
    // SafeChecksum
    // ========================================================================

    IntResult proven_checksum_crc32(Pointer ptr, long len);
    BoolResult proven_checksum_verify_crc32(Pointer ptr, long len, int expected);

    // ========================================================================
    // SafeProbability
    // ========================================================================

    double proven_probability_create(double value);
    double proven_probability_and(double a, double b);
    double proven_probability_or_exclusive(double a, double b);
    double proven_probability_not(double p);

    // ========================================================================
    // SafeDateTime
    // ========================================================================

    boolean proven_datetime_is_leap_year(int year);
    byte proven_datetime_days_in_month(int year, byte month);

    // ========================================================================
    // SafeML activation functions
    // ========================================================================

    double proven_ml_sigmoid(double x);
    double proven_ml_relu(double x);
    double proven_ml_leaky_relu(double x, double alpha);
    double proven_ml_clamp(double x, double minVal, double maxVal);

    // ========================================================================
    // SafeAngle
    // ========================================================================

    double proven_angle_deg_to_rad(double degrees);
    double proven_angle_rad_to_deg(double radians);
    double proven_angle_normalize_degrees(double degrees);
    double proven_angle_normalize_radians(double radians);

    // ========================================================================
    // SafeCalculator
    // ========================================================================

    FloatResult proven_calculator_eval(Pointer ptr, long len);
}
