// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// ProvenLibrary - JNA interface to libproven (Idris 2 + Zig C ABI).
// This is the sole entry point for all native calls.
// No logic is reimplemented; every call delegates to verified Idris code.
package com.hyperpolymath.proven

import com.sun.jna.Library
import com.sun.jna.Memory
import com.sun.jna.Native
import com.sun.jna.Pointer
import com.sun.jna.Structure
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets

/**
 * JNA interface to libproven.
 *
 * Mirrors the Java ProvenLibrary interface but as a Groovy-friendly class.
 * All computation delegates to Idris 2 verified code via the Zig FFI layer.
 */
@CompileStatic
interface ProvenLibrary extends Library {

    ProvenLibrary INSTANCE = Native.load('proven', ProvenLibrary) as ProvenLibrary

    // ---------- Result structs ----------

    @Structure.FieldOrder(['status', 'value'])
    class IntResult extends Structure implements Structure.ByValue {
        public int status
        public long value
    }

    @Structure.FieldOrder(['status', 'value'])
    class BoolResult extends Structure implements Structure.ByValue {
        public int status
        public boolean value
    }

    @Structure.FieldOrder(['status', 'value', 'length'])
    class StringResult extends Structure implements Structure.ByValue {
        public int status
        public Pointer value
        public long length
    }

    @Structure.FieldOrder(['status', 'value'])
    class FloatResult extends Structure implements Structure.ByValue {
        public int status
        public double value
    }

    // ---------- Lifecycle ----------

    int proven_init()
    void proven_deinit()
    boolean proven_is_initialized()
    int proven_ffi_abi_version()
    int proven_version_major()
    int proven_version_minor()
    int proven_version_patch()
    int proven_module_count()

    // ---------- Memory ----------

    void proven_free_string(Pointer ptr)

    // ---------- SafeMath ----------

    IntResult proven_math_add_checked(long a, long b)
    IntResult proven_math_sub_checked(long a, long b)
    IntResult proven_math_mul_checked(long a, long b)
    IntResult proven_math_div(long numerator, long denominator)
    IntResult proven_math_mod(long numerator, long denominator)
    IntResult proven_math_abs_safe(long n)
    long proven_math_clamp(long lo, long hi, long value)
    IntResult proven_math_pow_checked(long base, int exp)

    // ---------- SafeString ----------

    BoolResult proven_string_is_valid_utf8(Pointer ptr, long len)
    StringResult proven_string_escape_sql(Pointer ptr, long len)
    StringResult proven_string_escape_html(Pointer ptr, long len)
    StringResult proven_string_escape_js(Pointer ptr, long len)

    // ---------- SafePath ----------

    BoolResult proven_path_has_traversal(Pointer ptr, long len)
    StringResult proven_path_sanitize_filename(Pointer ptr, long len)

    // ---------- SafeEmail ----------

    BoolResult proven_email_is_valid(Pointer ptr, long len)

    // ---------- SafeCrypto ----------

    BoolResult proven_crypto_constant_time_eq(Pointer ptr1, long len1, Pointer ptr2, long len2)
    int proven_crypto_random_bytes(Pointer ptr, long len)

    // ---------- SafeNetwork (IPv4) ----------

    // Note: IPv4Result and IPv4Address structs handled via raw memory

    // ---------- SafeJson ----------

    BoolResult proven_json_is_valid(Pointer ptr, long len)

    // ---------- SafeFloat ----------

    FloatResult proven_float_div(double a, double b)
    boolean proven_float_is_finite(double x)
    boolean proven_float_is_nan(double x)
    FloatResult proven_float_sqrt(double x)
    FloatResult proven_float_ln(double x)

    // ---------- SafeHeader ----------

    BoolResult proven_header_has_crlf(Pointer ptr, long len)
    BoolResult proven_header_is_valid_name(Pointer ptr, long len)
    BoolResult proven_header_is_dangerous(Pointer ptr, long len)
    StringResult proven_header_render(Pointer namePtr, long nameLen, Pointer valuePtr, long valueLen)
    StringResult proven_header_build_hsts(long maxAge, boolean includeSubdomains, boolean preload)

    // ---------- SafeCookie ----------

    BoolResult proven_cookie_has_injection(Pointer ptr, long len)
    BoolResult proven_cookie_validate_name(Pointer ptr, long len)
    BoolResult proven_cookie_validate_value(Pointer ptr, long len)
    IntResult proven_cookie_get_prefix(Pointer ptr, long len)

    // ---------- SafeChecksum ----------

    IntResult proven_checksum_crc32(Pointer ptr, long len)
    BoolResult proven_checksum_verify_crc32(Pointer ptr, long len, int expected)

    // ---------- SafeProbability ----------

    double proven_probability_create(double value)
    double proven_probability_and(double a, double b)
    double proven_probability_or_exclusive(double a, double b)
    double proven_probability_not(double p)

    // ---------- SafeDateTime ----------

    boolean proven_datetime_is_leap_year(int year)
    byte proven_datetime_days_in_month(int year, byte month)

    // ---------- SafeML ----------

    double proven_ml_sigmoid(double x)
    double proven_ml_relu(double x)
    double proven_ml_leaky_relu(double x, double alpha)
    double proven_ml_clamp(double x, double minVal, double maxVal)

    // ---------- SafeAngle ----------

    double proven_angle_deg_to_rad(double degrees)
    double proven_angle_rad_to_deg(double radians)
    double proven_angle_normalize_degrees(double degrees)
    double proven_angle_normalize_radians(double radians)

    // ---------- SafeHex ----------

    StringResult proven_hex_encode(Pointer ptr, long len, boolean uppercase)

    // ---------- SafeUrl (HTTP) ----------

    StringResult proven_http_url_encode(Pointer ptr, long len)
    StringResult proven_http_url_decode(Pointer ptr, long len)

    // ---------- SafeCalculator ----------

    FloatResult proven_calculator_eval(Pointer ptr, long len)

    // ---------- Callback ----------

    int proven_callback_count(int eventType)
    int proven_callback_clear_all()
}
