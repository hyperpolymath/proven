// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Kotlin JNA bindings for libproven. All computation is performed by the
// native Idris 2 + Zig library; this file contains ONLY data marshaling.

package io.github.hyperpolymath.proven

import com.sun.jna.Library
import com.sun.jna.Native
import com.sun.jna.Pointer
import com.sun.jna.Structure

// ---------------------------------------------------------------------------
// JNA structure mappings for libproven C ABI result types
// ---------------------------------------------------------------------------

/**
 * Maps to C `ProvenIntResult { int32_t status; int64_t value; }`.
 */
@Structure.FieldOrder("status", "value")
open class ProvenIntResult : Structure() {
    @JvmField var status: Int = 0
    @JvmField var value: Long = 0L

    /** By-value variant for return types. */
    class ByValue : ProvenIntResult(), Structure.ByValue
}

/**
 * Maps to C `ProvenBoolResult { int32_t status; byte value; }`.
 */
@Structure.FieldOrder("status", "value")
open class ProvenBoolResult : Structure() {
    @JvmField var status: Int = 0
    @JvmField var value: Byte = 0

    class ByValue : ProvenBoolResult(), Structure.ByValue

    /** Interpret the byte field as a Boolean. */
    fun boolValue(): Boolean = value.toInt() != 0
}

/**
 * Maps to C `ProvenStringResult { int32_t status; Pointer value; size_t length; }`.
 */
@Structure.FieldOrder("status", "value", "length")
open class ProvenStringResult : Structure() {
    @JvmField var status: Int = 0
    @JvmField var value: Pointer? = null
    @JvmField var length: Long = 0L

    class ByValue : ProvenStringResult(), Structure.ByValue
}

/**
 * Maps to C `ProvenFloatResult { int32_t status; double value; }`.
 */
@Structure.FieldOrder("status", "value")
open class ProvenFloatResult : Structure() {
    @JvmField var status: Int = 0
    @JvmField var value: Double = 0.0

    class ByValue : ProvenFloatResult(), Structure.ByValue
}

/**
 * Maps to C `ProvenIPv4Address { uint8_t octets[4]; }`.
 */
@Structure.FieldOrder("octets")
open class ProvenIPv4Address : Structure() {
    @JvmField var octets: ByteArray = ByteArray(4)

    class ByValue : ProvenIPv4Address(), Structure.ByValue
}

/**
 * Maps to C `ProvenIPv4Result { int32_t status; ProvenIPv4Address address; }`.
 */
@Structure.FieldOrder("status", "address")
open class ProvenIPv4Result : Structure() {
    @JvmField var status: Int = 0
    @JvmField var address: ProvenIPv4Address = ProvenIPv4Address()

    class ByValue : ProvenIPv4Result(), Structure.ByValue
}

/**
 * Maps to C `ProvenUrlComponents`.
 */
@Structure.FieldOrder(
    "scheme", "scheme_len",
    "host", "host_len",
    "port", "has_port",
    "path", "path_len",
    "query", "query_len",
    "fragment", "fragment_len"
)
open class ProvenUrlComponents : Structure() {
    @JvmField var scheme: Pointer? = null
    @JvmField var scheme_len: Long = 0L
    @JvmField var host: Pointer? = null
    @JvmField var host_len: Long = 0L
    @JvmField var port: Short = 0
    @JvmField var has_port: Byte = 0
    @JvmField var path: Pointer? = null
    @JvmField var path_len: Long = 0L
    @JvmField var query: Pointer? = null
    @JvmField var query_len: Long = 0L
    @JvmField var fragment: Pointer? = null
    @JvmField var fragment_len: Long = 0L

    class ByReference : ProvenUrlComponents(), Structure.ByReference
}

/**
 * Maps to C `ProvenUrlResult { int32_t status; ProvenUrlComponents components; }`.
 */
@Structure.FieldOrder("status", "components")
open class ProvenUrlResult : Structure() {
    @JvmField var status: Int = 0
    @JvmField var components: ProvenUrlComponents = ProvenUrlComponents()

    class ByValue : ProvenUrlResult(), Structure.ByValue
}

/**
 * Maps to C `ProvenDateTime`.
 */
@Structure.FieldOrder(
    "year", "month", "day", "hour", "minute", "second", "nanosecond", "tz_offset_minutes"
)
open class ProvenDateTime : Structure() {
    @JvmField var year: Int = 0
    @JvmField var month: Byte = 0
    @JvmField var day: Byte = 0
    @JvmField var hour: Byte = 0
    @JvmField var minute: Byte = 0
    @JvmField var second: Byte = 0
    @JvmField var nanosecond: Int = 0
    @JvmField var tz_offset_minutes: Short = 0

    class ByValue : ProvenDateTime(), Structure.ByValue
}

/**
 * Maps to C `ProvenDateTimeResult`.
 */
@Structure.FieldOrder("status", "datetime")
open class ProvenDateTimeResult : Structure() {
    @JvmField var status: Int = 0
    @JvmField var datetime: ProvenDateTime = ProvenDateTime()

    class ByValue : ProvenDateTimeResult(), Structure.ByValue
}

/**
 * Maps to C `ProvenRGBColor`.
 */
@Structure.FieldOrder("r", "g", "b")
open class ProvenRGBColor : Structure() {
    @JvmField var r: Byte = 0
    @JvmField var g: Byte = 0
    @JvmField var b: Byte = 0

    class ByValue : ProvenRGBColor(), Structure.ByValue
}

/**
 * Maps to C `ProvenHSLColor`.
 */
@Structure.FieldOrder("h", "s", "l")
open class ProvenHSLColor : Structure() {
    @JvmField var h: Double = 0.0
    @JvmField var s: Double = 0.0
    @JvmField var l: Double = 0.0

    class ByValue : ProvenHSLColor(), Structure.ByValue
}

/**
 * Maps to C `ProvenColorResult`.
 */
@Structure.FieldOrder("status", "color")
open class ProvenColorResult : Structure() {
    @JvmField var status: Int = 0
    @JvmField var color: ProvenRGBColor = ProvenRGBColor()

    class ByValue : ProvenColorResult(), Structure.ByValue
}

/**
 * Maps to C `ProvenSemanticVersion`.
 */
@Structure.FieldOrder("major", "minor", "patch", "prerelease_len", "prerelease")
open class ProvenSemanticVersion : Structure() {
    @JvmField var major: Int = 0
    @JvmField var minor: Int = 0
    @JvmField var patch: Int = 0
    @JvmField var prerelease_len: Long = 0L
    @JvmField var prerelease: Pointer? = null

    class ByValue : ProvenSemanticVersion(), Structure.ByValue
    class ByReference : ProvenSemanticVersion(), Structure.ByReference
}

/**
 * Maps to C `ProvenVersionResult`.
 */
@Structure.FieldOrder("status", "version")
open class ProvenVersionResult : Structure() {
    @JvmField var status: Int = 0
    @JvmField var version: ProvenSemanticVersion = ProvenSemanticVersion()

    class ByValue : ProvenVersionResult(), Structure.ByValue
}

/**
 * Maps to C `ProvenRetryConfig`.
 */
@Structure.FieldOrder("max_attempts", "base_delay_ms", "max_delay_ms", "multiplier")
open class ProvenRetryConfig : Structure() {
    @JvmField var max_attempts: Int = 0
    @JvmField var base_delay_ms: Long = 0L
    @JvmField var max_delay_ms: Long = 0L
    @JvmField var multiplier: Double = 0.0

    class ByValue : ProvenRetryConfig(), Structure.ByValue
}

// ---------------------------------------------------------------------------
// JNA native library interface -- declares all 103 exported C functions
// ---------------------------------------------------------------------------

/**
 * Raw JNA interface to the libproven shared library.
 *
 * Every function here corresponds 1:1 to a C function exported by
 * `ffi/zig/` (which wraps Idris 2 RefC output). No logic is
 * reimplemented on the JVM side.
 */
interface ProvenNative : Library {

    companion object {
        /** Singleton instance loaded from `libproven.so` / `libproven.dylib` / `proven.dll`. */
        val INSTANCE: ProvenNative = Native.load("proven", ProvenNative::class.java)
    }

    // ---- Runtime Management (4) ----
    fun proven_init(): Int
    fun proven_deinit()
    fun proven_is_initialized(): Boolean
    fun proven_ffi_abi_version(): Int

    // ---- Memory Management (2) ----
    fun proven_free_string(ptr: Pointer?)
    fun proven_url_free(components: ProvenUrlComponents?)

    // ---- SafeMath (8) ----
    fun proven_math_div(numerator: Long, denominator: Long): ProvenIntResult.ByValue
    fun proven_math_mod(numerator: Long, denominator: Long): ProvenIntResult.ByValue
    fun proven_math_add_checked(a: Long, b: Long): ProvenIntResult.ByValue
    fun proven_math_sub_checked(a: Long, b: Long): ProvenIntResult.ByValue
    fun proven_math_mul_checked(a: Long, b: Long): ProvenIntResult.ByValue
    fun proven_math_pow_checked(base: Long, exp: Int): ProvenIntResult.ByValue
    fun proven_math_abs_safe(n: Long): ProvenIntResult.ByValue
    fun proven_math_clamp(lo: Long, hi: Long, value: Long): Long

    // ---- SafeString (4) ----
    fun proven_string_is_valid_utf8(ptr: ByteArray, len: Long): ProvenBoolResult.ByValue
    fun proven_string_escape_sql(ptr: ByteArray, len: Long): ProvenStringResult.ByValue
    fun proven_string_escape_html(ptr: ByteArray, len: Long): ProvenStringResult.ByValue
    fun proven_string_escape_js(ptr: ByteArray, len: Long): ProvenStringResult.ByValue

    // ---- SafePath (2) ----
    fun proven_path_has_traversal(ptr: ByteArray, len: Long): ProvenBoolResult.ByValue
    fun proven_path_sanitize_filename(ptr: ByteArray, len: Long): ProvenStringResult.ByValue

    // ---- SafeCrypto (2) ----
    fun proven_crypto_constant_time_eq(
        ptr1: ByteArray, len1: Long,
        ptr2: ByteArray, len2: Long
    ): ProvenBoolResult.ByValue
    fun proven_crypto_random_bytes(ptr: ByteArray, len: Long): Int

    // ---- SafeUrl (1 + free above) ----
    fun proven_url_parse(ptr: ByteArray, len: Long): ProvenUrlResult.ByValue

    // ---- SafeEmail (1) ----
    fun proven_email_is_valid(ptr: ByteArray, len: Long): ProvenBoolResult.ByValue

    // ---- SafeNetwork (3) ----
    fun proven_network_parse_ipv4(ptr: ByteArray, len: Long): ProvenIPv4Result.ByValue
    fun proven_network_ipv4_is_private(addr: ProvenIPv4Address.ByValue): Boolean
    fun proven_network_ipv4_is_loopback(addr: ProvenIPv4Address.ByValue): Boolean

    // ---- SafeJson (2) ----
    fun proven_json_is_valid(ptr: ByteArray, len: Long): ProvenBoolResult.ByValue
    fun proven_json_get_type(ptr: ByteArray, len: Long): Int

    // ---- SafeDatetime (4) ----
    fun proven_datetime_parse(ptr: ByteArray, len: Long): ProvenDateTimeResult.ByValue
    fun proven_datetime_format_iso8601(dt: ProvenDateTime.ByValue): ProvenStringResult.ByValue
    fun proven_datetime_is_leap_year(year: Int): Boolean
    fun proven_datetime_days_in_month(year: Int, month: Byte): Byte

    // ---- SafeFloat (5) ----
    fun proven_float_div(a: Double, b: Double): ProvenFloatResult.ByValue
    fun proven_float_sqrt(x: Double): ProvenFloatResult.ByValue
    fun proven_float_ln(x: Double): ProvenFloatResult.ByValue
    fun proven_float_is_finite(x: Double): Boolean
    fun proven_float_is_nan(x: Double): Boolean

    // ---- SafeVersion (3) ----
    fun proven_version_parse(ptr: ByteArray, len: Long): ProvenVersionResult.ByValue
    fun proven_version_free(version: ProvenSemanticVersion.ByReference?)
    fun proven_version_compare(a: ProvenSemanticVersion.ByValue, b: ProvenSemanticVersion.ByValue): Int

    // ---- SafeColor (3) ----
    fun proven_color_parse_hex(ptr: ByteArray, len: Long): ProvenColorResult.ByValue
    fun proven_color_rgb_to_hsl(rgb: ProvenRGBColor.ByValue): ProvenHSLColor.ByValue
    fun proven_color_to_hex(rgb: ProvenRGBColor.ByValue): ProvenStringResult.ByValue

    // ---- SafeAngle (4) ----
    fun proven_angle_deg_to_rad(degrees: Double): Double
    fun proven_angle_rad_to_deg(radians: Double): Double
    fun proven_angle_normalize_degrees(degrees: Double): Double
    fun proven_angle_normalize_radians(radians: Double): Double

    // ---- SafeUnit (2) ----
    fun proven_unit_convert_length(value: Double, from: Int, to: Int): ProvenFloatResult.ByValue
    fun proven_unit_convert_temp(value: Double, from: Int, to: Int): ProvenFloatResult.ByValue

    // ---- SafeBuffer (4) ----
    fun proven_buffer_create(capacity: Long): Pointer?
    fun proven_buffer_append(buffer: Pointer?, ptr: ByteArray, len: Long): Int
    fun proven_buffer_get(buffer: Pointer?, out_ptr: Pointer, out_len: Pointer): Int
    fun proven_buffer_free(buffer: Pointer?)

    // ---- SafeQueue (5) ----
    fun proven_queue_create(capacity: Long): Pointer?
    fun proven_queue_push(queue: Pointer?, value: Long): Boolean
    fun proven_queue_pop(queue: Pointer?): ProvenIntResult.ByValue
    fun proven_queue_size(queue: Pointer?): Long
    fun proven_queue_free(queue: Pointer?)

    // ---- SafeBloom (4) ----
    fun proven_bloom_create(expected_elements: Long, false_positive_rate: Double): Pointer?
    fun proven_bloom_add(filter: Pointer?, ptr: ByteArray, len: Long)
    fun proven_bloom_contains(filter: Pointer?, ptr: ByteArray, len: Long): Boolean
    fun proven_bloom_free(filter: Pointer?)

    // ---- SafeLRU (4) ----
    fun proven_lru_create(capacity: Long): Pointer?
    fun proven_lru_get(cache: Pointer?, key: Long): ProvenIntResult.ByValue
    fun proven_lru_put(cache: Pointer?, key: Long, value: Long): Int
    fun proven_lru_free(cache: Pointer?)

    // ---- SafeGraph (4) ----
    fun proven_graph_create(node_count: Long): Pointer?
    fun proven_graph_add_edge(graph: Pointer?, from: Long, to: Long): Int
    fun proven_graph_has_edge(graph: Pointer?, from: Long, to: Long): Boolean
    fun proven_graph_free(graph: Pointer?)

    // ---- SafeRateLimiter (3) ----
    fun proven_rate_limiter_create(capacity: Double, refill_rate: Double): Pointer?
    fun proven_rate_limiter_try_acquire(limiter: Pointer?, tokens: Double): Boolean
    fun proven_rate_limiter_free(limiter: Pointer?)

    // ---- SafeCircuitBreaker (6) ----
    fun proven_circuit_breaker_create(
        failure_threshold: Int, success_threshold: Int, timeout_ms: Long
    ): Pointer?
    fun proven_circuit_breaker_allow(cb: Pointer?): Boolean
    fun proven_circuit_breaker_success(cb: Pointer?)
    fun proven_circuit_breaker_failure(cb: Pointer?)
    fun proven_circuit_breaker_state(cb: Pointer?): Int
    fun proven_circuit_breaker_free(cb: Pointer?)

    // ---- SafeRetry (2) ----
    fun proven_retry_delay(config: ProvenRetryConfig.ByValue, attempt: Int): Long
    fun proven_retry_should_retry(config: ProvenRetryConfig.ByValue, attempt: Int): Boolean

    // ---- SafeMonotonic (3) ----
    fun proven_monotonic_create(initial: Long, max_value: Long): Pointer?
    fun proven_monotonic_next(counter: Pointer?): ProvenIntResult.ByValue
    fun proven_monotonic_free(counter: Pointer?)

    // ---- SafeStateMachine (5) ----
    fun proven_state_machine_create(state_count: Int, initial_state: Int): Pointer?
    fun proven_state_machine_allow(sm: Pointer?, from: Int, to: Int): Boolean
    fun proven_state_machine_transition(sm: Pointer?, to: Int): Boolean
    fun proven_state_machine_state(sm: Pointer?): Int
    fun proven_state_machine_free(sm: Pointer?)

    // ---- SafeCalculator (1) ----
    fun proven_calculator_eval(ptr: ByteArray, len: Long): ProvenFloatResult.ByValue

    // ---- Version Info (3) ----
    fun proven_version_major(): Int
    fun proven_version_minor(): Int
    fun proven_version_patch(): Int
}
