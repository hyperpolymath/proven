// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Scala JNA bindings for libproven. All computation is performed by the
// native Idris 2 + Zig library; this file contains ONLY data marshaling.

package io.github.hyperpolymath.proven

import com.sun.jna.{Library, Native, Pointer, Structure}
import java.util

// ---------------------------------------------------------------------------
// JNA structure mappings for libproven C ABI result types
// ---------------------------------------------------------------------------

/** Maps to C `ProvenIntResult { int32_t status; int64_t value; }`. */
@Structure.FieldOrder(Array("status", "value"))
class ProvenIntResult extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var status: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var value: Long = 0L

object ProvenIntResult:
  class ByValue extends ProvenIntResult with Structure.ByValue

/** Maps to C `ProvenBoolResult { int32_t status; byte value; }`. */
@Structure.FieldOrder(Array("status", "value"))
class ProvenBoolResult extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var status: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var value: Byte = 0

  def boolValue: Boolean = value.toInt != 0

object ProvenBoolResult:
  class ByValue extends ProvenBoolResult with Structure.ByValue

/** Maps to C `ProvenStringResult { int32_t status; Pointer value; size_t length; }`. */
@Structure.FieldOrder(Array("status", "value", "length"))
class ProvenStringResult extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var status: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var value: Pointer = _
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var length: Long = 0L

object ProvenStringResult:
  class ByValue extends ProvenStringResult with Structure.ByValue

/** Maps to C `ProvenFloatResult { int32_t status; double value; }`. */
@Structure.FieldOrder(Array("status", "value"))
class ProvenFloatResult extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var status: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var value: Double = 0.0

object ProvenFloatResult:
  class ByValue extends ProvenFloatResult with Structure.ByValue

/** Maps to C `ProvenIPv4Address { uint8_t octets[4]; }`. */
@Structure.FieldOrder(Array("octets"))
class ProvenIPv4Address extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var octets: Array[Byte] = new Array[Byte](4)

object ProvenIPv4Address:
  class ByValue extends ProvenIPv4Address with Structure.ByValue

/** Maps to C `ProvenIPv4Result { int32_t status; ProvenIPv4Address address; }`. */
@Structure.FieldOrder(Array("status", "address"))
class ProvenIPv4Result extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var status: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var address: ProvenIPv4Address = new ProvenIPv4Address()

object ProvenIPv4Result:
  class ByValue extends ProvenIPv4Result with Structure.ByValue

/** Maps to C `ProvenUrlComponents`. */
@Structure.FieldOrder(Array(
  "scheme", "scheme_len", "host", "host_len",
  "port", "has_port", "path", "path_len",
  "query", "query_len", "fragment", "fragment_len"
))
class ProvenUrlComponents extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var scheme: Pointer = _
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var scheme_len: Long = 0L
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var host: Pointer = _
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var host_len: Long = 0L
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var port: Short = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var has_port: Byte = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var path: Pointer = _
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var path_len: Long = 0L
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var query: Pointer = _
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var query_len: Long = 0L
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var fragment: Pointer = _
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var fragment_len: Long = 0L

object ProvenUrlComponents:
  class ByReference extends ProvenUrlComponents with Structure.ByReference

/** Maps to C `ProvenUrlResult`. */
@Structure.FieldOrder(Array("status", "components"))
class ProvenUrlResult extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var status: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var components: ProvenUrlComponents = new ProvenUrlComponents()

object ProvenUrlResult:
  class ByValue extends ProvenUrlResult with Structure.ByValue

/** Maps to C `ProvenDateTime`. */
@Structure.FieldOrder(Array(
  "year", "month", "day", "hour", "minute", "second", "nanosecond", "tz_offset_minutes"
))
class ProvenDateTime extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var year: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var month: Byte = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var day: Byte = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var hour: Byte = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var minute: Byte = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var second: Byte = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var nanosecond: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var tz_offset_minutes: Short = 0

object ProvenDateTime:
  class ByValue extends ProvenDateTime with Structure.ByValue

/** Maps to C `ProvenDateTimeResult`. */
@Structure.FieldOrder(Array("status", "datetime"))
class ProvenDateTimeResult extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var status: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var datetime: ProvenDateTime = new ProvenDateTime()

object ProvenDateTimeResult:
  class ByValue extends ProvenDateTimeResult with Structure.ByValue

/** Maps to C `ProvenRGBColor`. */
@Structure.FieldOrder(Array("r", "g", "b"))
class ProvenRGBColor extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var r: Byte = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var g: Byte = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var b: Byte = 0

object ProvenRGBColor:
  class ByValue extends ProvenRGBColor with Structure.ByValue

/** Maps to C `ProvenHSLColor`. */
@Structure.FieldOrder(Array("h", "s", "l"))
class ProvenHSLColor extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var h: Double = 0.0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var s: Double = 0.0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var l: Double = 0.0

object ProvenHSLColor:
  class ByValue extends ProvenHSLColor with Structure.ByValue

/** Maps to C `ProvenColorResult`. */
@Structure.FieldOrder(Array("status", "color"))
class ProvenColorResult extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var status: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var color: ProvenRGBColor = new ProvenRGBColor()

object ProvenColorResult:
  class ByValue extends ProvenColorResult with Structure.ByValue

/** Maps to C `ProvenSemanticVersion`. */
@Structure.FieldOrder(Array("major", "minor", "patch", "prerelease_len", "prerelease"))
class ProvenSemanticVersion extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var major: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var minor: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var patch: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var prerelease_len: Long = 0L
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var prerelease: Pointer = _

object ProvenSemanticVersion:
  class ByValue extends ProvenSemanticVersion with Structure.ByValue
  class ByReference extends ProvenSemanticVersion with Structure.ByReference

/** Maps to C `ProvenVersionResult`. */
@Structure.FieldOrder(Array("status", "version"))
class ProvenVersionResult extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var status: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var version: ProvenSemanticVersion = new ProvenSemanticVersion()

object ProvenVersionResult:
  class ByValue extends ProvenVersionResult with Structure.ByValue

/** Maps to C `ProvenRetryConfig`. */
@Structure.FieldOrder(Array("max_attempts", "base_delay_ms", "max_delay_ms", "multiplier"))
class ProvenRetryConfig extends Structure:
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var max_attempts: Int = 0
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var base_delay_ms: Long = 0L
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var max_delay_ms: Long = 0L
  @(scala.beans.BeanProperty @scala.annotation.meta.field)
  var multiplier: Double = 0.0

object ProvenRetryConfig:
  class ByValue extends ProvenRetryConfig with Structure.ByValue

// ---------------------------------------------------------------------------
// JNA native library interface -- declares all 103 exported C functions
// ---------------------------------------------------------------------------

/**
 * Raw JNA interface to the libproven shared library.
 *
 * Every function here corresponds 1:1 to a C function exported by
 * ffi/zig/ (which wraps Idris 2 RefC output). No logic is
 * reimplemented on the JVM side.
 */
trait ProvenNative extends Library:

  // ---- Runtime Management (4) ----
  def proven_init(): Int
  def proven_deinit(): Unit
  def proven_is_initialized(): Boolean
  def proven_ffi_abi_version(): Int

  // ---- Memory Management (2) ----
  def proven_free_string(ptr: Pointer): Unit
  def proven_url_free(components: ProvenUrlComponents): Unit

  // ---- SafeMath (8) ----
  def proven_math_div(numerator: Long, denominator: Long): ProvenIntResult.ByValue
  def proven_math_mod(numerator: Long, denominator: Long): ProvenIntResult.ByValue
  def proven_math_add_checked(a: Long, b: Long): ProvenIntResult.ByValue
  def proven_math_sub_checked(a: Long, b: Long): ProvenIntResult.ByValue
  def proven_math_mul_checked(a: Long, b: Long): ProvenIntResult.ByValue
  def proven_math_pow_checked(base: Long, exp: Int): ProvenIntResult.ByValue
  def proven_math_abs_safe(n: Long): ProvenIntResult.ByValue
  def proven_math_clamp(lo: Long, hi: Long, value: Long): Long

  // ---- SafeString (4) ----
  def proven_string_is_valid_utf8(ptr: Array[Byte], len: Long): ProvenBoolResult.ByValue
  def proven_string_escape_sql(ptr: Array[Byte], len: Long): ProvenStringResult.ByValue
  def proven_string_escape_html(ptr: Array[Byte], len: Long): ProvenStringResult.ByValue
  def proven_string_escape_js(ptr: Array[Byte], len: Long): ProvenStringResult.ByValue

  // ---- SafePath (2) ----
  def proven_path_has_traversal(ptr: Array[Byte], len: Long): ProvenBoolResult.ByValue
  def proven_path_sanitize_filename(ptr: Array[Byte], len: Long): ProvenStringResult.ByValue

  // ---- SafeCrypto (2) ----
  def proven_crypto_constant_time_eq(
      ptr1: Array[Byte], len1: Long,
      ptr2: Array[Byte], len2: Long
  ): ProvenBoolResult.ByValue
  def proven_crypto_random_bytes(ptr: Array[Byte], len: Long): Int

  // ---- SafeUrl (1) ----
  def proven_url_parse(ptr: Array[Byte], len: Long): ProvenUrlResult.ByValue

  // ---- SafeEmail (1) ----
  def proven_email_is_valid(ptr: Array[Byte], len: Long): ProvenBoolResult.ByValue

  // ---- SafeNetwork (3) ----
  def proven_network_parse_ipv4(ptr: Array[Byte], len: Long): ProvenIPv4Result.ByValue
  def proven_network_ipv4_is_private(addr: ProvenIPv4Address.ByValue): Boolean
  def proven_network_ipv4_is_loopback(addr: ProvenIPv4Address.ByValue): Boolean

  // ---- SafeJson (2) ----
  def proven_json_is_valid(ptr: Array[Byte], len: Long): ProvenBoolResult.ByValue
  def proven_json_get_type(ptr: Array[Byte], len: Long): Int

  // ---- SafeDatetime (4) ----
  def proven_datetime_parse(ptr: Array[Byte], len: Long): ProvenDateTimeResult.ByValue
  def proven_datetime_format_iso8601(dt: ProvenDateTime.ByValue): ProvenStringResult.ByValue
  def proven_datetime_is_leap_year(year: Int): Boolean
  def proven_datetime_days_in_month(year: Int, month: Byte): Byte

  // ---- SafeFloat (5) ----
  def proven_float_div(a: Double, b: Double): ProvenFloatResult.ByValue
  def proven_float_sqrt(x: Double): ProvenFloatResult.ByValue
  def proven_float_ln(x: Double): ProvenFloatResult.ByValue
  def proven_float_is_finite(x: Double): Boolean
  def proven_float_is_nan(x: Double): Boolean

  // ---- SafeVersion (3) ----
  def proven_version_parse(ptr: Array[Byte], len: Long): ProvenVersionResult.ByValue
  def proven_version_free(version: ProvenSemanticVersion.ByReference): Unit
  def proven_version_compare(a: ProvenSemanticVersion.ByValue, b: ProvenSemanticVersion.ByValue): Int

  // ---- SafeColor (3) ----
  def proven_color_parse_hex(ptr: Array[Byte], len: Long): ProvenColorResult.ByValue
  def proven_color_rgb_to_hsl(rgb: ProvenRGBColor.ByValue): ProvenHSLColor.ByValue
  def proven_color_to_hex(rgb: ProvenRGBColor.ByValue): ProvenStringResult.ByValue

  // ---- SafeAngle (4) ----
  def proven_angle_deg_to_rad(degrees: Double): Double
  def proven_angle_rad_to_deg(radians: Double): Double
  def proven_angle_normalize_degrees(degrees: Double): Double
  def proven_angle_normalize_radians(radians: Double): Double

  // ---- SafeUnit (2) ----
  def proven_unit_convert_length(value: Double, from: Int, to: Int): ProvenFloatResult.ByValue
  def proven_unit_convert_temp(value: Double, from: Int, to: Int): ProvenFloatResult.ByValue

  // ---- SafeBuffer (4) ----
  def proven_buffer_create(capacity: Long): Pointer
  def proven_buffer_append(buffer: Pointer, ptr: Array[Byte], len: Long): Int
  def proven_buffer_get(buffer: Pointer, out_ptr: Pointer, out_len: Pointer): Int
  def proven_buffer_free(buffer: Pointer): Unit

  // ---- SafeQueue (5) ----
  def proven_queue_create(capacity: Long): Pointer
  def proven_queue_push(queue: Pointer, value: Long): Boolean
  def proven_queue_pop(queue: Pointer): ProvenIntResult.ByValue
  def proven_queue_size(queue: Pointer): Long
  def proven_queue_free(queue: Pointer): Unit

  // ---- SafeBloom (4) ----
  def proven_bloom_create(expected_elements: Long, false_positive_rate: Double): Pointer
  def proven_bloom_add(filter: Pointer, ptr: Array[Byte], len: Long): Unit
  def proven_bloom_contains(filter: Pointer, ptr: Array[Byte], len: Long): Boolean
  def proven_bloom_free(filter: Pointer): Unit

  // ---- SafeLRU (4) ----
  def proven_lru_create(capacity: Long): Pointer
  def proven_lru_get(cache: Pointer, key: Long): ProvenIntResult.ByValue
  def proven_lru_put(cache: Pointer, key: Long, value: Long): Int
  def proven_lru_free(cache: Pointer): Unit

  // ---- SafeGraph (4) ----
  def proven_graph_create(node_count: Long): Pointer
  def proven_graph_add_edge(graph: Pointer, from: Long, to: Long): Int
  def proven_graph_has_edge(graph: Pointer, from: Long, to: Long): Boolean
  def proven_graph_free(graph: Pointer): Unit

  // ---- SafeRateLimiter (3) ----
  def proven_rate_limiter_create(capacity: Double, refill_rate: Double): Pointer
  def proven_rate_limiter_try_acquire(limiter: Pointer, tokens: Double): Boolean
  def proven_rate_limiter_free(limiter: Pointer): Unit

  // ---- SafeCircuitBreaker (6) ----
  def proven_circuit_breaker_create(
      failure_threshold: Int, success_threshold: Int, timeout_ms: Long
  ): Pointer
  def proven_circuit_breaker_allow(cb: Pointer): Boolean
  def proven_circuit_breaker_success(cb: Pointer): Unit
  def proven_circuit_breaker_failure(cb: Pointer): Unit
  def proven_circuit_breaker_state(cb: Pointer): Int
  def proven_circuit_breaker_free(cb: Pointer): Unit

  // ---- SafeRetry (2) ----
  def proven_retry_delay(config: ProvenRetryConfig.ByValue, attempt: Int): Long
  def proven_retry_should_retry(config: ProvenRetryConfig.ByValue, attempt: Int): Boolean

  // ---- SafeMonotonic (3) ----
  def proven_monotonic_create(initial: Long, max_value: Long): Pointer
  def proven_monotonic_next(counter: Pointer): ProvenIntResult.ByValue
  def proven_monotonic_free(counter: Pointer): Unit

  // ---- SafeStateMachine (5) ----
  def proven_state_machine_create(state_count: Int, initial_state: Int): Pointer
  def proven_state_machine_allow(sm: Pointer, from: Int, to: Int): Boolean
  def proven_state_machine_transition(sm: Pointer, to: Int): Boolean
  def proven_state_machine_state(sm: Pointer): Int
  def proven_state_machine_free(sm: Pointer): Unit

  // ---- SafeCalculator (1) ----
  def proven_calculator_eval(ptr: Array[Byte], len: Long): ProvenFloatResult.ByValue

  // ---- Version Info (3) ----
  def proven_version_major(): Int
  def proven_version_minor(): Int
  def proven_version_patch(): Int

end ProvenNative

/** Companion object providing the singleton native library instance. */
object ProvenNative:
  /** Singleton instance loaded from libproven.so / libproven.dylib / proven.dll. */
  lazy val instance: ProvenNative =
    Native.load("proven", classOf[ProvenNative])
