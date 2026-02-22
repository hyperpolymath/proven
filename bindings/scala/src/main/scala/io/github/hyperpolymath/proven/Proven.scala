// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Idiomatic Scala wrappers over the ProvenNative JNA interface.
// All computation is delegated to libproven via FFI -- no logic reimplemented.

package io.github.hyperpolymath.proven

import com.sun.jna.Pointer
import java.nio.charset.StandardCharsets

// ---------------------------------------------------------------------------
// Status code constants matching the C enum ProvenStatus
// ---------------------------------------------------------------------------

/** Status codes returned by libproven operations. */
object ProvenStatus:
  val OK: Int = 0
  val ERR_NULL_POINTER: Int = -1
  val ERR_INVALID_ARGUMENT: Int = -2
  val ERR_OVERFLOW: Int = -3
  val ERR_UNDERFLOW: Int = -4
  val ERR_DIVISION_BY_ZERO: Int = -5
  val ERR_PARSE_FAILURE: Int = -6
  val ERR_VALIDATION_FAILED: Int = -7
  val ERR_OUT_OF_BOUNDS: Int = -8
  val ERR_ENCODING_ERROR: Int = -9
  val ERR_ALLOCATION_FAILED: Int = -10
  val ERR_NOT_IMPLEMENTED: Int = -99

/** JSON type constants matching C enum ProvenJsonType. */
object ProvenJsonType:
  val NULL: Int = 0
  val BOOL: Int = 1
  val NUMBER: Int = 2
  val STRING: Int = 3
  val ARRAY: Int = 4
  val OBJECT: Int = 5
  val INVALID: Int = -1

/** Length unit constants matching C enum ProvenLengthUnit. */
object LengthUnit:
  val METERS: Int = 0
  val KILOMETERS: Int = 1
  val CENTIMETERS: Int = 2
  val MILLIMETERS: Int = 3
  val FEET: Int = 4
  val INCHES: Int = 5
  val MILES: Int = 6
  val YARDS: Int = 7

/** Temperature unit constants matching C enum ProvenTempUnit. */
object TempUnit:
  val CELSIUS: Int = 0
  val FAHRENHEIT: Int = 1
  val KELVIN: Int = 2

/** Circuit breaker state constants matching C enum ProvenCircuitState. */
object CircuitState:
  val CLOSED: Int = 0
  val OPEN: Int = 1
  val HALF_OPEN: Int = 2

// ---------------------------------------------------------------------------
// Internal helper: extract a String from ProvenStringResult and free native memory
// ---------------------------------------------------------------------------

private val lib: ProvenNative = ProvenNative.instance

private def extractAndFree(r: ProvenStringResult): Option[String] =
  if r.status != ProvenStatus.OK || r.value == null then None
  else
    val s = r.value.getString(0, "UTF-8")
    lib.proven_free_string(r.value)
    Some(s)

private def toBytes(s: String): Array[Byte] =
  s.getBytes(StandardCharsets.UTF_8)

// ---------------------------------------------------------------------------
// Proven runtime management
// ---------------------------------------------------------------------------

/** Entry-point object for initialising and querying the Proven runtime. */
object Proven:
  /** Initialise the Proven runtime. Returns true on success. */
  def init(): Boolean = lib.proven_init() == ProvenStatus.OK

  /** Deinitialise the Proven runtime. */
  def deinit(): Unit = lib.proven_deinit()

  /** Check whether the runtime is initialised. */
  def isInitialized: Boolean = lib.proven_is_initialized()

  /** Get the FFI ABI version number for compatibility checking. */
  def ffiAbiVersion: Int = lib.proven_ffi_abi_version()

  /** Get the library major version. */
  def versionMajor: Int = lib.proven_version_major()

  /** Get the library minor version. */
  def versionMinor: Int = lib.proven_version_minor()

  /** Get the library patch version. */
  def versionPatch: Int = lib.proven_version_patch()

  /** Get a "major.minor.patch" version string. */
  def versionString: String = s"$versionMajor.$versionMinor.$versionPatch"

// ---------------------------------------------------------------------------
// SafeMath -- arithmetic that cannot crash
// ---------------------------------------------------------------------------

/**
 * Safe arithmetic operations delegated to libproven.
 * Returns Option -- None on error (overflow, division by zero, etc.).
 */
object SafeMath:
  /** Safe integer division. Returns None on division by zero or MIN_INT/-1 overflow. */
  def div(numerator: Long, denominator: Long): Option[Long] =
    val r = lib.proven_math_div(numerator, denominator)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Safe modulo. Returns None on division by zero. */
  def mod(numerator: Long, denominator: Long): Option[Long] =
    val r = lib.proven_math_mod(numerator, denominator)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Checked addition. Returns None on overflow. */
  def add(a: Long, b: Long): Option[Long] =
    val r = lib.proven_math_add_checked(a, b)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Checked subtraction. Returns None on underflow. */
  def sub(a: Long, b: Long): Option[Long] =
    val r = lib.proven_math_sub_checked(a, b)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Checked multiplication. Returns None on overflow. */
  def mul(a: Long, b: Long): Option[Long] =
    val r = lib.proven_math_mul_checked(a, b)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Checked exponentiation. Returns None on overflow. */
  def pow(base: Long, exp: Int): Option[Long] =
    val r = lib.proven_math_pow_checked(base, exp)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Safe absolute value. Returns None for Long.MinValue. */
  def abs(n: Long): Option[Long] =
    val r = lib.proven_math_abs_safe(n)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Clamp value to [lo, hi]. */
  def clamp(lo: Long, hi: Long, value: Long): Long =
    lib.proven_math_clamp(lo, hi, value)

// ---------------------------------------------------------------------------
// SafeString -- text operations handling encoding safely
// ---------------------------------------------------------------------------

/** String escaping and validation delegated to libproven. */
object SafeString:
  /** Check if bytes are valid UTF-8. */
  def isValidUtf8(data: Array[Byte]): Boolean =
    val r = lib.proven_string_is_valid_utf8(data, data.length.toLong)
    r.status == ProvenStatus.OK && r.boolValue

  /** Escape a string for SQL (single-quote escaping). Prefer parameterised queries. */
  def escapeSql(value: String): Option[String] =
    val bytes = toBytes(value)
    extractAndFree(lib.proven_string_escape_sql(bytes, bytes.length.toLong))

  /** Escape a string for HTML (prevents XSS). */
  def escapeHtml(value: String): Option[String] =
    val bytes = toBytes(value)
    extractAndFree(lib.proven_string_escape_html(bytes, bytes.length.toLong))

  /** Escape a string for JavaScript string literals. */
  def escapeJs(value: String): Option[String] =
    val bytes = toBytes(value)
    extractAndFree(lib.proven_string_escape_js(bytes, bytes.length.toLong))

// ---------------------------------------------------------------------------
// SafePath -- filesystem operations preventing traversal attacks
// ---------------------------------------------------------------------------

/** Path validation and sanitisation delegated to libproven. */
object SafePath:
  /** Check if path contains directory traversal sequences (..). */
  def hasTraversal(path: String): Boolean =
    val bytes = toBytes(path)
    val r = lib.proven_path_has_traversal(bytes, bytes.length.toLong)
    r.status == ProvenStatus.OK && r.boolValue

  /** Sanitise a filename by removing dangerous characters. */
  def sanitizeFilename(filename: String): Option[String] =
    val bytes = toBytes(filename)
    extractAndFree(lib.proven_path_sanitize_filename(bytes, bytes.length.toLong))

// ---------------------------------------------------------------------------
// SafeCrypto -- cryptographic primitives
// ---------------------------------------------------------------------------

/** Cryptographic operations delegated to libproven. */
object SafeCrypto:
  /** Constant-time byte comparison (timing-safe). */
  def constantTimeEquals(a: Array[Byte], b: Array[Byte]): Boolean =
    val r = lib.proven_crypto_constant_time_eq(a, a.length.toLong, b, b.length.toLong)
    r.status == ProvenStatus.OK && r.boolValue

  /** Fill buffer with cryptographically secure random bytes. Returns true on success. */
  def randomBytes(buffer: Array[Byte]): Boolean =
    lib.proven_crypto_random_bytes(buffer, buffer.length.toLong) == ProvenStatus.OK

  /** Generate size cryptographically secure random bytes. Returns None on failure. */
  def generateRandomBytes(size: Int): Option[Array[Byte]] =
    val buf = new Array[Byte](size)
    if randomBytes(buf) then Some(buf) else None

// ---------------------------------------------------------------------------
// SafeEmail -- email validation
// ---------------------------------------------------------------------------

/** Email validation delegated to libproven. */
object SafeEmail:
  /** Validate email address (RFC 5321 simplified). */
  def isValid(email: String): Boolean =
    val bytes = toBytes(email)
    val r = lib.proven_email_is_valid(bytes, bytes.length.toLong)
    r.status == ProvenStatus.OK && r.boolValue

// ---------------------------------------------------------------------------
// SafeUrl -- URL parsing
// ---------------------------------------------------------------------------

/** Parsed URL components. */
case class ParsedUrl(
    scheme: String,
    host: String,
    port: Option[Int],
    path: String,
    query: Option[String],
    fragment: Option[String]
)

/** URL parsing delegated to libproven. */
object SafeUrl:
  /** Parse a URL into its components. Returns None on parse failure. */
  def parse(url: String): Option[ParsedUrl] =
    val bytes = toBytes(url)
    val r = lib.proven_url_parse(bytes, bytes.length.toLong)
    if r.status != ProvenStatus.OK then return None

    val c = r.components
    val scheme = Option(c.scheme).map(_.getString(0, "UTF-8")).getOrElse(return None)
    val host = Option(c.host).map(_.getString(0, "UTF-8")).getOrElse(return None)
    val path = Option(c.path).map(_.getString(0, "UTF-8")).getOrElse("/")
    val query = Option(c.query).map(_.getString(0, "UTF-8"))
    val fragment = Option(c.fragment).map(_.getString(0, "UTF-8"))
    val port = if c.has_port.toInt != 0 then Some(c.port.toInt & 0xFFFF) else None

    lib.proven_url_free(c)
    Some(ParsedUrl(scheme, host, port, path, query, fragment))

// ---------------------------------------------------------------------------
// SafeNetwork -- IP address parsing and classification
// ---------------------------------------------------------------------------

/** Parsed IPv4 address. */
case class IPv4Address(a: Int, b: Int, c: Int, d: Int):
  override def toString: String = s"$a.$b.$c.$d"

/** Network operations delegated to libproven. */
object SafeNetwork:
  /** Parse an IPv4 address string. Returns None on failure. */
  def parseIPv4(address: String): Option[IPv4Address] =
    val bytes = toBytes(address)
    val r = lib.proven_network_parse_ipv4(bytes, bytes.length.toLong)
    if r.status != ProvenStatus.OK then None
    else
      val o = r.address.octets
      Some(IPv4Address(
        o(0).toInt & 0xFF, o(1).toInt & 0xFF,
        o(2).toInt & 0xFF, o(3).toInt & 0xFF
      ))

  /** Check if an IPv4 address is private (RFC 1918). */
  def isPrivate(address: IPv4Address): Boolean =
    val addr = new ProvenIPv4Address.ByValue()
    addr.octets = Array(address.a.toByte, address.b.toByte, address.c.toByte, address.d.toByte)
    lib.proven_network_ipv4_is_private(addr)

  /** Check if an IPv4 address is loopback (127.0.0.0/8). */
  def isLoopback(address: IPv4Address): Boolean =
    val addr = new ProvenIPv4Address.ByValue()
    addr.octets = Array(address.a.toByte, address.b.toByte, address.c.toByte, address.d.toByte)
    lib.proven_network_ipv4_is_loopback(addr)

// ---------------------------------------------------------------------------
// SafeJson -- JSON validation and type detection
// ---------------------------------------------------------------------------

/** JSON validation delegated to libproven. */
object SafeJson:
  /** Check if a string is valid JSON. */
  def isValid(json: String): Boolean =
    val bytes = toBytes(json)
    val r = lib.proven_json_is_valid(bytes, bytes.length.toLong)
    r.status == ProvenStatus.OK && r.boolValue

  /** Get JSON value type at root level. Returns ProvenJsonType constant. */
  def getType(json: String): Int =
    val bytes = toBytes(json)
    lib.proven_json_get_type(bytes, bytes.length.toLong)

// ---------------------------------------------------------------------------
// SafeDatetime -- ISO 8601 date/time handling
// ---------------------------------------------------------------------------

/** Parsed datetime components. */
case class DateTime(
    year: Int, month: Int, day: Int,
    hour: Int, minute: Int, second: Int,
    nanosecond: Int, tzOffsetMinutes: Int
)

/** Datetime operations delegated to libproven. */
object SafeDatetime:
  /** Parse ISO 8601 date/time string. Returns None on failure. */
  def parse(dateString: String): Option[DateTime] =
    val bytes = toBytes(dateString)
    val r = lib.proven_datetime_parse(bytes, bytes.length.toLong)
    if r.status != ProvenStatus.OK then None
    else
      val dt = r.datetime
      Some(DateTime(
        dt.year, dt.month.toInt & 0xFF, dt.day.toInt & 0xFF,
        dt.hour.toInt & 0xFF, dt.minute.toInt & 0xFF, dt.second.toInt & 0xFF,
        dt.nanosecond, dt.tz_offset_minutes.toInt
      ))

  /** Format a DateTime as ISO 8601 string. Returns None on failure. */
  def formatIso8601(dt: DateTime): Option[String] =
    val nativeDt = new ProvenDateTime.ByValue()
    nativeDt.year = dt.year
    nativeDt.month = dt.month.toByte
    nativeDt.day = dt.day.toByte
    nativeDt.hour = dt.hour.toByte
    nativeDt.minute = dt.minute.toByte
    nativeDt.second = dt.second.toByte
    nativeDt.nanosecond = dt.nanosecond
    nativeDt.tz_offset_minutes = dt.tzOffsetMinutes.toShort
    extractAndFree(lib.proven_datetime_format_iso8601(nativeDt))

  /** Check if a year is a leap year. */
  def isLeapYear(year: Int): Boolean = lib.proven_datetime_is_leap_year(year)

  /** Get the number of days in a month (0 if invalid). */
  def daysInMonth(year: Int, month: Int): Int =
    lib.proven_datetime_days_in_month(year, month.toByte).toInt & 0xFF

// ---------------------------------------------------------------------------
// SafeFloat -- safe floating-point operations
// ---------------------------------------------------------------------------

/** Safe floating-point operations delegated to libproven. */
object SafeFloat:
  /** Safe floating-point division. Returns None if b is 0, NaN, or result would be infinite. */
  def div(a: Double, b: Double): Option[Double] =
    val r = lib.proven_float_div(a, b)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Safe square root. Returns None for negative or NaN inputs. */
  def sqrt(x: Double): Option[Double] =
    val r = lib.proven_float_sqrt(x)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Safe natural logarithm. Returns None for non-positive or NaN inputs. */
  def ln(x: Double): Option[Double] =
    val r = lib.proven_float_ln(x)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Check if a double is finite (not NaN or Inf). */
  def isFinite(x: Double): Boolean = lib.proven_float_is_finite(x)

  /** Check if a double is NaN. */
  def isNaN(x: Double): Boolean = lib.proven_float_is_nan(x)

// ---------------------------------------------------------------------------
// SafeVersion -- semantic versioning
// ---------------------------------------------------------------------------

/** Parsed semantic version. */
case class SemanticVersion(major: Int, minor: Int, patch: Int, prerelease: Option[String])

/** Semantic version operations delegated to libproven. */
object SafeVersion:
  /** Parse a semantic version string. Returns None on failure. */
  def parse(versionString: String): Option[SemanticVersion] =
    val bytes = toBytes(versionString)
    val r = lib.proven_version_parse(bytes, bytes.length.toLong)
    if r.status != ProvenStatus.OK then None
    else
      val v = r.version
      val pre = if v.prerelease_len > 0 && v.prerelease != null then
        Some(v.prerelease.getString(0, "UTF-8"))
      else None
      Some(SemanticVersion(v.major, v.minor, v.patch, pre))

  /** Compare two semantic versions. Returns negative, 0, or positive. */
  def compare(a: SemanticVersion, b: SemanticVersion): Int =
    val na = new ProvenSemanticVersion.ByValue()
    na.major = a.major; na.minor = a.minor; na.patch = a.patch
    val nb = new ProvenSemanticVersion.ByValue()
    nb.major = b.major; nb.minor = b.minor; nb.patch = b.patch
    lib.proven_version_compare(na, nb)

// ---------------------------------------------------------------------------
// SafeColor -- color parsing and conversion
// ---------------------------------------------------------------------------

/** RGB color with 0-255 components. */
case class RGBColor(r: Int, g: Int, b: Int)

/** HSL color with hue 0-360, saturation 0-1, lightness 0-1. */
case class HSLColor(h: Double, s: Double, l: Double)

/** Color operations delegated to libproven. */
object SafeColor:
  /** Parse a hex color string (e.g., "#FF0000" or "F00"). Returns None on failure. */
  def parseHex(hex: String): Option[RGBColor] =
    val bytes = toBytes(hex)
    val r = lib.proven_color_parse_hex(bytes, bytes.length.toLong)
    if r.status != ProvenStatus.OK then None
    else Some(RGBColor(
      r.color.r.toInt & 0xFF,
      r.color.g.toInt & 0xFF,
      r.color.b.toInt & 0xFF
    ))

  /** Convert RGB to HSL. */
  def rgbToHsl(rgb: RGBColor): HSLColor =
    val native = new ProvenRGBColor.ByValue()
    native.r = rgb.r.toByte
    native.g = rgb.g.toByte
    native.b = rgb.b.toByte
    val hsl = lib.proven_color_rgb_to_hsl(native)
    HSLColor(hsl.h, hsl.s, hsl.l)

  /** Format RGB as hex string (e.g., "#rrggbb"). Returns None on failure. */
  def toHex(rgb: RGBColor): Option[String] =
    val native = new ProvenRGBColor.ByValue()
    native.r = rgb.r.toByte
    native.g = rgb.g.toByte
    native.b = rgb.b.toByte
    extractAndFree(lib.proven_color_to_hex(native))

// ---------------------------------------------------------------------------
// SafeAngle -- angle conversions and normalisation
// ---------------------------------------------------------------------------

/** Angle conversion and normalisation delegated to libproven. */
object SafeAngle:
  /** Convert degrees to radians. */
  def degToRad(degrees: Double): Double = lib.proven_angle_deg_to_rad(degrees)

  /** Convert radians to degrees. */
  def radToDeg(radians: Double): Double = lib.proven_angle_rad_to_deg(radians)

  /** Normalise angle to [0, 360) degrees. */
  def normalizeDegrees(degrees: Double): Double = lib.proven_angle_normalize_degrees(degrees)

  /** Normalise angle to [0, 2*pi) radians. */
  def normalizeRadians(radians: Double): Double = lib.proven_angle_normalize_radians(radians)

// ---------------------------------------------------------------------------
// SafeUnit -- physical unit conversions
// ---------------------------------------------------------------------------

/** Physical unit conversions delegated to libproven. */
object SafeUnit:
  /** Convert length between units. Returns None on invalid input. */
  def convertLength(value: Double, from: Int, to: Int): Option[Double] =
    val r = lib.proven_unit_convert_length(value, from, to)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Convert temperature between units. Returns None on invalid input. */
  def convertTemp(value: Double, from: Int, to: Int): Option[Double] =
    val r = lib.proven_unit_convert_temp(value, from, to)
    if r.status == ProvenStatus.OK then Some(r.value) else None

// ---------------------------------------------------------------------------
// SafeCalculator -- safe expression evaluation
// ---------------------------------------------------------------------------

/** Expression evaluation delegated to libproven. */
object SafeCalculator:
  /** Evaluate an arithmetic expression string. Returns None on parse/division error. */
  def eval(expression: String): Option[Double] =
    val bytes = toBytes(expression)
    val r = lib.proven_calculator_eval(bytes, bytes.length.toLong)
    if r.status == ProvenStatus.OK then Some(r.value) else None

// ---------------------------------------------------------------------------
// SafeBuffer -- bounded buffer operations
// ---------------------------------------------------------------------------

/**
 * Bounded buffer backed by libproven. Manages a native buffer pointer.
 * Call close() when done to free native memory.
 */
class SafeBuffer private (private var ptr: Pointer) extends AutoCloseable:
  /** Append data to the buffer. Returns true on success. */
  def append(data: Array[Byte]): Boolean =
    lib.proven_buffer_append(ptr, data, data.length.toLong) == ProvenStatus.OK

  /** Free native memory. */
  override def close(): Unit =
    if ptr != null then
      lib.proven_buffer_free(ptr)
      ptr = null

object SafeBuffer:
  /** Create a bounded buffer with the given capacity. Returns None on failure. */
  def create(capacity: Long): Option[SafeBuffer] =
    val p = lib.proven_buffer_create(capacity)
    if p == null then None else Some(new SafeBuffer(p))

// ---------------------------------------------------------------------------
// SafeQueue -- bounded FIFO queue
// ---------------------------------------------------------------------------

/**
 * Bounded FIFO queue backed by libproven.
 * Call close() when done to free native memory.
 */
class SafeQueue private (private var ptr: Pointer) extends AutoCloseable:
  /** Push a value. Returns true on success, false if full. */
  def push(value: Long): Boolean = lib.proven_queue_push(ptr, value)

  /** Pop a value. Returns None if empty. */
  def pop(): Option[Long] =
    val r = lib.proven_queue_pop(ptr)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Get the current queue size. */
  def size: Long = lib.proven_queue_size(ptr)

  override def close(): Unit =
    if ptr != null then
      lib.proven_queue_free(ptr)
      ptr = null

object SafeQueue:
  /** Create a bounded queue. Returns None on failure. */
  def create(capacity: Long): Option[SafeQueue] =
    val p = lib.proven_queue_create(capacity)
    if p == null then None else Some(new SafeQueue(p))

// ---------------------------------------------------------------------------
// SafeBloom -- Bloom filter
// ---------------------------------------------------------------------------

/**
 * Bloom filter backed by libproven.
 * Call close() when done to free native memory.
 */
class SafeBloom private (private var ptr: Pointer) extends AutoCloseable:
  /** Add an element to the filter. */
  def add(data: Array[Byte]): Unit = lib.proven_bloom_add(ptr, data, data.length.toLong)

  /** Add a string element. */
  def add(value: String): Unit = add(toBytes(value))

  /** Check if element might be in the filter. True may be a false positive. */
  def contains(data: Array[Byte]): Boolean = lib.proven_bloom_contains(ptr, data, data.length.toLong)

  /** Check if string element might be in the filter. */
  def contains(value: String): Boolean = contains(toBytes(value))

  override def close(): Unit =
    if ptr != null then
      lib.proven_bloom_free(ptr)
      ptr = null

object SafeBloom:
  /** Create a Bloom filter. Returns None on failure. */
  def create(expectedElements: Long, falsePositiveRate: Double): Option[SafeBloom] =
    val p = lib.proven_bloom_create(expectedElements, falsePositiveRate)
    if p == null then None else Some(new SafeBloom(p))

// ---------------------------------------------------------------------------
// SafeLRU -- LRU cache
// ---------------------------------------------------------------------------

/**
 * LRU cache backed by libproven.
 * Call close() when done to free native memory.
 */
class SafeLRU private (private var ptr: Pointer) extends AutoCloseable:
  /** Get a value by key. Returns None if not found. */
  def get(key: Long): Option[Long] =
    val r = lib.proven_lru_get(ptr, key)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  /** Put a key-value pair. Returns true on success. */
  def put(key: Long, value: Long): Boolean =
    lib.proven_lru_put(ptr, key, value) == ProvenStatus.OK

  override def close(): Unit =
    if ptr != null then
      lib.proven_lru_free(ptr)
      ptr = null

object SafeLRU:
  /** Create an LRU cache. Returns None on failure. */
  def create(capacity: Long): Option[SafeLRU] =
    val p = lib.proven_lru_create(capacity)
    if p == null then None else Some(new SafeLRU(p))

// ---------------------------------------------------------------------------
// SafeGraph -- directed graph
// ---------------------------------------------------------------------------

/**
 * Directed graph backed by libproven.
 * Call close() when done to free native memory.
 */
class SafeGraph private (private var ptr: Pointer) extends AutoCloseable:
  /** Add a directed edge. Returns true on success. */
  def addEdge(from: Long, to: Long): Boolean =
    lib.proven_graph_add_edge(ptr, from, to) == ProvenStatus.OK

  /** Check if an edge exists. */
  def hasEdge(from: Long, to: Long): Boolean =
    lib.proven_graph_has_edge(ptr, from, to)

  override def close(): Unit =
    if ptr != null then
      lib.proven_graph_free(ptr)
      ptr = null

object SafeGraph:
  /** Create a graph with the given node count. Returns None on failure. */
  def create(nodeCount: Long): Option[SafeGraph] =
    val p = lib.proven_graph_create(nodeCount)
    if p == null then None else Some(new SafeGraph(p))

// ---------------------------------------------------------------------------
// SafeRateLimiter -- token bucket rate limiter
// ---------------------------------------------------------------------------

/**
 * Token bucket rate limiter backed by libproven.
 * Call close() when done to free native memory.
 */
class SafeRateLimiter private (private var ptr: Pointer) extends AutoCloseable:
  /** Try to acquire tokens. Returns true if successful. */
  def tryAcquire(tokens: Double = 1.0): Boolean =
    lib.proven_rate_limiter_try_acquire(ptr, tokens)

  override def close(): Unit =
    if ptr != null then
      lib.proven_rate_limiter_free(ptr)
      ptr = null

object SafeRateLimiter:
  /** Create a rate limiter. Returns None on failure. */
  def create(capacity: Double, refillRate: Double): Option[SafeRateLimiter] =
    val p = lib.proven_rate_limiter_create(capacity, refillRate)
    if p == null then None else Some(new SafeRateLimiter(p))

// ---------------------------------------------------------------------------
// SafeCircuitBreaker -- circuit breaker pattern
// ---------------------------------------------------------------------------

/**
 * Circuit breaker backed by libproven.
 * Call close() when done to free native memory.
 */
class SafeCircuitBreaker private (private var ptr: Pointer) extends AutoCloseable:
  /** Check if a request should be allowed. */
  def allow(): Boolean = lib.proven_circuit_breaker_allow(ptr)

  /** Record a success. */
  def success(): Unit = lib.proven_circuit_breaker_success(ptr)

  /** Record a failure. */
  def failure(): Unit = lib.proven_circuit_breaker_failure(ptr)

  /** Get the current state (see CircuitState constants). */
  def state: Int = lib.proven_circuit_breaker_state(ptr)

  override def close(): Unit =
    if ptr != null then
      lib.proven_circuit_breaker_free(ptr)
      ptr = null

object SafeCircuitBreaker:
  /** Create a circuit breaker. Returns None on failure. */
  def create(
      failureThreshold: Int,
      successThreshold: Int,
      timeoutMs: Long
  ): Option[SafeCircuitBreaker] =
    val p = lib.proven_circuit_breaker_create(failureThreshold, successThreshold, timeoutMs)
    if p == null then None else Some(new SafeCircuitBreaker(p))

// ---------------------------------------------------------------------------
// SafeRetry -- exponential backoff
// ---------------------------------------------------------------------------

/** Retry configuration. */
case class RetryConfig(
    maxAttempts: Int,
    baseDelayMs: Long,
    maxDelayMs: Long,
    multiplier: Double
)

/** Retry operations delegated to libproven. */
object SafeRetry:
  private def toNative(config: RetryConfig): ProvenRetryConfig.ByValue =
    val native = new ProvenRetryConfig.ByValue()
    native.max_attempts = config.maxAttempts
    native.base_delay_ms = config.baseDelayMs
    native.max_delay_ms = config.maxDelayMs
    native.multiplier = config.multiplier
    native

  /** Calculate delay for a given attempt (1-based). Returns 0 if attempt exceeds max. */
  def delay(config: RetryConfig, attempt: Int): Long =
    lib.proven_retry_delay(toNative(config), attempt)

  /** Check if more retries are allowed. */
  def shouldRetry(config: RetryConfig, attempt: Int): Boolean =
    lib.proven_retry_should_retry(toNative(config), attempt)

// ---------------------------------------------------------------------------
// SafeMonotonic -- monotonically increasing counter
// ---------------------------------------------------------------------------

/**
 * Monotonic counter backed by libproven.
 * Call close() when done to free native memory.
 */
class SafeMonotonic private (private var ptr: Pointer) extends AutoCloseable:
  /** Get next value and increment. Returns None if max reached. */
  def next(): Option[Long] =
    val r = lib.proven_monotonic_next(ptr)
    if r.status == ProvenStatus.OK then Some(r.value) else None

  override def close(): Unit =
    if ptr != null then
      lib.proven_monotonic_free(ptr)
      ptr = null

object SafeMonotonic:
  /** Create a monotonic counter. Returns None on failure. */
  def create(initial: Long, maxValue: Long): Option[SafeMonotonic] =
    val p = lib.proven_monotonic_create(initial, maxValue)
    if p == null then None else Some(new SafeMonotonic(p))

// ---------------------------------------------------------------------------
// SafeStateMachine -- type-safe state transitions
// ---------------------------------------------------------------------------

/**
 * State machine backed by libproven.
 * Call close() when done to free native memory.
 */
class SafeStateMachine private (private var ptr: Pointer) extends AutoCloseable:
  /** Allow a transition from one state to another. Returns true on success. */
  def allowTransition(from: Int, to: Int): Boolean =
    lib.proven_state_machine_allow(ptr, from, to)

  /** Attempt to transition to a new state. Returns true if the transition was allowed. */
  def transition(to: Int): Boolean = lib.proven_state_machine_transition(ptr, to)

  /** Get the current state index. */
  def currentState: Int = lib.proven_state_machine_state(ptr)

  override def close(): Unit =
    if ptr != null then
      lib.proven_state_machine_free(ptr)
      ptr = null

object SafeStateMachine:
  /** Create a state machine. Returns None on failure. */
  def create(stateCount: Int, initialState: Int): Option[SafeStateMachine] =
    val p = lib.proven_state_machine_create(stateCount, initialState)
    if p == null then None else Some(new SafeStateMachine(p))
