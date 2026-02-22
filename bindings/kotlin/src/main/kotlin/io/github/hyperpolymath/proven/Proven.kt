// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Idiomatic Kotlin wrappers over the ProvenNative JNA interface.
// All computation is delegated to libproven via FFI -- no logic reimplemented.

package io.github.hyperpolymath.proven

import com.sun.jna.Pointer

// ---------------------------------------------------------------------------
// Status code constants matching the C enum ProvenStatus
// ---------------------------------------------------------------------------

/** Status codes returned by libproven operations. */
object ProvenStatus {
    const val OK = 0
    const val ERR_NULL_POINTER = -1
    const val ERR_INVALID_ARGUMENT = -2
    const val ERR_OVERFLOW = -3
    const val ERR_UNDERFLOW = -4
    const val ERR_DIVISION_BY_ZERO = -5
    const val ERR_PARSE_FAILURE = -6
    const val ERR_VALIDATION_FAILED = -7
    const val ERR_OUT_OF_BOUNDS = -8
    const val ERR_ENCODING_ERROR = -9
    const val ERR_ALLOCATION_FAILED = -10
    const val ERR_NOT_IMPLEMENTED = -99
}

/** JSON type constants matching C enum ProvenJsonType. */
object ProvenJsonType {
    const val NULL = 0
    const val BOOL = 1
    const val NUMBER = 2
    const val STRING = 3
    const val ARRAY = 4
    const val OBJECT = 5
    const val INVALID = -1
}

/** Length unit constants matching C enum ProvenLengthUnit. */
object LengthUnit {
    const val METERS = 0
    const val KILOMETERS = 1
    const val CENTIMETERS = 2
    const val MILLIMETERS = 3
    const val FEET = 4
    const val INCHES = 5
    const val MILES = 6
    const val YARDS = 7
}

/** Temperature unit constants matching C enum ProvenTempUnit. */
object TempUnit {
    const val CELSIUS = 0
    const val FAHRENHEIT = 1
    const val KELVIN = 2
}

/** Circuit breaker state constants matching C enum ProvenCircuitState. */
object CircuitState {
    const val CLOSED = 0
    const val OPEN = 1
    const val HALF_OPEN = 2
}

// ---------------------------------------------------------------------------
// Internal helper: extract a String from ProvenStringResult and free native memory
// ---------------------------------------------------------------------------

private val lib = ProvenNative.INSTANCE

/**
 * Read the C string from a [ProvenStringResult], copy it to a JVM String,
 * and free the native allocation via `proven_free_string`.
 *
 * Returns null if the result status is not OK.
 */
private fun ProvenStringResult.extractAndFree(): String? {
    if (status != ProvenStatus.OK) return null
    val ptr = value ?: return null
    val result = ptr.getString(0, "UTF-8")
    lib.proven_free_string(ptr)
    return result
}

// ---------------------------------------------------------------------------
// Proven runtime management
// ---------------------------------------------------------------------------

/**
 * Entry-point object for initialising and querying the Proven runtime.
 */
object Proven {
    /** Initialise the Proven runtime. Returns true on success. */
    fun init(): Boolean = lib.proven_init() == ProvenStatus.OK

    /** Deinitialise the Proven runtime. */
    fun deinit() = lib.proven_deinit()

    /** Check whether the runtime is initialised. */
    fun isInitialized(): Boolean = lib.proven_is_initialized()

    /** Get the FFI ABI version number for compatibility checking. */
    fun ffiAbiVersion(): Int = lib.proven_ffi_abi_version()

    /** Get the library major version. */
    fun versionMajor(): Int = lib.proven_version_major()

    /** Get the library minor version. */
    fun versionMinor(): Int = lib.proven_version_minor()

    /** Get the library patch version. */
    fun versionPatch(): Int = lib.proven_version_patch()

    /** Get a "major.minor.patch" version string. */
    fun versionString(): String = "${versionMajor()}.${versionMinor()}.${versionPatch()}"
}

// ---------------------------------------------------------------------------
// SafeMath -- arithmetic that cannot crash
// ---------------------------------------------------------------------------

/**
 * Safe arithmetic operations delegated to libproven.
 * Returns null on error (overflow, division by zero, etc.) -- never throws.
 */
object SafeMath {
    /** Safe integer division. Returns null on division by zero or MIN_INT/-1 overflow. */
    fun div(numerator: Long, denominator: Long): Long? {
        val r = lib.proven_math_div(numerator, denominator)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Safe modulo. Returns null on division by zero. */
    fun mod(numerator: Long, denominator: Long): Long? {
        val r = lib.proven_math_mod(numerator, denominator)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Checked addition. Returns null on overflow. */
    fun add(a: Long, b: Long): Long? {
        val r = lib.proven_math_add_checked(a, b)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Checked subtraction. Returns null on underflow. */
    fun sub(a: Long, b: Long): Long? {
        val r = lib.proven_math_sub_checked(a, b)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Checked multiplication. Returns null on overflow. */
    fun mul(a: Long, b: Long): Long? {
        val r = lib.proven_math_mul_checked(a, b)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Checked exponentiation. Returns null on overflow. */
    fun pow(base: Long, exp: Int): Long? {
        val r = lib.proven_math_pow_checked(base, exp)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Safe absolute value. Returns null for Long.MIN_VALUE. */
    fun abs(n: Long): Long? {
        val r = lib.proven_math_abs_safe(n)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Clamp [value] to [[lo], [hi]]. */
    fun clamp(lo: Long, hi: Long, value: Long): Long =
        lib.proven_math_clamp(lo, hi, value)
}

// ---------------------------------------------------------------------------
// SafeString -- text operations handling encoding safely
// ---------------------------------------------------------------------------

/** String escaping and validation delegated to libproven. */
object SafeString {
    /** Check if bytes are valid UTF-8. */
    fun isValidUtf8(data: ByteArray): Boolean {
        val r = lib.proven_string_is_valid_utf8(data, data.size.toLong())
        return r.status == ProvenStatus.OK && r.boolValue()
    }

    /** Escape a string for SQL (single-quote escaping). Prefer parameterised queries. */
    fun escapeSql(value: String): String? {
        val bytes = value.toByteArray(Charsets.UTF_8)
        return lib.proven_string_escape_sql(bytes, bytes.size.toLong()).extractAndFree()
    }

    /** Escape a string for HTML (prevents XSS). */
    fun escapeHtml(value: String): String? {
        val bytes = value.toByteArray(Charsets.UTF_8)
        return lib.proven_string_escape_html(bytes, bytes.size.toLong()).extractAndFree()
    }

    /** Escape a string for JavaScript string literals. */
    fun escapeJs(value: String): String? {
        val bytes = value.toByteArray(Charsets.UTF_8)
        return lib.proven_string_escape_js(bytes, bytes.size.toLong()).extractAndFree()
    }
}

// ---------------------------------------------------------------------------
// SafePath -- filesystem operations preventing traversal attacks
// ---------------------------------------------------------------------------

/** Path validation and sanitisation delegated to libproven. */
object SafePath {
    /** Check if path contains directory traversal sequences (..). */
    fun hasTraversal(path: String): Boolean {
        val bytes = path.toByteArray(Charsets.UTF_8)
        val r = lib.proven_path_has_traversal(bytes, bytes.size.toLong())
        return r.status == ProvenStatus.OK && r.boolValue()
    }

    /** Sanitise a filename by removing dangerous characters. */
    fun sanitizeFilename(filename: String): String? {
        val bytes = filename.toByteArray(Charsets.UTF_8)
        return lib.proven_path_sanitize_filename(bytes, bytes.size.toLong()).extractAndFree()
    }
}

// ---------------------------------------------------------------------------
// SafeCrypto -- cryptographic primitives
// ---------------------------------------------------------------------------

/** Cryptographic operations delegated to libproven. */
object SafeCrypto {
    /** Constant-time byte comparison (timing-safe). */
    fun constantTimeEquals(a: ByteArray, b: ByteArray): Boolean {
        val r = lib.proven_crypto_constant_time_eq(
            a, a.size.toLong(), b, b.size.toLong()
        )
        return r.status == ProvenStatus.OK && r.boolValue()
    }

    /** Fill [buffer] with cryptographically secure random bytes. Returns true on success. */
    fun randomBytes(buffer: ByteArray): Boolean =
        lib.proven_crypto_random_bytes(buffer, buffer.size.toLong()) == ProvenStatus.OK

    /** Generate [size] cryptographically secure random bytes. Returns null on failure. */
    fun generateRandomBytes(size: Int): ByteArray? {
        val buf = ByteArray(size)
        return if (randomBytes(buf)) buf else null
    }
}

// ---------------------------------------------------------------------------
// SafeEmail -- email validation
// ---------------------------------------------------------------------------

/** Email validation delegated to libproven. */
object SafeEmail {
    /** Validate email address (RFC 5321 simplified). */
    fun isValid(email: String): Boolean {
        val bytes = email.toByteArray(Charsets.UTF_8)
        val r = lib.proven_email_is_valid(bytes, bytes.size.toLong())
        return r.status == ProvenStatus.OK && r.boolValue()
    }
}

// ---------------------------------------------------------------------------
// SafeUrl -- URL parsing
// ---------------------------------------------------------------------------

/** Parsed URL components (Kotlin data class). */
data class ParsedUrl(
    val scheme: String,
    val host: String,
    val port: Int?,
    val path: String,
    val query: String?,
    val fragment: String?
)

/** URL parsing delegated to libproven. */
object SafeUrl {
    /** Parse a URL into its components. Returns null on parse failure. */
    fun parse(url: String): ParsedUrl? {
        val bytes = url.toByteArray(Charsets.UTF_8)
        val r = lib.proven_url_parse(bytes, bytes.size.toLong())
        if (r.status != ProvenStatus.OK) return null

        val c = r.components
        val scheme = c.scheme?.getString(0, "UTF-8") ?: return null
        val host = c.host?.getString(0, "UTF-8") ?: return null
        val path = c.path?.getString(0, "UTF-8") ?: "/"
        val query = c.query?.getString(0, "UTF-8")
        val fragment = c.fragment?.getString(0, "UTF-8")
        val port = if (c.has_port.toInt() != 0) c.port.toInt() and 0xFFFF else null

        // Free the native URL components
        lib.proven_url_free(c)

        return ParsedUrl(scheme, host, port, path, query, fragment)
    }
}

// ---------------------------------------------------------------------------
// SafeNetwork -- IP address parsing and classification
// ---------------------------------------------------------------------------

/** Parsed IPv4 address. */
data class IPv4Address(val a: Int, val b: Int, val c: Int, val d: Int) {
    override fun toString(): String = "$a.$b.$c.$d"
}

/** Network operations delegated to libproven. */
object SafeNetwork {
    /** Parse an IPv4 address string. Returns null on failure. */
    fun parseIPv4(address: String): IPv4Address? {
        val bytes = address.toByteArray(Charsets.UTF_8)
        val r = lib.proven_network_parse_ipv4(bytes, bytes.size.toLong())
        if (r.status != ProvenStatus.OK) return null
        val o = r.address.octets
        return IPv4Address(
            o[0].toInt() and 0xFF, o[1].toInt() and 0xFF,
            o[2].toInt() and 0xFF, o[3].toInt() and 0xFF
        )
    }

    /** Check if an IPv4 address is private (RFC 1918). */
    fun isPrivate(address: IPv4Address): Boolean {
        val addr = ProvenIPv4Address.ByValue()
        addr.octets = byteArrayOf(
            address.a.toByte(), address.b.toByte(),
            address.c.toByte(), address.d.toByte()
        )
        return lib.proven_network_ipv4_is_private(addr)
    }

    /** Check if an IPv4 address is loopback (127.0.0.0/8). */
    fun isLoopback(address: IPv4Address): Boolean {
        val addr = ProvenIPv4Address.ByValue()
        addr.octets = byteArrayOf(
            address.a.toByte(), address.b.toByte(),
            address.c.toByte(), address.d.toByte()
        )
        return lib.proven_network_ipv4_is_loopback(addr)
    }
}

// ---------------------------------------------------------------------------
// SafeJson -- JSON validation and type detection
// ---------------------------------------------------------------------------

/** JSON validation delegated to libproven. */
object SafeJson {
    /** Check if a string is valid JSON. */
    fun isValid(json: String): Boolean {
        val bytes = json.toByteArray(Charsets.UTF_8)
        val r = lib.proven_json_is_valid(bytes, bytes.size.toLong())
        return r.status == ProvenStatus.OK && r.boolValue()
    }

    /** Get JSON value type at root level. Returns [ProvenJsonType] constant. */
    fun getType(json: String): Int {
        val bytes = json.toByteArray(Charsets.UTF_8)
        return lib.proven_json_get_type(bytes, bytes.size.toLong())
    }
}

// ---------------------------------------------------------------------------
// SafeDatetime -- ISO 8601 date/time handling
// ---------------------------------------------------------------------------

/** Parsed datetime components. */
data class DateTime(
    val year: Int,
    val month: Int,
    val day: Int,
    val hour: Int,
    val minute: Int,
    val second: Int,
    val nanosecond: Int,
    val tzOffsetMinutes: Int
)

/** Datetime operations delegated to libproven. */
object SafeDatetime {
    /** Parse ISO 8601 date/time string. Returns null on failure. */
    fun parse(dateString: String): DateTime? {
        val bytes = dateString.toByteArray(Charsets.UTF_8)
        val r = lib.proven_datetime_parse(bytes, bytes.size.toLong())
        if (r.status != ProvenStatus.OK) return null
        val dt = r.datetime
        return DateTime(
            dt.year, dt.month.toInt() and 0xFF, dt.day.toInt() and 0xFF,
            dt.hour.toInt() and 0xFF, dt.minute.toInt() and 0xFF, dt.second.toInt() and 0xFF,
            dt.nanosecond, dt.tz_offset_minutes.toInt()
        )
    }

    /** Format a DateTime as ISO 8601 string. Returns null on failure. */
    fun formatIso8601(dt: DateTime): String? {
        val nativeDt = ProvenDateTime.ByValue()
        nativeDt.year = dt.year
        nativeDt.month = dt.month.toByte()
        nativeDt.day = dt.day.toByte()
        nativeDt.hour = dt.hour.toByte()
        nativeDt.minute = dt.minute.toByte()
        nativeDt.second = dt.second.toByte()
        nativeDt.nanosecond = dt.nanosecond
        nativeDt.tz_offset_minutes = dt.tzOffsetMinutes.toShort()
        return lib.proven_datetime_format_iso8601(nativeDt).extractAndFree()
    }

    /** Check if a year is a leap year. */
    fun isLeapYear(year: Int): Boolean = lib.proven_datetime_is_leap_year(year)

    /** Get the number of days in a month (0 if invalid). */
    fun daysInMonth(year: Int, month: Int): Int =
        lib.proven_datetime_days_in_month(year, month.toByte()).toInt() and 0xFF
}

// ---------------------------------------------------------------------------
// SafeFloat -- safe floating-point operations
// ---------------------------------------------------------------------------

/** Safe floating-point operations delegated to libproven. */
object SafeFloat {
    /** Safe floating-point division. Returns null if b is 0, NaN, or result would be infinite. */
    fun div(a: Double, b: Double): Double? {
        val r = lib.proven_float_div(a, b)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Safe square root. Returns null for negative or NaN inputs. */
    fun sqrt(x: Double): Double? {
        val r = lib.proven_float_sqrt(x)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Safe natural logarithm. Returns null for non-positive or NaN inputs. */
    fun ln(x: Double): Double? {
        val r = lib.proven_float_ln(x)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Check if a double is finite (not NaN or Inf). */
    fun isFinite(x: Double): Boolean = lib.proven_float_is_finite(x)

    /** Check if a double is NaN. */
    fun isNaN(x: Double): Boolean = lib.proven_float_is_nan(x)
}

// ---------------------------------------------------------------------------
// SafeVersion -- semantic versioning
// ---------------------------------------------------------------------------

/** Parsed semantic version. */
data class SemanticVersion(
    val major: Int,
    val minor: Int,
    val patch: Int,
    val prerelease: String?
)

/** Semantic version operations delegated to libproven. */
object SafeVersion {
    /** Parse a semantic version string. Returns null on failure. */
    fun parse(versionString: String): SemanticVersion? {
        val bytes = versionString.toByteArray(Charsets.UTF_8)
        val r = lib.proven_version_parse(bytes, bytes.size.toLong())
        if (r.status != ProvenStatus.OK) return null
        val v = r.version
        val pre = if (v.prerelease_len > 0 && v.prerelease != null) {
            v.prerelease!!.getString(0, "UTF-8")
        } else null
        return SemanticVersion(v.major, v.minor, v.patch, pre)
    }

    /** Compare two semantic versions. Returns negative, 0, or positive. */
    fun compare(a: SemanticVersion, b: SemanticVersion): Int {
        val na = ProvenSemanticVersion.ByValue()
        na.major = a.major; na.minor = a.minor; na.patch = a.patch
        val nb = ProvenSemanticVersion.ByValue()
        nb.major = b.major; nb.minor = b.minor; nb.patch = b.patch
        return lib.proven_version_compare(na, nb)
    }
}

// ---------------------------------------------------------------------------
// SafeColor -- color parsing and conversion
// ---------------------------------------------------------------------------

/** RGB color with 0-255 components. */
data class RGBColor(val r: Int, val g: Int, val b: Int)

/** HSL color with hue 0-360, saturation 0-1, lightness 0-1. */
data class HSLColor(val h: Double, val s: Double, val l: Double)

/** Color operations delegated to libproven. */
object SafeColor {
    /** Parse a hex color string (e.g., "#FF0000" or "F00"). Returns null on failure. */
    fun parseHex(hex: String): RGBColor? {
        val bytes = hex.toByteArray(Charsets.UTF_8)
        val r = lib.proven_color_parse_hex(bytes, bytes.size.toLong())
        if (r.status != ProvenStatus.OK) return null
        return RGBColor(
            r.color.r.toInt() and 0xFF,
            r.color.g.toInt() and 0xFF,
            r.color.b.toInt() and 0xFF
        )
    }

    /** Convert RGB to HSL. */
    fun rgbToHsl(rgb: RGBColor): HSLColor {
        val native = ProvenRGBColor.ByValue()
        native.r = rgb.r.toByte()
        native.g = rgb.g.toByte()
        native.b = rgb.b.toByte()
        val hsl = lib.proven_color_rgb_to_hsl(native)
        return HSLColor(hsl.h, hsl.s, hsl.l)
    }

    /** Format RGB as hex string (e.g., "#rrggbb"). Returns null on failure. */
    fun toHex(rgb: RGBColor): String? {
        val native = ProvenRGBColor.ByValue()
        native.r = rgb.r.toByte()
        native.g = rgb.g.toByte()
        native.b = rgb.b.toByte()
        return lib.proven_color_to_hex(native).extractAndFree()
    }
}

// ---------------------------------------------------------------------------
// SafeAngle -- angle conversions and normalisation
// ---------------------------------------------------------------------------

/** Angle conversion and normalisation delegated to libproven. */
object SafeAngle {
    /** Convert degrees to radians. */
    fun degToRad(degrees: Double): Double = lib.proven_angle_deg_to_rad(degrees)

    /** Convert radians to degrees. */
    fun radToDeg(radians: Double): Double = lib.proven_angle_rad_to_deg(radians)

    /** Normalise angle to [0, 360) degrees. */
    fun normalizeDegrees(degrees: Double): Double = lib.proven_angle_normalize_degrees(degrees)

    /** Normalise angle to [0, 2*pi) radians. */
    fun normalizeRadians(radians: Double): Double = lib.proven_angle_normalize_radians(radians)
}

// ---------------------------------------------------------------------------
// SafeUnit -- physical unit conversions
// ---------------------------------------------------------------------------

/** Physical unit conversions delegated to libproven. */
object SafeUnit {
    /** Convert length between units. Returns null on invalid input. */
    fun convertLength(value: Double, from: Int, to: Int): Double? {
        val r = lib.proven_unit_convert_length(value, from, to)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Convert temperature between units. Returns null on invalid input. */
    fun convertTemp(value: Double, from: Int, to: Int): Double? {
        val r = lib.proven_unit_convert_temp(value, from, to)
        return if (r.status == ProvenStatus.OK) r.value else null
    }
}

// ---------------------------------------------------------------------------
// SafeCalculator -- safe expression evaluation
// ---------------------------------------------------------------------------

/** Expression evaluation delegated to libproven. */
object SafeCalculator {
    /** Evaluate an arithmetic expression string. Returns null on parse/division error. */
    fun eval(expression: String): Double? {
        val bytes = expression.toByteArray(Charsets.UTF_8)
        val r = lib.proven_calculator_eval(bytes, bytes.size.toLong())
        return if (r.status == ProvenStatus.OK) r.value else null
    }
}

// ---------------------------------------------------------------------------
// SafeBuffer -- bounded buffer operations
// ---------------------------------------------------------------------------

/**
 * Bounded buffer backed by libproven. Manages a native buffer pointer.
 * Call [close] when done to free native memory.
 */
class SafeBuffer private constructor(private var ptr: Pointer?) : AutoCloseable {
    companion object {
        /** Create a bounded buffer with the given capacity. Returns null on failure. */
        fun create(capacity: Long): SafeBuffer? {
            val p = lib.proven_buffer_create(capacity) ?: return null
            return SafeBuffer(p)
        }
    }

    /** Append data to the buffer. Returns true on success. */
    fun append(data: ByteArray): Boolean =
        lib.proven_buffer_append(ptr, data, data.size.toLong()) == ProvenStatus.OK

    /** Free native memory. */
    override fun close() {
        lib.proven_buffer_free(ptr)
        ptr = null
    }
}

// ---------------------------------------------------------------------------
// SafeQueue -- bounded FIFO queue
// ---------------------------------------------------------------------------

/**
 * Bounded FIFO queue backed by libproven.
 * Call [close] when done to free native memory.
 */
class SafeQueue private constructor(private var ptr: Pointer?) : AutoCloseable {
    companion object {
        /** Create a bounded queue. Returns null on failure. */
        fun create(capacity: Long): SafeQueue? {
            val p = lib.proven_queue_create(capacity) ?: return null
            return SafeQueue(p)
        }
    }

    /** Push a value. Returns true on success, false if full. */
    fun push(value: Long): Boolean = lib.proven_queue_push(ptr, value)

    /** Pop a value. Returns null if empty. */
    fun pop(): Long? {
        val r = lib.proven_queue_pop(ptr)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Get the current queue size. */
    fun size(): Long = lib.proven_queue_size(ptr)

    override fun close() {
        lib.proven_queue_free(ptr)
        ptr = null
    }
}

// ---------------------------------------------------------------------------
// SafeBloom -- Bloom filter
// ---------------------------------------------------------------------------

/**
 * Bloom filter backed by libproven.
 * Call [close] when done to free native memory.
 */
class SafeBloom private constructor(private var ptr: Pointer?) : AutoCloseable {
    companion object {
        /** Create a Bloom filter. Returns null on failure. */
        fun create(expectedElements: Long, falsePositiveRate: Double): SafeBloom? {
            val p = lib.proven_bloom_create(expectedElements, falsePositiveRate) ?: return null
            return SafeBloom(p)
        }
    }

    /** Add an element to the filter. */
    fun add(data: ByteArray) = lib.proven_bloom_add(ptr, data, data.size.toLong())

    /** Add a string element. */
    fun add(value: String) = add(value.toByteArray(Charsets.UTF_8))

    /** Check if element might be in the filter. True may be a false positive. */
    fun contains(data: ByteArray): Boolean = lib.proven_bloom_contains(ptr, data, data.size.toLong())

    /** Check if string element might be in the filter. */
    fun contains(value: String): Boolean = contains(value.toByteArray(Charsets.UTF_8))

    override fun close() {
        lib.proven_bloom_free(ptr)
        ptr = null
    }
}

// ---------------------------------------------------------------------------
// SafeLRU -- LRU cache
// ---------------------------------------------------------------------------

/**
 * LRU cache backed by libproven.
 * Call [close] when done to free native memory.
 */
class SafeLRU private constructor(private var ptr: Pointer?) : AutoCloseable {
    companion object {
        /** Create an LRU cache. Returns null on failure. */
        fun create(capacity: Long): SafeLRU? {
            val p = lib.proven_lru_create(capacity) ?: return null
            return SafeLRU(p)
        }
    }

    /** Get a value by key. Returns null if not found. */
    fun get(key: Long): Long? {
        val r = lib.proven_lru_get(ptr, key)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    /** Put a key-value pair. Returns true on success. */
    fun put(key: Long, value: Long): Boolean =
        lib.proven_lru_put(ptr, key, value) == ProvenStatus.OK

    override fun close() {
        lib.proven_lru_free(ptr)
        ptr = null
    }
}

// ---------------------------------------------------------------------------
// SafeGraph -- directed graph
// ---------------------------------------------------------------------------

/**
 * Directed graph backed by libproven.
 * Call [close] when done to free native memory.
 */
class SafeGraph private constructor(private var ptr: Pointer?) : AutoCloseable {
    companion object {
        /** Create a graph with the given node count. Returns null on failure. */
        fun create(nodeCount: Long): SafeGraph? {
            val p = lib.proven_graph_create(nodeCount) ?: return null
            return SafeGraph(p)
        }
    }

    /** Add a directed edge. Returns true on success. */
    fun addEdge(from: Long, to: Long): Boolean =
        lib.proven_graph_add_edge(ptr, from, to) == ProvenStatus.OK

    /** Check if an edge exists. */
    fun hasEdge(from: Long, to: Long): Boolean =
        lib.proven_graph_has_edge(ptr, from, to)

    override fun close() {
        lib.proven_graph_free(ptr)
        ptr = null
    }
}

// ---------------------------------------------------------------------------
// SafeRateLimiter -- token bucket rate limiter
// ---------------------------------------------------------------------------

/**
 * Token bucket rate limiter backed by libproven.
 * Call [close] when done to free native memory.
 */
class SafeRateLimiter private constructor(private var ptr: Pointer?) : AutoCloseable {
    companion object {
        /** Create a rate limiter. Returns null on failure. */
        fun create(capacity: Double, refillRate: Double): SafeRateLimiter? {
            val p = lib.proven_rate_limiter_create(capacity, refillRate) ?: return null
            return SafeRateLimiter(p)
        }
    }

    /** Try to acquire tokens. Returns true if successful. */
    fun tryAcquire(tokens: Double = 1.0): Boolean =
        lib.proven_rate_limiter_try_acquire(ptr, tokens)

    override fun close() {
        lib.proven_rate_limiter_free(ptr)
        ptr = null
    }
}

// ---------------------------------------------------------------------------
// SafeCircuitBreaker -- circuit breaker pattern
// ---------------------------------------------------------------------------

/**
 * Circuit breaker backed by libproven.
 * Call [close] when done to free native memory.
 */
class SafeCircuitBreaker private constructor(private var ptr: Pointer?) : AutoCloseable {
    companion object {
        /** Create a circuit breaker. Returns null on failure. */
        fun create(
            failureThreshold: Int,
            successThreshold: Int,
            timeoutMs: Long
        ): SafeCircuitBreaker? {
            val p = lib.proven_circuit_breaker_create(
                failureThreshold, successThreshold, timeoutMs
            ) ?: return null
            return SafeCircuitBreaker(p)
        }
    }

    /** Check if a request should be allowed. */
    fun allow(): Boolean = lib.proven_circuit_breaker_allow(ptr)

    /** Record a success. */
    fun success() = lib.proven_circuit_breaker_success(ptr)

    /** Record a failure. */
    fun failure() = lib.proven_circuit_breaker_failure(ptr)

    /** Get the current state (see [CircuitState] constants). */
    fun state(): Int = lib.proven_circuit_breaker_state(ptr)

    override fun close() {
        lib.proven_circuit_breaker_free(ptr)
        ptr = null
    }
}

// ---------------------------------------------------------------------------
// SafeRetry -- exponential backoff
// ---------------------------------------------------------------------------

/** Retry configuration. */
data class RetryConfig(
    val maxAttempts: Int,
    val baseDelayMs: Long,
    val maxDelayMs: Long,
    val multiplier: Double
)

/** Retry operations delegated to libproven. */
object SafeRetry {
    /** Calculate delay for a given attempt (1-based). Returns 0 if attempt exceeds max. */
    fun delay(config: RetryConfig, attempt: Int): Long {
        val native = ProvenRetryConfig.ByValue()
        native.max_attempts = config.maxAttempts
        native.base_delay_ms = config.baseDelayMs
        native.max_delay_ms = config.maxDelayMs
        native.multiplier = config.multiplier
        return lib.proven_retry_delay(native, attempt)
    }

    /** Check if more retries are allowed. */
    fun shouldRetry(config: RetryConfig, attempt: Int): Boolean {
        val native = ProvenRetryConfig.ByValue()
        native.max_attempts = config.maxAttempts
        native.base_delay_ms = config.baseDelayMs
        native.max_delay_ms = config.maxDelayMs
        native.multiplier = config.multiplier
        return lib.proven_retry_should_retry(native, attempt)
    }
}

// ---------------------------------------------------------------------------
// SafeMonotonic -- monotonically increasing counter
// ---------------------------------------------------------------------------

/**
 * Monotonic counter backed by libproven.
 * Call [close] when done to free native memory.
 */
class SafeMonotonic private constructor(private var ptr: Pointer?) : AutoCloseable {
    companion object {
        /** Create a monotonic counter. Returns null on failure. */
        fun create(initial: Long, maxValue: Long): SafeMonotonic? {
            val p = lib.proven_monotonic_create(initial, maxValue) ?: return null
            return SafeMonotonic(p)
        }
    }

    /** Get next value and increment. Returns null if max reached. */
    fun next(): Long? {
        val r = lib.proven_monotonic_next(ptr)
        return if (r.status == ProvenStatus.OK) r.value else null
    }

    override fun close() {
        lib.proven_monotonic_free(ptr)
        ptr = null
    }
}

// ---------------------------------------------------------------------------
// SafeStateMachine -- type-safe state transitions
// ---------------------------------------------------------------------------

/**
 * State machine backed by libproven.
 * Call [close] when done to free native memory.
 */
class SafeStateMachine private constructor(private var ptr: Pointer?) : AutoCloseable {
    companion object {
        /** Create a state machine. Returns null on failure. */
        fun create(stateCount: Int, initialState: Int): SafeStateMachine? {
            val p = lib.proven_state_machine_create(stateCount, initialState) ?: return null
            return SafeStateMachine(p)
        }
    }

    /** Allow a transition from one state to another. Returns true on success. */
    fun allowTransition(from: Int, to: Int): Boolean =
        lib.proven_state_machine_allow(ptr, from, to)

    /** Attempt to transition to a new state. Returns true if the transition was allowed. */
    fun transition(to: Int): Boolean = lib.proven_state_machine_transition(ptr, to)

    /** Get the current state index. */
    fun currentState(): Int = lib.proven_state_machine_state(ptr)

    override fun close() {
        lib.proven_state_machine_free(ptr)
        ptr = null
    }
}
