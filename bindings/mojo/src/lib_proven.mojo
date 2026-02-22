# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
Raw C FFI declarations for libproven.

This module defines the FFI result structs and low-level external_call wrappers
that map 1:1 to the C functions exported by libproven. All higher-level wrapper
modules (safe_math, safe_string, etc.) build on these primitives.

CRITICAL: No logic is reimplemented here. Every function delegates to libproven
via external_call. The Idris 2 core performs all computation.

Link against libproven.so or libproven.a at compile time.
"""

from memory import UnsafePointer


# =============================================================================
# Status Codes
# =============================================================================

alias PROVEN_OK: Int32 = 0
alias PROVEN_ERR_NULL_POINTER: Int32 = -1
alias PROVEN_ERR_INVALID_ARGUMENT: Int32 = -2
alias PROVEN_ERR_OVERFLOW: Int32 = -3
alias PROVEN_ERR_UNDERFLOW: Int32 = -4
alias PROVEN_ERR_DIVISION_BY_ZERO: Int32 = -5
alias PROVEN_ERR_PARSE_FAILURE: Int32 = -6
alias PROVEN_ERR_VALIDATION_FAILED: Int32 = -7
alias PROVEN_ERR_OUT_OF_BOUNDS: Int32 = -8
alias PROVEN_ERR_ENCODING_ERROR: Int32 = -9
alias PROVEN_ERR_ALLOCATION_FAILED: Int32 = -10
alias PROVEN_ERR_NOT_IMPLEMENTED: Int32 = -99


# =============================================================================
# ProvenError -- Mojo-idiomatic error representation
# =============================================================================


fn status_to_string(status: Int32) -> String:
    """Convert a Proven status code to a human-readable error string."""
    if status == PROVEN_OK:
        return "OK"
    if status == PROVEN_ERR_NULL_POINTER:
        return "null pointer"
    if status == PROVEN_ERR_INVALID_ARGUMENT:
        return "invalid argument"
    if status == PROVEN_ERR_OVERFLOW:
        return "overflow"
    if status == PROVEN_ERR_UNDERFLOW:
        return "underflow"
    if status == PROVEN_ERR_DIVISION_BY_ZERO:
        return "division by zero"
    if status == PROVEN_ERR_PARSE_FAILURE:
        return "parse failure"
    if status == PROVEN_ERR_VALIDATION_FAILED:
        return "validation failed"
    if status == PROVEN_ERR_OUT_OF_BOUNDS:
        return "out of bounds"
    if status == PROVEN_ERR_ENCODING_ERROR:
        return "encoding error"
    if status == PROVEN_ERR_ALLOCATION_FAILED:
        return "allocation failed"
    if status == PROVEN_ERR_NOT_IMPLEMENTED:
        return "not implemented"
    return "unknown error (code=" + str(Int(status)) + ")"


# =============================================================================
# Core Result Types (C ABI compatible)
# =============================================================================


@register_passable("trivial")
struct IntResult:
    """C ABI: struct { int32_t status; int64_t value; }."""

    var status: Int32
    var value: Int64

    fn __init__(out self, status: Int32, value: Int64):
        self.status = status
        self.value = value

    fn succeeded(self) -> Bool:
        """Return True if the operation succeeded (status == 0)."""
        return self.status == PROVEN_OK

    fn failed(self) -> Bool:
        """Return True if the operation failed (status != 0)."""
        return self.status != PROVEN_OK


@register_passable("trivial")
struct BoolResult:
    """C ABI: struct { int32_t status; bool value; }."""

    var status: Int32
    var value: Bool

    fn __init__(out self, status: Int32, value: Bool):
        self.status = status
        self.value = value

    fn succeeded(self) -> Bool:
        """Return True if the operation succeeded (status == 0)."""
        return self.status == PROVEN_OK

    fn failed(self) -> Bool:
        """Return True if the operation failed (status != 0)."""
        return self.status != PROVEN_OK


@register_passable("trivial")
struct StringResult:
    """C ABI: struct { int32_t status; char* value; size_t length; }."""

    var status: Int32
    var ptr: UnsafePointer[UInt8]
    var length: Int

    fn __init__(out self, status: Int32, ptr: UnsafePointer[UInt8], length: Int):
        self.status = status
        self.ptr = ptr
        self.length = length

    fn succeeded(self) -> Bool:
        """Return True if the operation succeeded (status == 0)."""
        return self.status == PROVEN_OK

    fn failed(self) -> Bool:
        """Return True if the operation failed (status != 0)."""
        return self.status != PROVEN_OK


@register_passable("trivial")
struct FloatResult:
    """C ABI: struct { int32_t status; double value; }."""

    var status: Int32
    var value: Float64

    fn __init__(out self, status: Int32, value: Float64):
        self.status = status
        self.value = value

    fn succeeded(self) -> Bool:
        """Return True if the operation succeeded (status == 0)."""
        return self.status == PROVEN_OK

    fn failed(self) -> Bool:
        """Return True if the operation failed (status != 0)."""
        return self.status != PROVEN_OK


@register_passable("trivial")
struct IPv4Address:
    """C ABI: struct { uint8_t octets[4]; }."""

    var o0: UInt8
    var o1: UInt8
    var o2: UInt8
    var o3: UInt8

    fn __init__(out self, o0: UInt8, o1: UInt8, o2: UInt8, o3: UInt8):
        self.o0 = o0
        self.o1 = o1
        self.o2 = o2
        self.o3 = o3

    fn __init__(out self):
        self.o0 = 0
        self.o1 = 0
        self.o2 = 0
        self.o3 = 0


@register_passable("trivial")
struct IPv4Result:
    """C ABI: struct { int32_t status; ProvenIPv4Address address; }."""

    var status: Int32
    var address: IPv4Address

    fn __init__(out self, status: Int32, address: IPv4Address):
        self.status = status
        self.address = address

    fn succeeded(self) -> Bool:
        """Return True if the operation succeeded (status == 0)."""
        return self.status == PROVEN_OK

    fn failed(self) -> Bool:
        """Return True if the operation failed (status != 0)."""
        return self.status != PROVEN_OK


@register_passable("trivial")
struct DateTime:
    """C ABI: ProvenDateTime struct for ISO 8601 date/time components."""

    var year: Int32
    var month: UInt8
    var day: UInt8
    var hour: UInt8
    var minute: UInt8
    var second: UInt8
    var nanosecond: UInt32
    var tz_offset_minutes: Int16

    fn __init__(
        out self,
        year: Int32,
        month: UInt8,
        day: UInt8,
        hour: UInt8,
        minute: UInt8,
        second: UInt8,
        nanosecond: UInt32,
        tz_offset_minutes: Int16,
    ):
        self.year = year
        self.month = month
        self.day = day
        self.hour = hour
        self.minute = minute
        self.second = second
        self.nanosecond = nanosecond
        self.tz_offset_minutes = tz_offset_minutes

    fn __init__(out self):
        self.year = 0
        self.month = 0
        self.day = 0
        self.hour = 0
        self.minute = 0
        self.second = 0
        self.nanosecond = 0
        self.tz_offset_minutes = 0


@register_passable("trivial")
struct DateTimeResult:
    """C ABI: struct { int32_t status; ProvenDateTime datetime; }."""

    var status: Int32
    var datetime: DateTime

    fn __init__(out self, status: Int32, datetime: DateTime):
        self.status = status
        self.datetime = datetime

    fn succeeded(self) -> Bool:
        """Return True if the operation succeeded (status == 0)."""
        return self.status == PROVEN_OK

    fn failed(self) -> Bool:
        """Return True if the operation failed (status != 0)."""
        return self.status != PROVEN_OK


# =============================================================================
# JSON type enumeration
# =============================================================================

alias PROVEN_JSON_NULL: Int32 = 0
alias PROVEN_JSON_BOOL: Int32 = 1
alias PROVEN_JSON_NUMBER: Int32 = 2
alias PROVEN_JSON_STRING: Int32 = 3
alias PROVEN_JSON_ARRAY: Int32 = 4
alias PROVEN_JSON_OBJECT: Int32 = 5
alias PROVEN_JSON_INVALID: Int32 = -1


# =============================================================================
# Lifecycle FFI
# =============================================================================


fn proven_init() -> Int32:
    """Initialize the Proven runtime. Must be called before other functions."""
    return external_call["proven_init", Int32]()


fn proven_deinit():
    """Shutdown the Proven runtime. Free all resources before calling."""
    external_call["proven_deinit", NoneType]()


fn proven_is_initialized() -> Bool:
    """Check if the Proven runtime has been initialized."""
    return external_call["proven_is_initialized", Bool]()


# =============================================================================
# Memory Management FFI
# =============================================================================


fn proven_free_string(ptr: UnsafePointer[UInt8]):
    """Free a string allocated by a Proven function. Safe to call with null."""
    external_call["proven_free_string", NoneType, UnsafePointer[UInt8]](ptr)


# =============================================================================
# Version FFI
# =============================================================================


fn proven_ffi_abi_version() -> UInt32:
    """Get FFI ABI version for compatibility checking."""
    return external_call["proven_ffi_abi_version", UInt32]()


fn proven_version_major() -> UInt32:
    """Get the library major version number."""
    return external_call["proven_version_major", UInt32]()


fn proven_version_minor() -> UInt32:
    """Get the library minor version number."""
    return external_call["proven_version_minor", UInt32]()


fn proven_version_patch() -> UInt32:
    """Get the library patch version number."""
    return external_call["proven_version_patch", UInt32]()


fn proven_module_count() -> UInt32:
    """Get total number of modules in the library."""
    return external_call["proven_module_count", UInt32]()


# =============================================================================
# SafeMath FFI
# =============================================================================


fn proven_math_add_checked(a: Int64, b: Int64) -> IntResult:
    """Checked addition with overflow detection. Delegates to libproven."""
    return external_call["proven_math_add_checked", IntResult, Int64, Int64](a, b)


fn proven_math_sub_checked(a: Int64, b: Int64) -> IntResult:
    """Checked subtraction with underflow detection. Delegates to libproven."""
    return external_call["proven_math_sub_checked", IntResult, Int64, Int64](a, b)


fn proven_math_mul_checked(a: Int64, b: Int64) -> IntResult:
    """Checked multiplication with overflow detection. Delegates to libproven."""
    return external_call["proven_math_mul_checked", IntResult, Int64, Int64](a, b)


fn proven_math_div(numerator: Int64, denominator: Int64) -> IntResult:
    """Safe integer division. Returns error on division by zero or overflow."""
    return external_call["proven_math_div", IntResult, Int64, Int64](
        numerator, denominator
    )


fn proven_math_mod(numerator: Int64, denominator: Int64) -> IntResult:
    """Safe modulo operation. Returns error on division by zero."""
    return external_call["proven_math_mod", IntResult, Int64, Int64](
        numerator, denominator
    )


fn proven_math_abs_safe(n: Int64) -> IntResult:
    """Safe absolute value. Returns error for INT64_MIN."""
    return external_call["proven_math_abs_safe", IntResult, Int64](n)


fn proven_math_clamp(lo: Int64, hi: Int64, value: Int64) -> Int64:
    """Clamp value to [lo, hi] range."""
    return external_call["proven_math_clamp", Int64, Int64, Int64, Int64](
        lo, hi, value
    )


fn proven_math_pow_checked(base: Int64, exp: UInt32) -> IntResult:
    """Integer exponentiation with overflow checking."""
    return external_call["proven_math_pow_checked", IntResult, Int64, UInt32](
        base, exp
    )


# =============================================================================
# SafeFloat FFI
# =============================================================================


fn proven_float_div(a: Float64, b: Float64) -> FloatResult:
    """Safe floating-point division. Error on zero divisor or NaN."""
    return external_call["proven_float_div", FloatResult, Float64, Float64](a, b)


fn proven_float_is_finite(x: Float64) -> Bool:
    """Check if float is finite (not NaN or Inf)."""
    return external_call["proven_float_is_finite", Bool, Float64](x)


fn proven_float_is_nan(x: Float64) -> Bool:
    """Check if float is NaN."""
    return external_call["proven_float_is_nan", Bool, Float64](x)


fn proven_float_sqrt(x: Float64) -> FloatResult:
    """Safe square root. Error if x is negative or NaN."""
    return external_call["proven_float_sqrt", FloatResult, Float64](x)


fn proven_float_ln(x: Float64) -> FloatResult:
    """Safe natural logarithm. Error if x <= 0 or NaN."""
    return external_call["proven_float_ln", FloatResult, Float64](x)


# =============================================================================
# SafeString FFI
# =============================================================================


fn proven_string_is_valid_utf8(
    ptr: UnsafePointer[UInt8], length: Int
) -> BoolResult:
    """Check if bytes are valid UTF-8. Delegates to libproven."""
    return external_call[
        "proven_string_is_valid_utf8", BoolResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_string_escape_sql(
    ptr: UnsafePointer[UInt8], length: Int
) -> StringResult:
    """Escape string for SQL. Caller must free result with proven_free_string."""
    return external_call[
        "proven_string_escape_sql", StringResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_string_escape_html(
    ptr: UnsafePointer[UInt8], length: Int
) -> StringResult:
    """Escape string for HTML (XSS prevention). Caller must free result."""
    return external_call[
        "proven_string_escape_html", StringResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_string_escape_js(
    ptr: UnsafePointer[UInt8], length: Int
) -> StringResult:
    """Escape string for JavaScript literals. Caller must free result."""
    return external_call[
        "proven_string_escape_js", StringResult, UnsafePointer[UInt8], Int
    ](ptr, length)


# =============================================================================
# SafePath FFI
# =============================================================================


fn proven_path_has_traversal(
    ptr: UnsafePointer[UInt8], length: Int
) -> BoolResult:
    """Check if path contains directory traversal sequences (..)."""
    return external_call[
        "proven_path_has_traversal", BoolResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_path_sanitize_filename(
    ptr: UnsafePointer[UInt8], length: Int
) -> StringResult:
    """Sanitize a filename. Caller must free result with proven_free_string."""
    return external_call[
        "proven_path_sanitize_filename", StringResult, UnsafePointer[UInt8], Int
    ](ptr, length)


# =============================================================================
# SafeEmail FFI
# =============================================================================


fn proven_email_is_valid(
    ptr: UnsafePointer[UInt8], length: Int
) -> BoolResult:
    """Validate email address (RFC 5321 simplified)."""
    return external_call[
        "proven_email_is_valid", BoolResult, UnsafePointer[UInt8], Int
    ](ptr, length)


# =============================================================================
# SafeNetwork FFI
# =============================================================================


fn proven_network_parse_ipv4(
    ptr: UnsafePointer[UInt8], length: Int
) -> IPv4Result:
    """Parse IPv4 address string (e.g. '192.168.1.1')."""
    return external_call[
        "proven_network_parse_ipv4", IPv4Result, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_network_ipv4_is_private(addr: IPv4Address) -> Bool:
    """Check if IPv4 address is private (RFC 1918)."""
    return external_call[
        "proven_network_ipv4_is_private", Bool, IPv4Address
    ](addr)


fn proven_network_ipv4_is_loopback(addr: IPv4Address) -> Bool:
    """Check if IPv4 address is loopback (127.0.0.0/8)."""
    return external_call[
        "proven_network_ipv4_is_loopback", Bool, IPv4Address
    ](addr)


# =============================================================================
# SafeCrypto FFI
# =============================================================================


fn proven_crypto_constant_time_eq(
    ptr1: UnsafePointer[UInt8],
    len1: Int,
    ptr2: UnsafePointer[UInt8],
    len2: Int,
) -> BoolResult:
    """Constant-time byte comparison (timing-attack safe)."""
    return external_call[
        "proven_crypto_constant_time_eq",
        BoolResult,
        UnsafePointer[UInt8],
        Int,
        UnsafePointer[UInt8],
        Int,
    ](ptr1, len1, ptr2, len2)


fn proven_crypto_random_bytes(ptr: UnsafePointer[UInt8], length: Int) -> Int32:
    """Fill buffer with cryptographically secure random bytes."""
    return external_call[
        "proven_crypto_random_bytes", Int32, UnsafePointer[UInt8], Int
    ](ptr, length)


# =============================================================================
# SafeJson FFI
# =============================================================================


fn proven_json_is_valid(
    ptr: UnsafePointer[UInt8], length: Int
) -> BoolResult:
    """Check if string is valid JSON."""
    return external_call[
        "proven_json_is_valid", BoolResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_json_get_type(ptr: UnsafePointer[UInt8], length: Int) -> Int32:
    """Get JSON value type at root level. Returns PROVEN_JSON_INVALID if invalid."""
    return external_call[
        "proven_json_get_type", Int32, UnsafePointer[UInt8], Int
    ](ptr, length)


# =============================================================================
# SafeDateTime FFI
# =============================================================================


fn proven_datetime_parse(
    ptr: UnsafePointer[UInt8], length: Int
) -> DateTimeResult:
    """Parse ISO 8601 date string."""
    return external_call[
        "proven_datetime_parse", DateTimeResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_datetime_format_iso8601(dt: DateTime) -> StringResult:
    """Format DateTime as ISO 8601 string. Caller must free result."""
    return external_call[
        "proven_datetime_format_iso8601", StringResult, DateTime
    ](dt)


fn proven_datetime_is_leap_year(year: Int32) -> Bool:
    """Check if a year is a leap year."""
    return external_call["proven_datetime_is_leap_year", Bool, Int32](year)


fn proven_datetime_days_in_month(year: Int32, month: UInt8) -> UInt8:
    """Get number of days in a month. Returns 0 if month is invalid."""
    return external_call[
        "proven_datetime_days_in_month", UInt8, Int32, UInt8
    ](year, month)


# =============================================================================
# SafeHex FFI
# =============================================================================


fn proven_hex_encode(
    ptr: UnsafePointer[UInt8], length: Int, uppercase: Bool
) -> StringResult:
    """Encode bytes to hex string. Caller must free result."""
    return external_call[
        "proven_hex_encode", StringResult, UnsafePointer[UInt8], Int, Bool
    ](ptr, length, uppercase)


# =============================================================================
# SafeChecksum FFI
# =============================================================================


fn proven_checksum_crc32(
    ptr: UnsafePointer[UInt8], length: Int
) -> IntResult:
    """Calculate CRC32 checksum."""
    return external_call[
        "proven_checksum_crc32", IntResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_checksum_verify_crc32(
    ptr: UnsafePointer[UInt8], length: Int, expected: UInt32
) -> BoolResult:
    """Verify CRC32 matches expected value."""
    return external_call[
        "proven_checksum_verify_crc32",
        BoolResult,
        UnsafePointer[UInt8],
        Int,
        UInt32,
    ](ptr, length, expected)


# =============================================================================
# SafeProbability FFI
# =============================================================================


fn proven_probability_create(value: Float64) -> Float64:
    """Create probability value clamped to [0, 1]."""
    return external_call["proven_probability_create", Float64, Float64](value)


fn proven_probability_and(a: Float64, b: Float64) -> Float64:
    """Multiply probabilities (independent events)."""
    return external_call["proven_probability_and", Float64, Float64, Float64](
        a, b
    )


fn proven_probability_or_exclusive(a: Float64, b: Float64) -> Float64:
    """Add probabilities (mutually exclusive events), clamped to 1.0."""
    return external_call[
        "proven_probability_or_exclusive", Float64, Float64, Float64
    ](a, b)


fn proven_probability_not(p: Float64) -> Float64:
    """Complement probability: 1 - p."""
    return external_call["proven_probability_not", Float64, Float64](p)


# =============================================================================
# SafeCalculator FFI
# =============================================================================


fn proven_calculator_eval(
    ptr: UnsafePointer[UInt8], length: Int
) -> FloatResult:
    """Evaluate an arithmetic expression safely."""
    return external_call[
        "proven_calculator_eval", FloatResult, UnsafePointer[UInt8], Int
    ](ptr, length)


# =============================================================================
# SafeAngle FFI
# =============================================================================


fn proven_angle_deg_to_rad(degrees: Float64) -> Float64:
    """Convert degrees to radians."""
    return external_call["proven_angle_deg_to_rad", Float64, Float64](degrees)


fn proven_angle_rad_to_deg(radians: Float64) -> Float64:
    """Convert radians to degrees."""
    return external_call["proven_angle_rad_to_deg", Float64, Float64](radians)


fn proven_angle_normalize_degrees(degrees: Float64) -> Float64:
    """Normalize angle to [0, 360) degrees."""
    return external_call["proven_angle_normalize_degrees", Float64, Float64](
        degrees
    )


fn proven_angle_normalize_radians(radians: Float64) -> Float64:
    """Normalize angle to [0, 2*pi) radians."""
    return external_call["proven_angle_normalize_radians", Float64, Float64](
        radians
    )


# =============================================================================
# SafeML FFI
# =============================================================================


fn proven_ml_sigmoid(x: Float64) -> Float64:
    """Sigmoid function: 1 / (1 + exp(-x))."""
    return external_call["proven_ml_sigmoid", Float64, Float64](x)


fn proven_ml_relu(x: Float64) -> Float64:
    """ReLU function: max(0, x)."""
    return external_call["proven_ml_relu", Float64, Float64](x)


fn proven_ml_leaky_relu(x: Float64, alpha: Float64) -> Float64:
    """Leaky ReLU: x >= 0 ? x : alpha * x."""
    return external_call["proven_ml_leaky_relu", Float64, Float64, Float64](
        x, alpha
    )


fn proven_ml_clamp(x: Float64, min_val: Float64, max_val: Float64) -> Float64:
    """Clamp value to [min_val, max_val]."""
    return external_call[
        "proven_ml_clamp", Float64, Float64, Float64, Float64
    ](x, min_val, max_val)


# =============================================================================
# SafePassword FFI
# =============================================================================


fn proven_password_is_common(
    ptr: UnsafePointer[UInt8], length: Int
) -> Bool:
    """Check if password is in the common passwords list."""
    return external_call[
        "proven_password_is_common", Bool, UnsafePointer[UInt8], Int
    ](ptr, length)


# =============================================================================
# SafeUnit FFI (length unit aliases)
# =============================================================================

alias PROVEN_LENGTH_METERS: Int32 = 0
alias PROVEN_LENGTH_KILOMETERS: Int32 = 1
alias PROVEN_LENGTH_CENTIMETERS: Int32 = 2
alias PROVEN_LENGTH_MILLIMETERS: Int32 = 3
alias PROVEN_LENGTH_FEET: Int32 = 4
alias PROVEN_LENGTH_INCHES: Int32 = 5
alias PROVEN_LENGTH_MILES: Int32 = 6
alias PROVEN_LENGTH_YARDS: Int32 = 7

alias PROVEN_TEMP_CELSIUS: Int32 = 0
alias PROVEN_TEMP_FAHRENHEIT: Int32 = 1
alias PROVEN_TEMP_KELVIN: Int32 = 2


fn proven_unit_convert_length(
    value: Float64, from_unit: Int32, to_unit: Int32
) -> FloatResult:
    """Convert length between units."""
    return external_call[
        "proven_unit_convert_length", FloatResult, Float64, Int32, Int32
    ](value, from_unit, to_unit)


fn proven_unit_convert_temp(
    value: Float64, from_unit: Int32, to_unit: Int32
) -> FloatResult:
    """Convert temperature between units."""
    return external_call[
        "proven_unit_convert_temp", FloatResult, Float64, Int32, Int32
    ](value, from_unit, to_unit)


# =============================================================================
# SafeHeader FFI
# =============================================================================


fn proven_header_has_crlf(
    ptr: UnsafePointer[UInt8], length: Int
) -> BoolResult:
    """Check for CRLF injection characters in header value."""
    return external_call[
        "proven_header_has_crlf", BoolResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_header_is_valid_name(
    ptr: UnsafePointer[UInt8], length: Int
) -> BoolResult:
    """Check if header name is a valid token per RFC 7230."""
    return external_call[
        "proven_header_is_valid_name", BoolResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_header_is_dangerous(
    ptr: UnsafePointer[UInt8], length: Int
) -> BoolResult:
    """Check if header name is in the dangerous headers list."""
    return external_call[
        "proven_header_is_dangerous", BoolResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn proven_header_render(
    name_ptr: UnsafePointer[UInt8],
    name_len: Int,
    value_ptr: UnsafePointer[UInt8],
    value_len: Int,
) -> StringResult:
    """Create validated header string 'Name: Value'. Caller must free result."""
    return external_call[
        "proven_header_render",
        StringResult,
        UnsafePointer[UInt8],
        Int,
        UnsafePointer[UInt8],
        Int,
    ](name_ptr, name_len, value_ptr, value_len)


fn proven_header_build_hsts(
    max_age: Int64, include_subdomains: Bool, preload: Bool
) -> StringResult:
    """Build HSTS header value. Caller must free result."""
    return external_call[
        "proven_header_build_hsts", StringResult, Int64, Bool, Bool
    ](max_age, include_subdomains, preload)


# =============================================================================
# Utility: copy StringResult to a Mojo String and free the C allocation
# =============================================================================


fn string_result_to_string(result: StringResult) -> Optional[String]:
    """Convert a successful StringResult to a Mojo String, freeing the C buffer.

    Returns None if the result indicates failure.
    """
    if result.failed():
        return None

    if result.length == 0:
        proven_free_string(result.ptr)
        return String("")

    # Copy bytes from the C allocation into a Mojo String
    var buf = List[UInt8](capacity=result.length + 1)
    for i in range(result.length):
        buf.append(result.ptr[i])
    buf.append(0)  # null terminator for String
    proven_free_string(result.ptr)
    return String(buf^)
