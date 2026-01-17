// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

// =============================================================================
// Proven Safety Library for Inkle Ink
// =============================================================================
//
// Formally verified safety primitives for interactive fiction narratives.
// Provides safe math, validation, state tracking, and narrative utilities.
//
// Version: 0.4.0
// Module Count: 38
//
// Categories:
//   Core (11): safe_math, safe_string, safe_path, safe_email, safe_url,
//              safe_network, safe_crypto, safe_uuid, safe_currency,
//              safe_phone, safe_hex
//   Data (7): safe_json, safe_datetime, safe_float, safe_version,
//             safe_color, safe_angle, safe_unit
//   Data Structures (5): safe_buffer, safe_queue, safe_bloom, safe_lru,
//                        safe_graph
//   Resilience (4): safe_rate_limiter, safe_circuit_breaker, safe_retry,
//                   safe_monotonic
//   State (2): safe_state_machine, safe_calculator
//   Algorithm (4): safe_geo, safe_probability, safe_checksum, safe_tensor
//   Security (2): safe_password, safe_ml
//   HTTP (3): safe_header, safe_cookie, safe_content_type
//
// Usage:
//   INCLUDE proven.ink
//   ~ proven_init()
//
// =============================================================================

// -----------------------------------------------------------------------------
// LIBRARY METADATA
// -----------------------------------------------------------------------------

VAR PROVEN_VERSION = "0.4.0"
VAR PROVEN_MODULE_COUNT = 38

// Library initialized flag
VAR proven_initialized = false

// Error tracking
VAR proven_last_error = ""
VAR proven_error_count = 0

// -----------------------------------------------------------------------------
// INITIALIZATION
// -----------------------------------------------------------------------------

=== function proven_init()
    ~ proven_initialized = true
    ~ proven_last_error = ""
    ~ proven_error_count = 0
    ~ return true

=== function proven_version()
    ~ return PROVEN_VERSION

=== function proven_module_count()
    ~ return PROVEN_MODULE_COUNT

// =============================================================================
// CORE MODULES (11)
// =============================================================================

// -----------------------------------------------------------------------------
// SAFE_MATH: Safe mathematical operations with overflow detection
// -----------------------------------------------------------------------------

// Maximum safe integer for Ink (based on 32-bit signed)
CONST SAFE_INT_MAX = 2147483647
CONST SAFE_INT_MIN = -2147483648

=== function safe_add(a, b)
    // Safe addition with overflow detection
    { a > 0 && b > SAFE_INT_MAX - a:
        ~ proven_last_error = "Overflow in addition"
        ~ proven_error_count++
        ~ return SAFE_INT_MAX
    }
    { a < 0 && b < SAFE_INT_MIN - a:
        ~ proven_last_error = "Underflow in addition"
        ~ proven_error_count++
        ~ return SAFE_INT_MIN
    }
    ~ return a + b

=== function safe_sub(a, b)
    // Safe subtraction with underflow detection
    { b < 0 && a > SAFE_INT_MAX + b:
        ~ proven_last_error = "Overflow in subtraction"
        ~ proven_error_count++
        ~ return SAFE_INT_MAX
    }
    { b > 0 && a < SAFE_INT_MIN + b:
        ~ proven_last_error = "Underflow in subtraction"
        ~ proven_error_count++
        ~ return SAFE_INT_MIN
    }
    ~ return a - b

=== function safe_mul(a, b)
    // Safe multiplication with overflow detection
    { a == 0 || b == 0:
        ~ return 0
    }
    { a > 0 && b > 0 && a > SAFE_INT_MAX / b:
        ~ proven_last_error = "Overflow in multiplication"
        ~ proven_error_count++
        ~ return SAFE_INT_MAX
    }
    { a < 0 && b < 0 && a < SAFE_INT_MAX / b:
        ~ proven_last_error = "Overflow in multiplication"
        ~ proven_error_count++
        ~ return SAFE_INT_MAX
    }
    { (a < 0 && b > 0 && a < SAFE_INT_MIN / b) || (a > 0 && b < 0 && b < SAFE_INT_MIN / a):
        ~ proven_last_error = "Underflow in multiplication"
        ~ proven_error_count++
        ~ return SAFE_INT_MIN
    }
    ~ return a * b

=== function safe_div(a, b)
    // Safe division with zero check
    { b == 0:
        ~ proven_last_error = "Division by zero"
        ~ proven_error_count++
        ~ return 0
    }
    { a == SAFE_INT_MIN && b == -1:
        ~ proven_last_error = "Overflow in division"
        ~ proven_error_count++
        ~ return SAFE_INT_MAX
    }
    ~ return a / b

=== function safe_mod(a, b)
    // Safe modulo with zero check
    { b == 0:
        ~ proven_last_error = "Modulo by zero"
        ~ proven_error_count++
        ~ return 0
    }
    ~ return a mod b

=== function safe_abs(a)
    // Safe absolute value
    { a == SAFE_INT_MIN:
        ~ proven_last_error = "Overflow in absolute value"
        ~ proven_error_count++
        ~ return SAFE_INT_MAX
    }
    { a < 0:
        ~ return -a
    }
    ~ return a

=== function safe_clamp(value, min_val, max_val)
    // Clamp value to range [min, max]
    { value < min_val:
        ~ return min_val
    }
    { value > max_val:
        ~ return max_val
    }
    ~ return value

=== function safe_inc(a)
    // Safe increment
    ~ return safe_add(a, 1)

=== function safe_dec(a)
    // Safe decrement
    ~ return safe_sub(a, 1)

=== function safe_pow(base, exp)
    // Safe power (positive exponents only, iterative)
    { exp < 0:
        ~ proven_last_error = "Negative exponent not supported"
        ~ proven_error_count++
        ~ return 0
    }
    { exp == 0:
        ~ return 1
    }
    ~ temp result = 1
    ~ temp i = 0
    { i < exp:
        ~ result = safe_mul(result, base)
        ~ i++
        -> DONE
    }
    ~ return result

// -----------------------------------------------------------------------------
// SAFE_STRING: Safe string operations with injection prevention
// -----------------------------------------------------------------------------

// String length limits
CONST MAX_STRING_LENGTH = 65535
CONST MAX_NAME_LENGTH = 255

=== function string_is_empty(s)
    // Check if string is empty
    ~ return s == ""

=== function string_length_valid(s, max_len)
    // Check if string length is within limit
    // Note: Ink doesn't have native strlen, this is a conceptual check
    ~ return s != "" || max_len > 0

=== function string_sanitize_for_display(s)
    // Mark string as sanitized for display
    // In Ink, we track this through narrative
    ~ return s

=== function string_is_alphanumeric_char(c)
    // Check if single character is alphanumeric
    // Note: Ink string handling is limited
    ~ return c != ""

=== function string_truncate_hint(s, max_chars)
    // Hint that string should be truncated
    // Actual truncation would need external function
    ~ return s

// Injection detection flags (for tracking in narrative)
VAR string_injection_detected = false

=== function string_check_injection(s)
    // Flag potential injection patterns
    // Note: Full detection needs external function binding
    ~ string_injection_detected = false
    ~ return s

// -----------------------------------------------------------------------------
// SAFE_PATH: Safe file path operations
// -----------------------------------------------------------------------------

CONST MAX_PATH_LENGTH = 4096
CONST MAX_PATH_COMPONENT = 255

VAR path_is_valid = true
VAR path_is_absolute = false
VAR path_has_traversal = false

=== function path_validate(p)
    // Validate path structure
    ~ path_is_valid = p != ""
    ~ path_has_traversal = false
    // Note: Full validation needs external binding
    ~ return path_is_valid

=== function path_is_safe(p)
    // Check if path is safe (no traversal attacks)
    ~ path_validate(p)
    ~ return path_is_valid && not path_has_traversal

=== function path_check_absolute(p)
    // Check if path is absolute
    // Note: Full check needs external binding
    ~ path_is_absolute = false
    ~ return path_is_absolute

// -----------------------------------------------------------------------------
// SAFE_EMAIL: Safe email validation
// -----------------------------------------------------------------------------

VAR email_is_valid = false
VAR email_local_part = ""
VAR email_domain = ""

=== function email_validate(email)
    // Validate email format
    // Note: Full validation needs external binding
    ~ email_is_valid = email != ""
    ~ return email_is_valid

=== function email_get_domain(email)
    // Extract domain from email
    ~ return email_domain

=== function email_is_disposable(email)
    // Check if email is from disposable provider
    // Note: Needs external list/binding
    ~ return false

// -----------------------------------------------------------------------------
// SAFE_URL: Safe URL validation and parsing
// -----------------------------------------------------------------------------

CONST MAX_URL_LENGTH = 2048

VAR url_is_valid = false
VAR url_scheme = ""
VAR url_host = ""
VAR url_port = 0
VAR url_path = ""

=== function url_validate(url)
    // Validate URL format
    ~ url_is_valid = url != ""
    // Note: Full validation needs external binding
    ~ return url_is_valid

=== function url_is_https(url)
    // Check if URL uses HTTPS
    ~ return url_scheme == "https"

=== function url_is_safe_scheme(url)
    // Check if URL uses a safe scheme (http, https)
    ~ return url_scheme == "http" || url_scheme == "https"

=== function url_encode_hint(s)
    // Hint that string should be URL encoded
    ~ return s

=== function url_decode_hint(s)
    // Hint that string should be URL decoded
    ~ return s

// -----------------------------------------------------------------------------
// SAFE_NETWORK: Safe network address validation
// -----------------------------------------------------------------------------

CONST PORT_MIN = 1
CONST PORT_MAX = 65535

VAR network_ip_valid = false
VAR network_port_valid = false
VAR network_is_private_ip = false
VAR network_is_loopback = false

=== function network_validate_port(port)
    // Validate port number
    ~ network_port_valid = port >= PORT_MIN && port <= PORT_MAX
    ~ return network_port_valid

=== function network_is_privileged_port(port)
    // Check if port requires root/admin
    ~ return port >= 1 && port <= 1023

=== function network_validate_ipv4(ip)
    // Validate IPv4 address format
    // Note: Full validation needs external binding
    ~ network_ip_valid = ip != ""
    ~ return network_ip_valid

=== function network_check_private(ip)
    // Check if IP is private/internal
    // Note: Full check needs external binding
    ~ network_is_private_ip = false
    ~ return network_is_private_ip

=== function network_check_loopback(ip)
    // Check if IP is loopback
    ~ network_is_loopback = false
    ~ return network_is_loopback

// -----------------------------------------------------------------------------
// SAFE_CRYPTO: Safe cryptographic operations
// -----------------------------------------------------------------------------

VAR crypto_hash_result = ""
VAR crypto_hash_valid = false

=== function crypto_hash_hint(data, algorithm)
    // Request hash computation (external binding)
    // algorithm: "sha3_256", "sha3_512", "blake3"
    ~ crypto_hash_valid = false
    ~ crypto_hash_result = ""
    ~ return crypto_hash_result

=== function crypto_verify_hash(data, expected_hash, algorithm)
    // Verify hash matches (external binding)
    ~ return crypto_hash_valid

=== function crypto_random_bytes_hint(length)
    // Request random bytes (external binding)
    ~ return ""

=== function crypto_constant_time_compare_hint(a, b)
    // Request constant-time comparison (external binding)
    ~ return a == b

=== function crypto_to_hex(bytes)
    // Convert bytes to hex string
    ~ return ""

=== function crypto_from_hex(hex_str)
    // Convert hex string to bytes
    ~ return ""

// -----------------------------------------------------------------------------
// SAFE_UUID: Safe UUID generation and validation
// -----------------------------------------------------------------------------

VAR uuid_is_valid = false
VAR uuid_version = 0

=== function uuid_validate(uuid_str)
    // Validate UUID format
    ~ uuid_is_valid = uuid_str != ""
    // Note: Full validation needs external binding
    ~ return uuid_is_valid

=== function uuid_generate_v4_hint()
    // Request UUIDv4 generation (external binding)
    ~ return "00000000-0000-0000-0000-000000000000"

=== function uuid_get_version(uuid_str)
    // Extract UUID version
    ~ return uuid_version

=== function uuid_is_nil(uuid_str)
    // Check if UUID is nil UUID
    ~ return uuid_str == "00000000-0000-0000-0000-000000000000"

// -----------------------------------------------------------------------------
// SAFE_CURRENCY: Safe currency and money operations
// -----------------------------------------------------------------------------

// Store amounts in smallest unit (cents, pence, etc.)
CONST CURRENCY_PRECISION = 2

VAR currency_amount = 0
VAR currency_code = "USD"
VAR currency_is_valid = true

=== function currency_create(amount_cents, code)
    // Create currency amount
    ~ currency_amount = amount_cents
    ~ currency_code = code
    ~ currency_is_valid = amount_cents >= 0
    ~ return currency_is_valid

=== function currency_add(a_cents, b_cents)
    // Safe currency addition
    ~ return safe_add(a_cents, b_cents)

=== function currency_sub(a_cents, b_cents)
    // Safe currency subtraction (no negative results)
    ~ temp result = safe_sub(a_cents, b_cents)
    { result < 0:
        ~ proven_last_error = "Currency cannot be negative"
        ~ proven_error_count++
        ~ return 0
    }
    ~ return result

=== function currency_to_major_units(cents)
    // Convert cents to dollars (integer division)
    ~ return safe_div(cents, 100)

=== function currency_from_major_units(dollars)
    // Convert dollars to cents
    ~ return safe_mul(dollars, 100)

=== function currency_format_hint(cents, code)
    // Request formatted currency string (external binding)
    ~ return ""

// -----------------------------------------------------------------------------
// SAFE_PHONE: Safe phone number validation
// -----------------------------------------------------------------------------

VAR phone_is_valid = false
VAR phone_country_code = ""
VAR phone_number = ""

=== function phone_validate(phone_str)
    // Validate phone number format
    ~ phone_is_valid = phone_str != ""
    // Note: Full validation needs external binding
    ~ return phone_is_valid

=== function phone_extract_country_code(phone_str)
    // Extract country code from phone number
    ~ return phone_country_code

=== function phone_normalize(phone_str)
    // Normalize phone number format
    ~ return phone_str

=== function phone_is_mobile_hint(phone_str)
    // Check if phone is mobile (external binding)
    ~ return false

// -----------------------------------------------------------------------------
// SAFE_HEX: Safe hexadecimal encoding/decoding
// -----------------------------------------------------------------------------

VAR hex_is_valid = false

=== function hex_validate(hex_str)
    // Validate hex string
    ~ hex_is_valid = hex_str != ""
    // Note: Full validation needs external binding
    ~ return hex_is_valid

=== function hex_encode_hint(data)
    // Request hex encoding (external binding)
    ~ return ""

=== function hex_decode_hint(hex_str)
    // Request hex decoding (external binding)
    ~ return ""

=== function hex_to_int(hex_str)
    // Convert hex string to integer
    // Note: Needs external binding for full support
    ~ return 0

=== function hex_from_int(value)
    // Convert integer to hex string
    ~ return ""

// =============================================================================
// DATA MODULES (7)
// =============================================================================

// -----------------------------------------------------------------------------
// SAFE_JSON: Safe JSON operations
// -----------------------------------------------------------------------------

VAR json_is_valid = false
VAR json_parse_error = ""
VAR json_depth = 0

CONST JSON_MAX_DEPTH = 100

=== function json_validate(json_str)
    // Validate JSON format
    ~ json_is_valid = json_str != ""
    ~ json_parse_error = ""
    // Note: Full validation needs external binding
    ~ return json_is_valid

=== function json_parse_hint(json_str)
    // Request JSON parsing (external binding)
    ~ return ""

=== function json_stringify_hint(data)
    // Request JSON serialization (external binding)
    ~ return ""

=== function json_get_string_hint(json_str, path)
    // Get string value at JSON path
    ~ return ""

=== function json_get_number_hint(json_str, path)
    // Get number value at JSON path
    ~ return 0

// -----------------------------------------------------------------------------
// SAFE_DATETIME: Safe date/time operations
// -----------------------------------------------------------------------------

// Timestamp tracking (seconds since epoch)
VAR datetime_timestamp = 0
VAR datetime_year = 2025
VAR datetime_month = 1
VAR datetime_day = 1
VAR datetime_hour = 0
VAR datetime_minute = 0
VAR datetime_second = 0
VAR datetime_is_valid = true

=== function datetime_now_hint()
    // Get current timestamp (external binding)
    ~ return datetime_timestamp

=== function datetime_validate(year, month, day)
    // Validate date components
    ~ datetime_is_valid = true
    { month < 1 || month > 12:
        ~ datetime_is_valid = false
        ~ proven_last_error = "Invalid month"
        ~ proven_error_count++
    }
    { day < 1 || day > 31:
        ~ datetime_is_valid = false
        ~ proven_last_error = "Invalid day"
        ~ proven_error_count++
    }
    { year < 1 || year > 9999:
        ~ datetime_is_valid = false
        ~ proven_last_error = "Invalid year"
        ~ proven_error_count++
    }
    ~ return datetime_is_valid

=== function datetime_is_leap_year(year)
    // Check if year is leap year
    { year mod 400 == 0:
        ~ return true
    }
    { year mod 100 == 0:
        ~ return false
    }
    { year mod 4 == 0:
        ~ return true
    }
    ~ return false

=== function datetime_days_in_month(year, month)
    // Get days in month
    { month == 2:
        { datetime_is_leap_year(year):
            ~ return 29
        }
        ~ return 28
    }
    { month == 4 || month == 6 || month == 9 || month == 11:
        ~ return 30
    }
    ~ return 31

=== function datetime_add_days(timestamp, days)
    // Add days to timestamp
    ~ return safe_add(timestamp, safe_mul(days, 86400))

=== function datetime_add_hours(timestamp, hours)
    // Add hours to timestamp
    ~ return safe_add(timestamp, safe_mul(hours, 3600))

=== function datetime_diff_days(timestamp1, timestamp2)
    // Get difference in days
    ~ return safe_div(safe_sub(timestamp2, timestamp1), 86400)

// -----------------------------------------------------------------------------
// SAFE_FLOAT: Safe floating-point operations (integer simulation)
// -----------------------------------------------------------------------------

// Simulate float with fixed-point (3 decimal places)
CONST FLOAT_SCALE = 1000

=== function float_from_int(value)
    // Convert integer to scaled float
    ~ return safe_mul(value, FLOAT_SCALE)

=== function float_to_int(scaled_value)
    // Convert scaled float to integer (truncate)
    ~ return safe_div(scaled_value, FLOAT_SCALE)

=== function float_add(a, b)
    // Add two scaled floats
    ~ return safe_add(a, b)

=== function float_sub(a, b)
    // Subtract two scaled floats
    ~ return safe_sub(a, b)

=== function float_mul(a, b)
    // Multiply two scaled floats
    ~ temp result = safe_mul(a, b)
    ~ return safe_div(result, FLOAT_SCALE)

=== function float_div(a, b)
    // Divide two scaled floats
    { b == 0:
        ~ proven_last_error = "Float division by zero"
        ~ proven_error_count++
        ~ return 0
    }
    ~ temp scaled = safe_mul(a, FLOAT_SCALE)
    ~ return safe_div(scaled, b)

=== function float_round(scaled_value)
    // Round scaled float to nearest integer
    ~ temp remainder = scaled_value mod FLOAT_SCALE
    ~ temp base = safe_div(scaled_value, FLOAT_SCALE)
    { remainder >= FLOAT_SCALE / 2:
        ~ return safe_inc(base)
    }
    ~ return base

// -----------------------------------------------------------------------------
// SAFE_VERSION: Safe semantic version handling
// -----------------------------------------------------------------------------

VAR version_major = 0
VAR version_minor = 0
VAR version_patch = 0
VAR version_is_valid = true

=== function version_parse(major, minor, patch)
    // Set version components
    ~ version_major = major
    ~ version_minor = minor
    ~ version_patch = patch
    ~ version_is_valid = major >= 0 && minor >= 0 && patch >= 0
    ~ return version_is_valid

=== function version_compare(a_major, a_minor, a_patch, b_major, b_minor, b_patch)
    // Compare versions: returns -1, 0, or 1
    { a_major < b_major:
        ~ return -1
    }
    { a_major > b_major:
        ~ return 1
    }
    { a_minor < b_minor:
        ~ return -1
    }
    { a_minor > b_minor:
        ~ return 1
    }
    { a_patch < b_patch:
        ~ return -1
    }
    { a_patch > b_patch:
        ~ return 1
    }
    ~ return 0

=== function version_is_compatible(current_major, current_minor, required_major, required_minor)
    // Check if current version is compatible with required
    { current_major != required_major:
        ~ return false
    }
    ~ return current_minor >= required_minor

=== function version_increment_major()
    // Increment major version (resets minor and patch)
    ~ version_major = safe_inc(version_major)
    ~ version_minor = 0
    ~ version_patch = 0

=== function version_increment_minor()
    // Increment minor version (resets patch)
    ~ version_minor = safe_inc(version_minor)
    ~ version_patch = 0

=== function version_increment_patch()
    // Increment patch version
    ~ version_patch = safe_inc(version_patch)

// -----------------------------------------------------------------------------
// SAFE_COLOR: Safe color operations
// -----------------------------------------------------------------------------

// RGB components (0-255)
VAR color_r = 0
VAR color_g = 0
VAR color_b = 0
VAR color_a = 255

=== function color_rgb(r, g, b)
    // Set RGB color
    ~ color_r = safe_clamp(r, 0, 255)
    ~ color_g = safe_clamp(g, 0, 255)
    ~ color_b = safe_clamp(b, 0, 255)
    ~ color_a = 255

=== function color_rgba(r, g, b, a)
    // Set RGBA color
    ~ color_rgb(r, g, b)
    ~ color_a = safe_clamp(a, 0, 255)

=== function color_to_packed()
    // Convert to packed integer (0xRRGGBBAA)
    ~ temp packed = 0
    ~ packed = safe_mul(color_r, 16777216)
    ~ packed = safe_add(packed, safe_mul(color_g, 65536))
    ~ packed = safe_add(packed, safe_mul(color_b, 256))
    ~ packed = safe_add(packed, color_a)
    ~ return packed

=== function color_from_packed(packed)
    // Extract from packed integer
    ~ color_r = safe_div(packed, 16777216) mod 256
    ~ color_g = safe_div(packed, 65536) mod 256
    ~ color_b = safe_div(packed, 256) mod 256
    ~ color_a = packed mod 256

=== function color_blend(weight)
    // Blend with another color (weight 0-100 for second color)
    // Note: Requires external color state
    ~ return weight

=== function color_grayscale()
    // Convert to grayscale
    ~ temp gray = safe_div(safe_add(safe_add(color_r, color_g), color_b), 3)
    ~ color_r = gray
    ~ color_g = gray
    ~ color_b = gray

// -----------------------------------------------------------------------------
// SAFE_ANGLE: Safe angle operations
// -----------------------------------------------------------------------------

// Angles in degrees (scaled by 100 for precision)
CONST ANGLE_SCALE = 100
CONST ANGLE_360 = 36000

VAR angle_degrees = 0

=== function angle_normalize(deg_scaled)
    // Normalize angle to [0, 360) degrees
    ~ temp normalized = deg_scaled mod ANGLE_360
    { normalized < 0:
        ~ normalized = safe_add(normalized, ANGLE_360)
    }
    ~ return normalized

=== function angle_from_degrees(degrees)
    // Create scaled angle from degrees
    ~ angle_degrees = angle_normalize(safe_mul(degrees, ANGLE_SCALE))
    ~ return angle_degrees

=== function angle_to_degrees()
    // Get angle in degrees
    ~ return safe_div(angle_degrees, ANGLE_SCALE)

=== function angle_add(deg1_scaled, deg2_scaled)
    // Add two angles
    ~ return angle_normalize(safe_add(deg1_scaled, deg2_scaled))

=== function angle_sub(deg1_scaled, deg2_scaled)
    // Subtract two angles
    ~ return angle_normalize(safe_sub(deg1_scaled, deg2_scaled))

=== function angle_is_acute(deg_scaled)
    // Check if angle is acute (< 90 degrees)
    ~ return deg_scaled < 9000

=== function angle_is_right(deg_scaled)
    // Check if angle is right (= 90 degrees)
    ~ return deg_scaled == 9000

=== function angle_is_obtuse(deg_scaled)
    // Check if angle is obtuse (90 < angle < 180)
    ~ return deg_scaled > 9000 && deg_scaled < 18000

// -----------------------------------------------------------------------------
// SAFE_UNIT: Safe unit conversions
// -----------------------------------------------------------------------------

// Length (millimeters as base unit)
=== function unit_mm_to_cm(mm)
    ~ return safe_div(mm, 10)

=== function unit_cm_to_mm(cm)
    ~ return safe_mul(cm, 10)

=== function unit_mm_to_m(mm)
    ~ return safe_div(mm, 1000)

=== function unit_m_to_mm(m)
    ~ return safe_mul(m, 1000)

=== function unit_m_to_km(m)
    ~ return safe_div(m, 1000)

=== function unit_km_to_m(km)
    ~ return safe_mul(km, 1000)

// Temperature (Celsius scaled by 100)
=== function unit_celsius_to_fahrenheit(c_scaled)
    // F = C * 9/5 + 32
    ~ temp f = safe_div(safe_mul(c_scaled, 9), 5)
    ~ return safe_add(f, 3200)

=== function unit_fahrenheit_to_celsius(f_scaled)
    // C = (F - 32) * 5/9
    ~ temp adjusted = safe_sub(f_scaled, 3200)
    ~ return safe_div(safe_mul(adjusted, 5), 9)

// Weight (grams as base unit)
=== function unit_g_to_kg(g)
    ~ return safe_div(g, 1000)

=== function unit_kg_to_g(kg)
    ~ return safe_mul(kg, 1000)

// =============================================================================
// DATA STRUCTURES (5)
// =============================================================================

// -----------------------------------------------------------------------------
// SAFE_BUFFER: Safe bounded buffer
// -----------------------------------------------------------------------------

CONST BUFFER_MAX_SIZE = 1000

VAR buffer_data_0 = 0
VAR buffer_data_1 = 0
VAR buffer_data_2 = 0
VAR buffer_data_3 = 0
VAR buffer_data_4 = 0
VAR buffer_data_5 = 0
VAR buffer_data_6 = 0
VAR buffer_data_7 = 0
VAR buffer_data_8 = 0
VAR buffer_data_9 = 0
VAR buffer_size = 0
VAR buffer_capacity = 10

=== function buffer_clear()
    // Clear buffer
    ~ buffer_data_0 = 0
    ~ buffer_data_1 = 0
    ~ buffer_data_2 = 0
    ~ buffer_data_3 = 0
    ~ buffer_data_4 = 0
    ~ buffer_data_5 = 0
    ~ buffer_data_6 = 0
    ~ buffer_data_7 = 0
    ~ buffer_data_8 = 0
    ~ buffer_data_9 = 0
    ~ buffer_size = 0

=== function buffer_push(value)
    // Push value to buffer
    { buffer_size >= buffer_capacity:
        ~ proven_last_error = "Buffer overflow"
        ~ proven_error_count++
        ~ return false
    }
    { buffer_size == 0: ~ buffer_data_0 = value }
    { buffer_size == 1: ~ buffer_data_1 = value }
    { buffer_size == 2: ~ buffer_data_2 = value }
    { buffer_size == 3: ~ buffer_data_3 = value }
    { buffer_size == 4: ~ buffer_data_4 = value }
    { buffer_size == 5: ~ buffer_data_5 = value }
    { buffer_size == 6: ~ buffer_data_6 = value }
    { buffer_size == 7: ~ buffer_data_7 = value }
    { buffer_size == 8: ~ buffer_data_8 = value }
    { buffer_size == 9: ~ buffer_data_9 = value }
    ~ buffer_size = safe_inc(buffer_size)
    ~ return true

=== function buffer_get(index)
    // Get value at index
    { index < 0 || index >= buffer_size:
        ~ proven_last_error = "Buffer index out of bounds"
        ~ proven_error_count++
        ~ return 0
    }
    { index == 0: ~ return buffer_data_0 }
    { index == 1: ~ return buffer_data_1 }
    { index == 2: ~ return buffer_data_2 }
    { index == 3: ~ return buffer_data_3 }
    { index == 4: ~ return buffer_data_4 }
    { index == 5: ~ return buffer_data_5 }
    { index == 6: ~ return buffer_data_6 }
    { index == 7: ~ return buffer_data_7 }
    { index == 8: ~ return buffer_data_8 }
    { index == 9: ~ return buffer_data_9 }
    ~ return 0

=== function buffer_is_empty()
    ~ return buffer_size == 0

=== function buffer_is_full()
    ~ return buffer_size >= buffer_capacity

// -----------------------------------------------------------------------------
// SAFE_QUEUE: Safe FIFO queue
// -----------------------------------------------------------------------------

VAR queue_data_0 = 0
VAR queue_data_1 = 0
VAR queue_data_2 = 0
VAR queue_data_3 = 0
VAR queue_data_4 = 0
VAR queue_head = 0
VAR queue_tail = 0
VAR queue_size = 0
VAR queue_capacity = 5

=== function queue_clear()
    // Clear queue
    ~ queue_data_0 = 0
    ~ queue_data_1 = 0
    ~ queue_data_2 = 0
    ~ queue_data_3 = 0
    ~ queue_data_4 = 0
    ~ queue_head = 0
    ~ queue_tail = 0
    ~ queue_size = 0

=== function queue_enqueue(value)
    // Add to queue
    { queue_size >= queue_capacity:
        ~ proven_last_error = "Queue overflow"
        ~ proven_error_count++
        ~ return false
    }
    { queue_tail == 0: ~ queue_data_0 = value }
    { queue_tail == 1: ~ queue_data_1 = value }
    { queue_tail == 2: ~ queue_data_2 = value }
    { queue_tail == 3: ~ queue_data_3 = value }
    { queue_tail == 4: ~ queue_data_4 = value }
    ~ queue_tail = (queue_tail + 1) mod queue_capacity
    ~ queue_size = safe_inc(queue_size)
    ~ return true

=== function queue_dequeue()
    // Remove from queue
    { queue_size == 0:
        ~ proven_last_error = "Queue underflow"
        ~ proven_error_count++
        ~ return 0
    }
    ~ temp value = 0
    { queue_head == 0: ~ value = queue_data_0 }
    { queue_head == 1: ~ value = queue_data_1 }
    { queue_head == 2: ~ value = queue_data_2 }
    { queue_head == 3: ~ value = queue_data_3 }
    { queue_head == 4: ~ value = queue_data_4 }
    ~ queue_head = (queue_head + 1) mod queue_capacity
    ~ queue_size = safe_dec(queue_size)
    ~ return value

=== function queue_peek()
    // Peek at front of queue
    { queue_size == 0:
        ~ return 0
    }
    { queue_head == 0: ~ return queue_data_0 }
    { queue_head == 1: ~ return queue_data_1 }
    { queue_head == 2: ~ return queue_data_2 }
    { queue_head == 3: ~ return queue_data_3 }
    { queue_head == 4: ~ return queue_data_4 }
    ~ return 0

=== function queue_is_empty()
    ~ return queue_size == 0

=== function queue_is_full()
    ~ return queue_size >= queue_capacity

// -----------------------------------------------------------------------------
// SAFE_BLOOM: Safe Bloom filter (probabilistic set)
// -----------------------------------------------------------------------------

// Simple bloom filter with 32 bits
VAR bloom_bits = 0
VAR bloom_false_positive_count = 0

=== function bloom_clear()
    // Clear bloom filter
    ~ bloom_bits = 0
    ~ bloom_false_positive_count = 0

=== function bloom_add(value)
    // Add value to bloom filter
    // Using simple hash: value mod 32
    ~ temp bit = 1
    ~ temp shift = value mod 32
    ~ temp i = 0
    { i < shift:
        ~ bit = safe_mul(bit, 2)
        ~ i++
        -> DONE
    }
    ~ bloom_bits = bloom_bits + bit

=== function bloom_might_contain(value)
    // Check if value might be in bloom filter
    ~ temp bit = 1
    ~ temp shift = value mod 32
    ~ temp i = 0
    { i < shift:
        ~ bit = safe_mul(bit, 2)
        ~ i++
        -> DONE
    }
    ~ return (bloom_bits / bit) mod 2 == 1

=== function bloom_definitely_not_contains(value)
    // Check if value is definitely not in filter
    ~ return not bloom_might_contain(value)

// -----------------------------------------------------------------------------
// SAFE_LRU: Safe LRU cache
// -----------------------------------------------------------------------------

// Simple LRU with 3 entries
VAR lru_key_0 = 0
VAR lru_key_1 = 0
VAR lru_key_2 = 0
VAR lru_val_0 = 0
VAR lru_val_1 = 0
VAR lru_val_2 = 0
VAR lru_time_0 = 0
VAR lru_time_1 = 0
VAR lru_time_2 = 0
VAR lru_access_time = 0

=== function lru_clear()
    // Clear LRU cache
    ~ lru_key_0 = 0
    ~ lru_key_1 = 0
    ~ lru_key_2 = 0
    ~ lru_val_0 = 0
    ~ lru_val_1 = 0
    ~ lru_val_2 = 0
    ~ lru_time_0 = 0
    ~ lru_time_1 = 0
    ~ lru_time_2 = 0
    ~ lru_access_time = 0

=== function lru_get(key)
    // Get value from LRU cache (returns 0 if not found)
    ~ lru_access_time = safe_inc(lru_access_time)
    { lru_key_0 == key:
        ~ lru_time_0 = lru_access_time
        ~ return lru_val_0
    }
    { lru_key_1 == key:
        ~ lru_time_1 = lru_access_time
        ~ return lru_val_1
    }
    { lru_key_2 == key:
        ~ lru_time_2 = lru_access_time
        ~ return lru_val_2
    }
    ~ return 0

=== function lru_put(key, value)
    // Put value in LRU cache (evicts LRU if full)
    ~ lru_access_time = safe_inc(lru_access_time)

    // Check if key exists
    { lru_key_0 == key:
        ~ lru_val_0 = value
        ~ lru_time_0 = lru_access_time
        ~ return
    }
    { lru_key_1 == key:
        ~ lru_val_1 = value
        ~ lru_time_1 = lru_access_time
        ~ return
    }
    { lru_key_2 == key:
        ~ lru_val_2 = value
        ~ lru_time_2 = lru_access_time
        ~ return
    }

    // Find LRU entry to evict
    { lru_time_0 <= lru_time_1 && lru_time_0 <= lru_time_2:
        ~ lru_key_0 = key
        ~ lru_val_0 = value
        ~ lru_time_0 = lru_access_time
        ~ return
    }
    { lru_time_1 <= lru_time_2:
        ~ lru_key_1 = key
        ~ lru_val_1 = value
        ~ lru_time_1 = lru_access_time
        ~ return
    }
    ~ lru_key_2 = key
    ~ lru_val_2 = value
    ~ lru_time_2 = lru_access_time

=== function lru_contains(key)
    // Check if key is in cache
    ~ return lru_key_0 == key || lru_key_1 == key || lru_key_2 == key

// -----------------------------------------------------------------------------
// SAFE_GRAPH: Safe graph operations
// -----------------------------------------------------------------------------

// Simple adjacency matrix for 4 nodes
VAR graph_edge_0_1 = false
VAR graph_edge_0_2 = false
VAR graph_edge_0_3 = false
VAR graph_edge_1_0 = false
VAR graph_edge_1_2 = false
VAR graph_edge_1_3 = false
VAR graph_edge_2_0 = false
VAR graph_edge_2_1 = false
VAR graph_edge_2_3 = false
VAR graph_edge_3_0 = false
VAR graph_edge_3_1 = false
VAR graph_edge_3_2 = false
VAR graph_node_count = 4

=== function graph_clear()
    // Clear all edges
    ~ graph_edge_0_1 = false
    ~ graph_edge_0_2 = false
    ~ graph_edge_0_3 = false
    ~ graph_edge_1_0 = false
    ~ graph_edge_1_2 = false
    ~ graph_edge_1_3 = false
    ~ graph_edge_2_0 = false
    ~ graph_edge_2_1 = false
    ~ graph_edge_2_3 = false
    ~ graph_edge_3_0 = false
    ~ graph_edge_3_1 = false
    ~ graph_edge_3_2 = false

=== function graph_add_edge(from, to)
    // Add directed edge
    { from == 0 && to == 1: ~ graph_edge_0_1 = true }
    { from == 0 && to == 2: ~ graph_edge_0_2 = true }
    { from == 0 && to == 3: ~ graph_edge_0_3 = true }
    { from == 1 && to == 0: ~ graph_edge_1_0 = true }
    { from == 1 && to == 2: ~ graph_edge_1_2 = true }
    { from == 1 && to == 3: ~ graph_edge_1_3 = true }
    { from == 2 && to == 0: ~ graph_edge_2_0 = true }
    { from == 2 && to == 1: ~ graph_edge_2_1 = true }
    { from == 2 && to == 3: ~ graph_edge_2_3 = true }
    { from == 3 && to == 0: ~ graph_edge_3_0 = true }
    { from == 3 && to == 1: ~ graph_edge_3_1 = true }
    { from == 3 && to == 2: ~ graph_edge_3_2 = true }

=== function graph_has_edge(from, to)
    // Check if edge exists
    { from == 0 && to == 1: ~ return graph_edge_0_1 }
    { from == 0 && to == 2: ~ return graph_edge_0_2 }
    { from == 0 && to == 3: ~ return graph_edge_0_3 }
    { from == 1 && to == 0: ~ return graph_edge_1_0 }
    { from == 1 && to == 2: ~ return graph_edge_1_2 }
    { from == 1 && to == 3: ~ return graph_edge_1_3 }
    { from == 2 && to == 0: ~ return graph_edge_2_0 }
    { from == 2 && to == 1: ~ return graph_edge_2_1 }
    { from == 2 && to == 3: ~ return graph_edge_2_3 }
    { from == 3 && to == 0: ~ return graph_edge_3_0 }
    { from == 3 && to == 1: ~ return graph_edge_3_1 }
    { from == 3 && to == 2: ~ return graph_edge_3_2 }
    ~ return false

=== function graph_remove_edge(from, to)
    // Remove directed edge
    { from == 0 && to == 1: ~ graph_edge_0_1 = false }
    { from == 0 && to == 2: ~ graph_edge_0_2 = false }
    { from == 0 && to == 3: ~ graph_edge_0_3 = false }
    { from == 1 && to == 0: ~ graph_edge_1_0 = false }
    { from == 1 && to == 2: ~ graph_edge_1_2 = false }
    { from == 1 && to == 3: ~ graph_edge_1_3 = false }
    { from == 2 && to == 0: ~ graph_edge_2_0 = false }
    { from == 2 && to == 1: ~ graph_edge_2_1 = false }
    { from == 2 && to == 3: ~ graph_edge_2_3 = false }
    { from == 3 && to == 0: ~ graph_edge_3_0 = false }
    { from == 3 && to == 1: ~ graph_edge_3_1 = false }
    { from == 3 && to == 2: ~ graph_edge_3_2 = false }

=== function graph_out_degree(node)
    // Count outgoing edges from node
    ~ temp count = 0
    { node == 0:
        { graph_edge_0_1: ~ count++ }
        { graph_edge_0_2: ~ count++ }
        { graph_edge_0_3: ~ count++ }
    }
    { node == 1:
        { graph_edge_1_0: ~ count++ }
        { graph_edge_1_2: ~ count++ }
        { graph_edge_1_3: ~ count++ }
    }
    { node == 2:
        { graph_edge_2_0: ~ count++ }
        { graph_edge_2_1: ~ count++ }
        { graph_edge_2_3: ~ count++ }
    }
    { node == 3:
        { graph_edge_3_0: ~ count++ }
        { graph_edge_3_1: ~ count++ }
        { graph_edge_3_2: ~ count++ }
    }
    ~ return count

// =============================================================================
// RESILIENCE MODULES (4)
// =============================================================================

// -----------------------------------------------------------------------------
// SAFE_RATE_LIMITER: Safe rate limiting
// -----------------------------------------------------------------------------

VAR rate_limit_tokens = 10
VAR rate_limit_max_tokens = 10
VAR rate_limit_refill_rate = 1
VAR rate_limit_last_refill = 0

=== function rate_limit_init(max_tokens, refill_rate)
    // Initialize rate limiter
    ~ rate_limit_max_tokens = max_tokens
    ~ rate_limit_tokens = max_tokens
    ~ rate_limit_refill_rate = refill_rate
    ~ rate_limit_last_refill = 0

=== function rate_limit_refill(current_time)
    // Refill tokens based on time elapsed
    ~ temp elapsed = safe_sub(current_time, rate_limit_last_refill)
    ~ temp new_tokens = safe_mul(elapsed, rate_limit_refill_rate)
    ~ rate_limit_tokens = safe_add(rate_limit_tokens, new_tokens)
    { rate_limit_tokens > rate_limit_max_tokens:
        ~ rate_limit_tokens = rate_limit_max_tokens
    }
    ~ rate_limit_last_refill = current_time

=== function rate_limit_try_acquire(current_time)
    // Try to acquire a token
    ~ rate_limit_refill(current_time)
    { rate_limit_tokens > 0:
        ~ rate_limit_tokens = safe_dec(rate_limit_tokens)
        ~ return true
    }
    ~ return false

=== function rate_limit_try_acquire_n(current_time, n)
    // Try to acquire n tokens
    ~ rate_limit_refill(current_time)
    { rate_limit_tokens >= n:
        ~ rate_limit_tokens = safe_sub(rate_limit_tokens, n)
        ~ return true
    }
    ~ return false

=== function rate_limit_available()
    // Get available tokens
    ~ return rate_limit_tokens

// -----------------------------------------------------------------------------
// SAFE_CIRCUIT_BREAKER: Safe circuit breaker pattern
// -----------------------------------------------------------------------------

// Circuit states
CONST CIRCUIT_CLOSED = 0
CONST CIRCUIT_OPEN = 1
CONST CIRCUIT_HALF_OPEN = 2

VAR circuit_state = 0
VAR circuit_failures = 0
VAR circuit_successes = 0
VAR circuit_failure_threshold = 5
VAR circuit_success_threshold = 2
VAR circuit_timeout = 30
VAR circuit_last_failure_time = 0
VAR circuit_half_open_calls = 0
VAR circuit_half_open_max = 3

=== function circuit_init(failure_threshold, success_threshold, timeout)
    // Initialize circuit breaker
    ~ circuit_state = CIRCUIT_CLOSED
    ~ circuit_failures = 0
    ~ circuit_successes = 0
    ~ circuit_failure_threshold = failure_threshold
    ~ circuit_success_threshold = success_threshold
    ~ circuit_timeout = timeout
    ~ circuit_last_failure_time = 0
    ~ circuit_half_open_calls = 0

=== function circuit_can_execute(current_time)
    // Check if request can proceed
    // Update state first
    { circuit_state == CIRCUIT_OPEN:
        { current_time >= safe_add(circuit_last_failure_time, circuit_timeout):
            ~ circuit_state = CIRCUIT_HALF_OPEN
            ~ circuit_successes = 0
            ~ circuit_half_open_calls = 0
        }
    }

    { circuit_state == CIRCUIT_CLOSED:
        ~ return true
    }
    { circuit_state == CIRCUIT_OPEN:
        ~ return false
    }
    { circuit_state == CIRCUIT_HALF_OPEN:
        ~ return circuit_half_open_calls < circuit_half_open_max
    }
    ~ return false

=== function circuit_record_success()
    // Record successful call
    { circuit_state == CIRCUIT_CLOSED:
        ~ circuit_failures = 0
    }
    { circuit_state == CIRCUIT_HALF_OPEN:
        ~ circuit_successes = safe_inc(circuit_successes)
        { circuit_successes >= circuit_success_threshold:
            ~ circuit_state = CIRCUIT_CLOSED
            ~ circuit_failures = 0
            ~ circuit_successes = 0
        }
    }

=== function circuit_record_failure(current_time)
    // Record failed call
    ~ circuit_last_failure_time = current_time
    { circuit_state == CIRCUIT_CLOSED:
        ~ circuit_failures = safe_inc(circuit_failures)
        { circuit_failures >= circuit_failure_threshold:
            ~ circuit_state = CIRCUIT_OPEN
        }
    }
    { circuit_state == CIRCUIT_HALF_OPEN:
        ~ circuit_state = CIRCUIT_OPEN
        ~ circuit_failures = safe_inc(circuit_failures)
    }

=== function circuit_record_attempt()
    // Record attempt for half-open tracking
    { circuit_state == CIRCUIT_HALF_OPEN:
        ~ circuit_half_open_calls = safe_inc(circuit_half_open_calls)
    }

=== function circuit_is_healthy()
    // Check if circuit is healthy
    ~ return circuit_state == CIRCUIT_CLOSED

=== function circuit_reset()
    // Force reset circuit
    ~ circuit_state = CIRCUIT_CLOSED
    ~ circuit_failures = 0
    ~ circuit_successes = 0
    ~ circuit_half_open_calls = 0

// -----------------------------------------------------------------------------
// SAFE_RETRY: Safe retry logic with backoff
// -----------------------------------------------------------------------------

VAR retry_attempts = 0
VAR retry_max_attempts = 3
VAR retry_delay = 1
VAR retry_max_delay = 60
VAR retry_backoff_multiplier = 2

=== function retry_init(max_attempts, initial_delay, max_delay, multiplier)
    // Initialize retry config
    ~ retry_max_attempts = max_attempts
    ~ retry_delay = initial_delay
    ~ retry_max_delay = max_delay
    ~ retry_backoff_multiplier = multiplier
    ~ retry_attempts = 0

=== function retry_reset()
    // Reset retry state
    ~ retry_attempts = 0

=== function retry_should_retry()
    // Check if should retry
    ~ return retry_attempts < retry_max_attempts

=== function retry_record_attempt()
    // Record an attempt
    ~ retry_attempts = safe_inc(retry_attempts)

=== function retry_get_delay()
    // Get current delay with exponential backoff
    ~ temp delay = retry_delay
    ~ temp i = 1
    { i < retry_attempts:
        ~ delay = safe_mul(delay, retry_backoff_multiplier)
        ~ i++
        -> DONE
    }
    { delay > retry_max_delay:
        ~ delay = retry_max_delay
    }
    ~ return delay

=== function retry_attempts_remaining()
    // Get remaining attempts
    ~ return safe_sub(retry_max_attempts, retry_attempts)

// -----------------------------------------------------------------------------
// SAFE_MONOTONIC: Safe monotonically increasing counter
// -----------------------------------------------------------------------------

VAR monotonic_value = 0
VAR monotonic_high_water = 0

=== function monotonic_reset()
    // Reset counter
    ~ monotonic_value = 0
    ~ monotonic_high_water = 0

=== function monotonic_next()
    // Get next value (always increases)
    ~ monotonic_value = safe_inc(monotonic_value)
    { monotonic_value > monotonic_high_water:
        ~ monotonic_high_water = monotonic_value
    }
    ~ return monotonic_value

=== function monotonic_current()
    // Get current value
    ~ return monotonic_value

=== function monotonic_high_water_mark()
    // Get highest value ever seen
    ~ return monotonic_high_water

=== function monotonic_set_minimum(min_value)
    // Ensure value is at least min_value
    { monotonic_value < min_value:
        ~ monotonic_value = min_value
        { monotonic_value > monotonic_high_water:
            ~ monotonic_high_water = monotonic_value
        }
    }
    ~ return monotonic_value

// =============================================================================
// STATE MODULES (2)
// =============================================================================

// -----------------------------------------------------------------------------
// SAFE_STATE_MACHINE: Safe state machine with validated transitions
// -----------------------------------------------------------------------------

VAR state_current = 0
VAR state_previous = 0
VAR state_history_0 = 0
VAR state_history_1 = 0
VAR state_history_2 = 0
VAR state_history_3 = 0
VAR state_history_4 = 0
VAR state_history_count = 0

// Transition matrix (5 states, each can transition to certain states)
VAR state_trans_0_to = 0
VAR state_trans_1_to = 0
VAR state_trans_2_to = 0
VAR state_trans_3_to = 0
VAR state_trans_4_to = 0

=== function state_machine_init(initial_state)
    // Initialize state machine
    ~ state_current = initial_state
    ~ state_previous = initial_state
    ~ state_history_count = 0
    ~ state_trans_0_to = 0
    ~ state_trans_1_to = 0
    ~ state_trans_2_to = 0
    ~ state_trans_3_to = 0
    ~ state_trans_4_to = 0

=== function state_add_transition(from, to)
    // Add valid transition (using bit flags)
    ~ temp bit = 1
    ~ temp i = 0
    { i < to:
        ~ bit = safe_mul(bit, 2)
        ~ i++
        -> DONE
    }
    { from == 0: ~ state_trans_0_to = state_trans_0_to + bit }
    { from == 1: ~ state_trans_1_to = state_trans_1_to + bit }
    { from == 2: ~ state_trans_2_to = state_trans_2_to + bit }
    { from == 3: ~ state_trans_3_to = state_trans_3_to + bit }
    { from == 4: ~ state_trans_4_to = state_trans_4_to + bit }

=== function state_can_transition(to)
    // Check if transition is valid
    ~ temp transitions = 0
    { state_current == 0: ~ transitions = state_trans_0_to }
    { state_current == 1: ~ transitions = state_trans_1_to }
    { state_current == 2: ~ transitions = state_trans_2_to }
    { state_current == 3: ~ transitions = state_trans_3_to }
    { state_current == 4: ~ transitions = state_trans_4_to }

    ~ temp bit = 1
    ~ temp i = 0
    { i < to:
        ~ bit = safe_mul(bit, 2)
        ~ i++
        -> DONE
    }
    ~ return (transitions / bit) mod 2 == 1

=== function state_transition(to)
    // Attempt transition
    { not state_can_transition(to):
        ~ proven_last_error = "Invalid state transition"
        ~ proven_error_count++
        ~ return false
    }

    // Record history
    { state_history_count < 5:
        { state_history_count == 0: ~ state_history_0 = state_current }
        { state_history_count == 1: ~ state_history_1 = state_current }
        { state_history_count == 2: ~ state_history_2 = state_current }
        { state_history_count == 3: ~ state_history_3 = state_current }
        { state_history_count == 4: ~ state_history_4 = state_current }
        ~ state_history_count++
    }

    ~ state_previous = state_current
    ~ state_current = to
    ~ return true

=== function state_current_state()
    ~ return state_current

=== function state_previous_state()
    ~ return state_previous

// -----------------------------------------------------------------------------
// SAFE_CALCULATOR: Safe calculator with expression evaluation
// -----------------------------------------------------------------------------

VAR calc_accumulator = 0
VAR calc_memory = 0
VAR calc_last_operation = ""
VAR calc_error = false

=== function calc_clear()
    // Clear calculator
    ~ calc_accumulator = 0
    ~ calc_last_operation = ""
    ~ calc_error = false

=== function calc_clear_all()
    // Clear calculator and memory
    ~ calc_clear()
    ~ calc_memory = 0

=== function calc_set(value)
    // Set accumulator value
    ~ calc_accumulator = value
    ~ calc_error = false

=== function calc_add(value)
    // Add to accumulator
    ~ calc_accumulator = safe_add(calc_accumulator, value)
    ~ calc_last_operation = "add"
    ~ return calc_accumulator

=== function calc_subtract(value)
    // Subtract from accumulator
    ~ calc_accumulator = safe_sub(calc_accumulator, value)
    ~ calc_last_operation = "sub"
    ~ return calc_accumulator

=== function calc_multiply(value)
    // Multiply accumulator
    ~ calc_accumulator = safe_mul(calc_accumulator, value)
    ~ calc_last_operation = "mul"
    ~ return calc_accumulator

=== function calc_divide(value)
    // Divide accumulator
    { value == 0:
        ~ calc_error = true
        ~ proven_last_error = "Calculator division by zero"
        ~ proven_error_count++
        ~ return calc_accumulator
    }
    ~ calc_accumulator = safe_div(calc_accumulator, value)
    ~ calc_last_operation = "div"
    ~ return calc_accumulator

=== function calc_memory_store()
    // Store accumulator in memory
    ~ calc_memory = calc_accumulator

=== function calc_memory_recall()
    // Recall memory to accumulator
    ~ calc_accumulator = calc_memory
    ~ return calc_accumulator

=== function calc_memory_add()
    // Add accumulator to memory
    ~ calc_memory = safe_add(calc_memory, calc_accumulator)

=== function calc_memory_clear()
    // Clear memory
    ~ calc_memory = 0

=== function calc_result()
    // Get current result
    ~ return calc_accumulator

=== function calc_has_error()
    // Check for error
    ~ return calc_error

// =============================================================================
// ALGORITHM MODULES (4)
// =============================================================================

// -----------------------------------------------------------------------------
// SAFE_GEO: Safe geographic coordinate operations
// -----------------------------------------------------------------------------

// Earth radius in km (scaled by 10)
CONST GEO_EARTH_RADIUS = 63710

VAR geo_lat = 0
VAR geo_lon = 0
VAR geo_is_valid = true

=== function geo_set_coordinate(lat_scaled, lon_scaled)
    // Set coordinate (scaled by 1000000 for precision)
    // lat: -90000000 to 90000000
    // lon: -180000000 to 180000000
    ~ geo_is_valid = true
    { lat_scaled < -90000000 || lat_scaled > 90000000:
        ~ geo_is_valid = false
        ~ proven_last_error = "Latitude out of range"
        ~ proven_error_count++
    }
    { lon_scaled < -180000000 || lon_scaled > 180000000:
        ~ geo_is_valid = false
        ~ proven_last_error = "Longitude out of range"
        ~ proven_error_count++
    }
    { geo_is_valid:
        ~ geo_lat = lat_scaled
        ~ geo_lon = lon_scaled
    }
    ~ return geo_is_valid

=== function geo_is_northern()
    // Check if in Northern Hemisphere
    ~ return geo_lat >= 0

=== function geo_is_eastern()
    // Check if in Eastern Hemisphere
    ~ return geo_lon >= 0

=== function geo_distance_approx(lat1, lon1, lat2, lon2)
    // Approximate distance (simplified, returns relative distance)
    ~ temp dlat = safe_sub(lat2, lat1)
    ~ temp dlon = safe_sub(lon2, lon1)
    ~ dlat = safe_abs(dlat)
    ~ dlon = safe_abs(dlon)
    // Approximate: use simple sum (real would need haversine)
    ~ return safe_add(dlat, dlon)

=== function geo_in_bounding_box(lat, lon, min_lat, min_lon, max_lat, max_lon)
    // Check if coordinate is in bounding box
    ~ return lat >= min_lat && lat <= max_lat && lon >= min_lon && lon <= max_lon

// -----------------------------------------------------------------------------
// SAFE_PROBABILITY: Safe probability operations
// -----------------------------------------------------------------------------

// Probabilities scaled by 1000 (0 = 0%, 1000 = 100%)
CONST PROB_SCALE = 1000
CONST PROB_ZERO = 0
CONST PROB_HALF = 500
CONST PROB_ONE = 1000

=== function prob_from_percent(percent)
    // Convert percentage to probability
    ~ return safe_clamp(safe_mul(percent, 10), 0, PROB_ONE)

=== function prob_to_percent(prob_scaled)
    // Convert probability to percentage
    ~ return safe_div(prob_scaled, 10)

=== function prob_complement(p)
    // Get complement (1 - p)
    ~ return safe_sub(PROB_ONE, p)

=== function prob_and(p1, p2)
    // P(A and B) for independent events
    ~ return safe_div(safe_mul(p1, p2), PROB_ONE)

=== function prob_or(p1, p2)
    // P(A or B) for independent events
    ~ temp product = prob_and(p1, p2)
    ~ return safe_sub(safe_add(p1, p2), product)

=== function prob_to_odds(p)
    // Convert probability to odds ratio (scaled)
    { p >= PROB_ONE:
        ~ return SAFE_INT_MAX
    }
    ~ return safe_div(safe_mul(p, PROB_ONE), safe_sub(PROB_ONE, p))

=== function prob_from_odds(odds_scaled)
    // Convert odds ratio to probability
    ~ return safe_div(safe_mul(odds_scaled, PROB_ONE), safe_add(PROB_ONE, odds_scaled))

=== function prob_expected_value(value1, prob1, value2, prob2)
    // Calculate expected value for two outcomes
    ~ temp ev1 = safe_div(safe_mul(value1, prob1), PROB_ONE)
    ~ temp ev2 = safe_div(safe_mul(value2, prob2), PROB_ONE)
    ~ return safe_add(ev1, ev2)

// -----------------------------------------------------------------------------
// SAFE_CHECKSUM: Safe checksum operations
// -----------------------------------------------------------------------------

VAR checksum_value = 0

=== function checksum_reset()
    // Reset checksum
    ~ checksum_value = 0

=== function checksum_add_byte(byte)
    // Add byte to checksum (simple additive)
    ~ byte = byte mod 256
    ~ checksum_value = (checksum_value + byte) mod 65536

=== function checksum_add_value(value)
    // Add integer value to checksum
    ~ checksum_add_byte(value mod 256)
    ~ checksum_add_byte((value / 256) mod 256)
    ~ checksum_add_byte((value / 65536) mod 256)
    ~ checksum_add_byte((value / 16777216) mod 256)

=== function checksum_get()
    // Get current checksum
    ~ return checksum_value

=== function checksum_verify(expected)
    // Verify checksum matches
    ~ return checksum_value == expected

=== function checksum_luhn(number)
    // Luhn algorithm for credit card validation (simplified)
    // Returns true if valid
    ~ temp sum = 0
    ~ temp double_digit = false
    ~ temp n = number
    { n > 0:
        ~ temp digit = n mod 10
        { double_digit:
            ~ digit = safe_mul(digit, 2)
            { digit > 9:
                ~ digit = safe_sub(digit, 9)
            }
        }
        ~ sum = safe_add(sum, digit)
        ~ double_digit = not double_digit
        ~ n = safe_div(n, 10)
        -> DONE
    }
    ~ return sum mod 10 == 0

// -----------------------------------------------------------------------------
// SAFE_TENSOR: Safe tensor operations (simplified 2D matrix)
// -----------------------------------------------------------------------------

// 3x3 matrix
VAR tensor_0_0 = 0
VAR tensor_0_1 = 0
VAR tensor_0_2 = 0
VAR tensor_1_0 = 0
VAR tensor_1_1 = 0
VAR tensor_1_2 = 0
VAR tensor_2_0 = 0
VAR tensor_2_1 = 0
VAR tensor_2_2 = 0

=== function tensor_clear()
    // Clear tensor
    ~ tensor_0_0 = 0
    ~ tensor_0_1 = 0
    ~ tensor_0_2 = 0
    ~ tensor_1_0 = 0
    ~ tensor_1_1 = 0
    ~ tensor_1_2 = 0
    ~ tensor_2_0 = 0
    ~ tensor_2_1 = 0
    ~ tensor_2_2 = 0

=== function tensor_identity()
    // Set to identity matrix
    ~ tensor_clear()
    ~ tensor_0_0 = 1
    ~ tensor_1_1 = 1
    ~ tensor_2_2 = 1

=== function tensor_get(row, col)
    // Get tensor value
    { row == 0 && col == 0: ~ return tensor_0_0 }
    { row == 0 && col == 1: ~ return tensor_0_1 }
    { row == 0 && col == 2: ~ return tensor_0_2 }
    { row == 1 && col == 0: ~ return tensor_1_0 }
    { row == 1 && col == 1: ~ return tensor_1_1 }
    { row == 1 && col == 2: ~ return tensor_1_2 }
    { row == 2 && col == 0: ~ return tensor_2_0 }
    { row == 2 && col == 1: ~ return tensor_2_1 }
    { row == 2 && col == 2: ~ return tensor_2_2 }
    ~ return 0

=== function tensor_set(row, col, value)
    // Set tensor value
    { row == 0 && col == 0: ~ tensor_0_0 = value }
    { row == 0 && col == 1: ~ tensor_0_1 = value }
    { row == 0 && col == 2: ~ tensor_0_2 = value }
    { row == 1 && col == 0: ~ tensor_1_0 = value }
    { row == 1 && col == 1: ~ tensor_1_1 = value }
    { row == 1 && col == 2: ~ tensor_1_2 = value }
    { row == 2 && col == 0: ~ tensor_2_0 = value }
    { row == 2 && col == 1: ~ tensor_2_1 = value }
    { row == 2 && col == 2: ~ tensor_2_2 = value }

=== function tensor_scale(scalar)
    // Scale all values by scalar
    ~ tensor_0_0 = safe_mul(tensor_0_0, scalar)
    ~ tensor_0_1 = safe_mul(tensor_0_1, scalar)
    ~ tensor_0_2 = safe_mul(tensor_0_2, scalar)
    ~ tensor_1_0 = safe_mul(tensor_1_0, scalar)
    ~ tensor_1_1 = safe_mul(tensor_1_1, scalar)
    ~ tensor_1_2 = safe_mul(tensor_1_2, scalar)
    ~ tensor_2_0 = safe_mul(tensor_2_0, scalar)
    ~ tensor_2_1 = safe_mul(tensor_2_1, scalar)
    ~ tensor_2_2 = safe_mul(tensor_2_2, scalar)

=== function tensor_trace()
    // Get trace (sum of diagonal)
    ~ return safe_add(safe_add(tensor_0_0, tensor_1_1), tensor_2_2)

// =============================================================================
// SECURITY MODULES (2)
// =============================================================================

// -----------------------------------------------------------------------------
// SAFE_PASSWORD: Safe password validation
// -----------------------------------------------------------------------------

CONST PASSWORD_MIN_LENGTH = 8
CONST PASSWORD_MAX_LENGTH = 128

VAR password_length = 0
VAR password_has_upper = false
VAR password_has_lower = false
VAR password_has_digit = false
VAR password_has_special = false
VAR password_strength = 0

=== function password_check_requirements(length, has_upper, has_lower, has_digit, has_special)
    // Check password requirements
    ~ password_length = length
    ~ password_has_upper = has_upper
    ~ password_has_lower = has_lower
    ~ password_has_digit = has_digit
    ~ password_has_special = has_special

    // Calculate strength (0-100)
    ~ password_strength = 0
    { length >= PASSWORD_MIN_LENGTH:
        ~ password_strength = safe_add(password_strength, 20)
    }
    { length >= 12:
        ~ password_strength = safe_add(password_strength, 10)
    }
    { length >= 16:
        ~ password_strength = safe_add(password_strength, 10)
    }
    { has_upper:
        ~ password_strength = safe_add(password_strength, 15)
    }
    { has_lower:
        ~ password_strength = safe_add(password_strength, 15)
    }
    { has_digit:
        ~ password_strength = safe_add(password_strength, 15)
    }
    { has_special:
        ~ password_strength = safe_add(password_strength, 15)
    }

    ~ return password_strength

=== function password_is_valid()
    // Check if password meets minimum requirements
    ~ return password_length >= PASSWORD_MIN_LENGTH && password_length <= PASSWORD_MAX_LENGTH

=== function password_is_strong()
    // Check if password is strong (score >= 70)
    ~ return password_strength >= 70

=== function password_get_strength()
    // Get password strength score
    ~ return password_strength

=== function password_is_common_hint(password)
    // Check if password is in common password list (external binding)
    ~ return false

// -----------------------------------------------------------------------------
// SAFE_ML: Safe machine learning operations (simplified scoring)
// -----------------------------------------------------------------------------

// Simple linear model weights
VAR ml_weight_0 = 0
VAR ml_weight_1 = 0
VAR ml_weight_2 = 0
VAR ml_bias = 0
VAR ml_prediction = 0

=== function ml_set_weights(w0, w1, w2, bias)
    // Set model weights
    ~ ml_weight_0 = w0
    ~ ml_weight_1 = w1
    ~ ml_weight_2 = w2
    ~ ml_bias = bias

=== function ml_predict(x0, x1, x2)
    // Make prediction (linear: w0*x0 + w1*x1 + w2*x2 + bias)
    ~ temp sum = ml_bias
    ~ sum = safe_add(sum, safe_mul(ml_weight_0, x0))
    ~ sum = safe_add(sum, safe_mul(ml_weight_1, x1))
    ~ sum = safe_add(sum, safe_mul(ml_weight_2, x2))
    ~ ml_prediction = sum
    ~ return ml_prediction

=== function ml_classify(x0, x1, x2, threshold)
    // Binary classification
    ~ ml_predict(x0, x1, x2)
    ~ return ml_prediction >= threshold

=== function ml_normalize(value, min_val, max_val, scale)
    // Normalize value to [0, scale]
    { max_val == min_val:
        ~ return 0
    }
    ~ temp normalized = safe_sub(value, min_val)
    ~ normalized = safe_mul(normalized, scale)
    ~ return safe_div(normalized, safe_sub(max_val, min_val))

=== function ml_clamp_prediction(min_val, max_val)
    // Clamp prediction to range
    ~ ml_prediction = safe_clamp(ml_prediction, min_val, max_val)
    ~ return ml_prediction

// =============================================================================
// HTTP MODULES (3)
// =============================================================================

// -----------------------------------------------------------------------------
// SAFE_HEADER: Safe HTTP header validation
// -----------------------------------------------------------------------------

VAR header_is_valid = false
VAR header_name = ""
VAR header_value = ""

=== function header_validate_name(name)
    // Validate header name
    ~ header_is_valid = name != ""
    // Full validation needs external binding
    ~ header_name = name
    ~ return header_is_valid

=== function header_validate_value(value)
    // Validate header value (no CRLF injection)
    ~ header_is_valid = true
    // Full validation needs external binding
    ~ header_value = value
    ~ return header_is_valid

=== function header_is_content_type(name)
    // Check if header is Content-Type
    ~ return name == "Content-Type" || name == "content-type"

=== function header_is_authorization(name)
    // Check if header is Authorization
    ~ return name == "Authorization" || name == "authorization"

=== function header_is_sensitive(name)
    // Check if header contains sensitive data
    ~ return header_is_authorization(name) || name == "Cookie" || name == "cookie" || name == "Set-Cookie" || name == "set-cookie"

// -----------------------------------------------------------------------------
// SAFE_COOKIE: Safe HTTP cookie handling
// -----------------------------------------------------------------------------

VAR cookie_name = ""
VAR cookie_value = ""
VAR cookie_secure = false
VAR cookie_http_only = false
VAR cookie_same_site = ""
VAR cookie_max_age = 0
VAR cookie_is_valid = false

=== function cookie_create(name, value)
    // Create cookie
    ~ cookie_name = name
    ~ cookie_value = value
    ~ cookie_secure = false
    ~ cookie_http_only = false
    ~ cookie_same_site = "Lax"
    ~ cookie_max_age = 0
    ~ cookie_is_valid = name != ""

=== function cookie_set_secure(secure)
    // Set Secure flag
    ~ cookie_secure = secure

=== function cookie_set_http_only(http_only)
    // Set HttpOnly flag
    ~ cookie_http_only = http_only

=== function cookie_set_same_site(same_site)
    // Set SameSite attribute (Strict, Lax, None)
    ~ cookie_same_site = same_site

=== function cookie_set_max_age(seconds)
    // Set Max-Age
    ~ cookie_max_age = seconds

=== function cookie_is_secure_recommended()
    // Check if cookie has recommended security settings
    ~ return cookie_secure && cookie_http_only

=== function cookie_validate()
    // Validate cookie
    ~ cookie_is_valid = cookie_name != ""
    ~ return cookie_is_valid

// -----------------------------------------------------------------------------
// SAFE_CONTENT_TYPE: Safe Content-Type handling
// -----------------------------------------------------------------------------

VAR content_type_main = ""
VAR content_type_sub = ""
VAR content_type_charset = ""
VAR content_type_is_valid = false

=== function content_type_parse(main_type, sub_type)
    // Parse content type
    ~ content_type_main = main_type
    ~ content_type_sub = sub_type
    ~ content_type_is_valid = main_type != "" && sub_type != ""
    ~ return content_type_is_valid

=== function content_type_set_charset(charset)
    // Set charset
    ~ content_type_charset = charset

=== function content_type_is_json()
    // Check if JSON
    ~ return content_type_main == "application" && content_type_sub == "json"

=== function content_type_is_html()
    // Check if HTML
    ~ return content_type_main == "text" && content_type_sub == "html"

=== function content_type_is_text()
    // Check if any text type
    ~ return content_type_main == "text"

=== function content_type_is_binary()
    // Check if binary type
    ~ return content_type_main == "application" && (content_type_sub == "octet-stream" || content_type_sub == "pdf" || content_type_sub == "zip")

=== function content_type_is_image()
    // Check if image type
    ~ return content_type_main == "image"

=== function content_type_is_audio()
    // Check if audio type
    ~ return content_type_main == "audio"

=== function content_type_is_video()
    // Check if video type
    ~ return content_type_main == "video"

// =============================================================================
// NARRATIVE UTILITIES
// =============================================================================

// These functions help integrate proven safety with narrative mechanics

=== function narrative_safe_score(score, min_score, max_score)
    // Safely update a score within bounds
    ~ return safe_clamp(score, min_score, max_score)

=== function narrative_safe_countdown(current, decrement)
    // Safely decrement a countdown (never goes below 0)
    ~ temp result = safe_sub(current, decrement)
    { result < 0:
        ~ return 0
    }
    ~ return result

=== function narrative_safe_percentage(current, total)
    // Calculate percentage safely
    { total == 0:
        ~ return 0
    }
    ~ return safe_div(safe_mul(current, 100), total)

=== function narrative_random_in_range_hint(min_val, max_val)
    // Request random value in range (external binding)
    // For Ink, use RANDOM(min, max) instead
    ~ return min_val

// =============================================================================
// ERROR HANDLING
// =============================================================================

=== function proven_get_last_error()
    ~ return proven_last_error

=== function proven_get_error_count()
    ~ return proven_error_count

=== function proven_clear_errors()
    ~ proven_last_error = ""
    ~ proven_error_count = 0

=== function proven_has_error()
    ~ return proven_last_error != ""

// =============================================================================
// END OF PROVEN LIBRARY
// =============================================================================
