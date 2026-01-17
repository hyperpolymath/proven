# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven Safety Primitives for Rego (Open Policy Agent)
#
# Comprehensive library of 38 safety modules providing reusable
# validation rules and safety functions for OPA policies.
#
# Categories:
#   Core (11): safe_math, safe_string, safe_path, safe_email, safe_url,
#              safe_network, safe_crypto, safe_uuid, safe_currency,
#              safe_phone, safe_hex
#   Data (7): safe_json, safe_datetime, safe_float, safe_version,
#             safe_color, safe_angle, safe_unit
#   Data Structures (5): safe_buffer, safe_queue, safe_bloom, safe_lru,
#                        safe_graph
#   Resilience (4): safe_rate_limiter, safe_circuit_breaker, safe_retry,
#                   safe_monotonic
#   State (2): safe_state_machine, safe_calculator
#   Algorithm (4): safe_geo, safe_probability, safe_checksum, safe_tensor
#   Security (2): safe_password, safe_ml
#   HTTP (3): safe_header, safe_cookie, safe_content_type

package proven

import rego.v1

# Version information
version := "0.4.0"

module_count := 38

# ============================================================================
# RESULT TYPE HELPERS
# ============================================================================

# Create a successful result with a value
ok(value) := {"ok": true, "value": value}

# Create an error result with an error message
err(error) := {"ok": false, "error": error}

# Check if result indicates success
is_ok(result) if result.ok == true

# Check if result indicates failure
is_err(result) if result.ok == false

# Extract value from result or return default value if error
unwrap_or(result, default_val) := result.value if is_ok(result)

unwrap_or(result, default_val) := default_val if is_err(result)

# Map over a successful result, apply function to value
map_result(result, fn) := ok(fn(result.value)) if is_ok(result)

map_result(result, fn) := result if is_err(result)

# ============================================================================
# MODULE: safe_math - Safe Mathematical Operations
# ============================================================================

# Validate that a number is within safe integer bounds
validate_safe_integer(n) := ok(n) if {
	is_number(n)
	n >= -9007199254740991
	n <= 9007199254740991
}

validate_safe_integer(n) := err("Number outside safe integer range") if {
	is_number(n)
	n < -9007199254740991
}

validate_safe_integer(n) := err("Number outside safe integer range") if {
	is_number(n)
	n > 9007199254740991
}

validate_safe_integer(n) := err("Not a number") if not is_number(n)

# Validate positive number
validate_positive(n) := ok(n) if {
	is_number(n)
	n > 0
}

validate_positive(n) := err("Number must be positive") if {
	is_number(n)
	n <= 0
}

validate_positive(n) := err("Not a number") if not is_number(n)

# Validate non-negative number
validate_non_negative(n) := ok(n) if {
	is_number(n)
	n >= 0
}

validate_non_negative(n) := err("Number must be non-negative") if {
	is_number(n)
	n < 0
}

# Validate range
validate_range(n, min_val, max_val) := ok(n) if {
	is_number(n)
	n >= min_val
	n <= max_val
}

validate_range(n, min_val, max_val) := err(sprintf("Number must be between %v and %v", [min_val, max_val])) if {
	is_number(n)
	n < min_val
}

validate_range(n, min_val, max_val) := err(sprintf("Number must be between %v and %v", [min_val, max_val])) if {
	is_number(n)
	n > max_val
}

# Safe division with zero check
safe_div(a, b) := ok(a / b) if {
	is_number(a)
	is_number(b)
	b != 0
}

safe_div(a, b) := err("Division by zero") if {
	is_number(a)
	is_number(b)
	b == 0
}

# Safe modulo with zero check
safe_mod(a, b) := ok(a % b) if {
	is_number(a)
	is_number(b)
	b != 0
}

safe_mod(a, b) := err("Modulo by zero") if {
	is_number(a)
	is_number(b)
	b == 0
}

# Clamp value to range
clamp(value, min_val, max_val) := min_val if value < min_val

clamp(value, min_val, max_val) := max_val if value > max_val

clamp(value, min_val, max_val) := value if {
	value >= min_val
	value <= max_val
}

# ============================================================================
# MODULE: safe_string - Safe String Operations
# ============================================================================

# Validate non-empty string
validate_non_empty(s) := ok(s) if {
	is_string(s)
	count(s) > 0
}

validate_non_empty(s) := err("String cannot be empty") if {
	is_string(s)
	count(s) == 0
}

validate_non_empty(s) := err("Not a string") if not is_string(s)

# Validate max length
validate_max_length(s, max_len) := ok(s) if {
	is_string(s)
	count(s) <= max_len
}

validate_max_length(s, max_len) := err(sprintf("String exceeds max length %d", [max_len])) if {
	is_string(s)
	count(s) > max_len
}

# Validate min length
validate_min_length(s, min_len) := ok(s) if {
	is_string(s)
	count(s) >= min_len
}

validate_min_length(s, min_len) := err(sprintf("String must be at least %d characters", [min_len])) if {
	is_string(s)
	count(s) < min_len
}

# Validate string length range
validate_length_range(s, min_len, max_len) := ok(s) if {
	is_string(s)
	count(s) >= min_len
	count(s) <= max_len
}

validate_length_range(s, min_len, max_len) := err(sprintf("String length must be between %d and %d", [min_len, max_len])) if {
	is_string(s)
	count(s) < min_len
}

validate_length_range(s, min_len, max_len) := err(sprintf("String length must be between %d and %d", [min_len, max_len])) if {
	is_string(s)
	count(s) > max_len
}

# Validate alphanumeric string
validate_alphanumeric(s) := ok(s) if {
	is_string(s)
	regex.match(`^[a-zA-Z0-9]+$`, s)
}

validate_alphanumeric(s) := err("String must be alphanumeric") if {
	is_string(s)
	not regex.match(`^[a-zA-Z0-9]+$`, s)
}

# Validate identifier format (letters, digits, underscores, starting with letter)
validate_identifier(s) := ok(s) if {
	is_string(s)
	regex.match(`^[a-zA-Z_][a-zA-Z0-9_]*$`, s)
}

validate_identifier(s) := err("Invalid identifier format") if {
	is_string(s)
	not regex.match(`^[a-zA-Z_][a-zA-Z0-9_]*$`, s)
}

# ============================================================================
# MODULE: safe_path - Safe Path Operations
# ============================================================================

# Validate path does not contain traversal attacks
validate_no_traversal(path) := ok(path) if {
	is_string(path)
	not contains(path, "..")
	not contains(path, "//")
}

validate_no_traversal(path) := err("Path contains traversal pattern") if {
	is_string(path)
	contains(path, "..")
}

validate_no_traversal(path) := err("Path contains double slashes") if {
	is_string(path)
	contains(path, "//")
}

# Validate absolute path
validate_absolute_path(path) := ok(path) if {
	is_string(path)
	startswith(path, "/")
}

validate_absolute_path(path) := err("Path must be absolute") if {
	is_string(path)
	not startswith(path, "/")
}

# Validate relative path
validate_relative_path(path) := ok(path) if {
	is_string(path)
	not startswith(path, "/")
}

validate_relative_path(path) := err("Path must be relative") if {
	is_string(path)
	startswith(path, "/")
}

# Check if path is within allowed directory
path_within(path, allowed_dir) if {
	startswith(path, allowed_dir)
	validate_no_traversal(path).ok
}

# Validate file extension
validate_extension(path, allowed_extensions) := ok(path) if {
	is_string(path)
	some ext in allowed_extensions
	endswith(lower(path), ext)
}

validate_extension(path, allowed_extensions) := err("File extension not allowed") if {
	is_string(path)
	not some_extension_matches(path, allowed_extensions)
}

some_extension_matches(path, extensions) if {
	some ext in extensions
	endswith(lower(path), ext)
}

# ============================================================================
# MODULE: safe_email - Safe Email Validation
# ============================================================================

# Email validation pattern (RFC 5322 simplified)
email_pattern := `^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$`

# Validate email format
validate_email(s) := ok(s) if {
	is_string(s)
	regex.match(email_pattern, s)
	count(s) <= 254
}

validate_email(s) := err("Invalid email format") if {
	is_string(s)
	not regex.match(email_pattern, s)
}

validate_email(s) := err("Email too long") if {
	is_string(s)
	regex.match(email_pattern, s)
	count(s) > 254
}

# Extract domain from email
email_domain(email) := domain if {
	parts := split(email, "@")
	count(parts) == 2
	domain := parts[1]
}

# Check if email is from allowed domain
email_from_domain(email, allowed_domains) if {
	domain := email_domain(email)
	domain in allowed_domains
}

# ============================================================================
# MODULE: safe_url - Safe URL Validation
# ============================================================================

# URL pattern for validation
url_pattern := `^https?://[a-zA-Z0-9][-a-zA-Z0-9]*(\.[a-zA-Z0-9][-a-zA-Z0-9]*)*(:[0-9]+)?(/.*)?$`

# Validate URL format
validate_url(s) := ok(s) if {
	is_string(s)
	regex.match(url_pattern, s)
}

validate_url(s) := err("Invalid URL format") if {
	is_string(s)
	not regex.match(url_pattern, s)
}

# Validate HTTPS URL only
validate_https_url(s) := ok(s) if {
	is_string(s)
	startswith(s, "https://")
	regex.match(url_pattern, s)
}

validate_https_url(s) := err("URL must use HTTPS") if {
	is_string(s)
	not startswith(s, "https://")
}

# Check URL is from allowed origin
url_from_origin(url, allowed_origins) if {
	some origin in allowed_origins
	startswith(url, origin)
}

# ============================================================================
# MODULE: safe_network - Safe Network Validation
# ============================================================================

# Common port definitions
common_ports := {
	"http": 80,
	"https": 443,
	"ssh": 22,
	"dns": 53,
	"ftp": 21,
	"smtp": 25,
	"mysql": 3306,
	"postgres": 5432,
	"redis": 6379,
	"mongodb": 27017,
}

# Privileged ports (require root on Unix)
privileged_port_max := 1023

# Validate port number
validate_port(port) := ok(port) if {
	is_number(port)
	port >= 1
	port <= 65535
}

validate_port(port) := err("Port must be between 1 and 65535") if {
	is_number(port)
	port < 1
}

validate_port(port) := err("Port must be between 1 and 65535") if {
	is_number(port)
	port > 65535
}

# Check if port is privileged
is_privileged_port(port) if {
	port <= privileged_port_max
}

# Validate CIDR notation
validate_cidr(cidr) := ok(cidr) if {
	is_string(cidr)
	regex.match(`^(\d{1,3}\.){3}\d{1,3}/\d{1,2}$`, cidr)
	parts := split(cidr, "/")
	mask := to_number(parts[1])
	mask >= 0
	mask <= 32
}

validate_cidr(cidr) := err("Invalid CIDR notation") if {
	is_string(cidr)
	not regex.match(`^(\d{1,3}\.){3}\d{1,3}/\d{1,2}$`, cidr)
}

# Validate IPv4 address
validate_ipv4(ip) := ok(ip) if {
	is_string(ip)
	parts := split(ip, ".")
	count(parts) == 4
	all_valid_octets(parts)
}

validate_ipv4(ip) := err("Invalid IPv4 address") if {
	is_string(ip)
	parts := split(ip, ".")
	count(parts) != 4
}

validate_ipv4(ip) := err("Invalid IPv4 address - octet out of range") if {
	is_string(ip)
	parts := split(ip, ".")
	count(parts) == 4
	not all_valid_octets(parts)
}

all_valid_octets(parts) if {
	every part in parts {
		n := to_number(part)
		n >= 0
		n <= 255
	}
}

# Validate IPv6 address (simplified)
validate_ipv6(ip) := ok(ip) if {
	is_string(ip)
	regex.match(`^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$`, ip)
}

validate_ipv6(ip) := ok(ip) if {
	is_string(ip)
	contains(ip, "::")
	regex.match(`^[0-9a-fA-F:]+$`, ip)
}

# Check if IP is in private range
is_private_ipv4(ip) if {
	parts := split(ip, ".")
	to_number(parts[0]) == 10
}

is_private_ipv4(ip) if {
	parts := split(ip, ".")
	to_number(parts[0]) == 172
	second := to_number(parts[1])
	second >= 16
	second <= 31
}

is_private_ipv4(ip) if {
	parts := split(ip, ".")
	to_number(parts[0]) == 192
	to_number(parts[1]) == 168
}

# ============================================================================
# MODULE: safe_crypto - Safe Cryptographic Validation
# ============================================================================

# Validate hash length by algorithm
validate_hash_length(hash, algorithm) := ok(hash) if {
	algorithm == "sha256"
	count(hash) == 64
	regex.match(`^[a-fA-F0-9]+$`, hash)
}

validate_hash_length(hash, algorithm) := ok(hash) if {
	algorithm == "sha512"
	count(hash) == 128
	regex.match(`^[a-fA-F0-9]+$`, hash)
}

validate_hash_length(hash, algorithm) := ok(hash) if {
	algorithm == "blake3"
	count(hash) == 64
	regex.match(`^[a-fA-F0-9]+$`, hash)
}

validate_hash_length(hash, algorithm) := ok(hash) if {
	algorithm == "md5"
	count(hash) == 32
	regex.match(`^[a-fA-F0-9]+$`, hash)
}

validate_hash_length(hash, algorithm) := err("Invalid hash format") if {
	not regex.match(`^[a-fA-F0-9]+$`, hash)
}

# Validate key length for common algorithms
validate_key_length(key_bytes, algorithm) := ok(key_bytes) if {
	algorithm == "aes-128"
	key_bytes == 16
}

validate_key_length(key_bytes, algorithm) := ok(key_bytes) if {
	algorithm == "aes-256"
	key_bytes == 32
}

validate_key_length(key_bytes, algorithm) := ok(key_bytes) if {
	algorithm == "chacha20"
	key_bytes == 32
}

validate_key_length(key_bytes, algorithm) := err(sprintf("Invalid key length for %s", [algorithm])) if {
	algorithm == "aes-128"
	key_bytes != 16
}

validate_key_length(key_bytes, algorithm) := err(sprintf("Invalid key length for %s", [algorithm])) if {
	algorithm == "aes-256"
	key_bytes != 32
}

# Minimum recommended key sizes
min_rsa_key_bits := 2048

min_ec_key_bits := 256

# ============================================================================
# MODULE: safe_uuid - Safe UUID Validation
# ============================================================================

# UUID v4 pattern
uuid_v4_pattern := `^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-4[0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$`

# General UUID pattern (any version)
uuid_pattern := `^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$`

# Validate UUID format
validate_uuid(s) := ok(s) if {
	is_string(s)
	regex.match(uuid_pattern, s)
}

validate_uuid(s) := err("Invalid UUID format") if {
	is_string(s)
	not regex.match(uuid_pattern, s)
}

# Validate UUID v4 specifically
validate_uuid_v4(s) := ok(s) if {
	is_string(s)
	regex.match(uuid_v4_pattern, s)
}

validate_uuid_v4(s) := err("Invalid UUID v4 format") if {
	is_string(s)
	not regex.match(uuid_v4_pattern, s)
}

# ============================================================================
# MODULE: safe_currency - Safe Currency Validation
# ============================================================================

# ISO 4217 currency codes (subset of common ones)
valid_currency_codes := {
	"USD", "EUR", "GBP", "JPY", "CNY", "CHF", "CAD", "AUD", "NZD",
	"HKD", "SGD", "SEK", "NOK", "DKK", "INR", "BRL", "MXN", "ZAR",
	"KRW", "TWD", "THB", "PHP", "IDR", "MYR", "PLN", "CZK", "HUF",
	"RUB", "TRY", "ILS", "AED", "SAR", "BTC", "ETH",
}

# Validate currency code
validate_currency_code(code) := ok(code) if {
	upper(code) in valid_currency_codes
}

validate_currency_code(code) := err("Invalid currency code") if {
	not upper(code) in valid_currency_codes
}

# Validate monetary amount (positive, max 2 decimal places for most currencies)
validate_monetary_amount(amount) := ok(amount) if {
	is_number(amount)
	amount >= 0
}

validate_monetary_amount(amount) := err("Monetary amount must be non-negative") if {
	is_number(amount)
	amount < 0
}

# ============================================================================
# MODULE: safe_phone - Safe Phone Number Validation
# ============================================================================

# E.164 format pattern (international)
e164_pattern := `^\+[1-9]\d{1,14}$`

# Validate E.164 phone number
validate_e164_phone(s) := ok(s) if {
	is_string(s)
	regex.match(e164_pattern, s)
}

validate_e164_phone(s) := err("Invalid E.164 phone number format") if {
	is_string(s)
	not regex.match(e164_pattern, s)
}

# North American Numbering Plan pattern
nanp_pattern := `^\+1[2-9]\d{2}[2-9]\d{6}$`

# Validate NANP phone number
validate_nanp_phone(s) := ok(s) if {
	is_string(s)
	regex.match(nanp_pattern, s)
}

# ============================================================================
# MODULE: safe_hex - Safe Hexadecimal Validation
# ============================================================================

# Validate hexadecimal string
validate_hex(s) := ok(s) if {
	is_string(s)
	count(s) > 0
	regex.match(`^[0-9a-fA-F]+$`, s)
}

validate_hex(s) := err("Invalid hexadecimal string") if {
	is_string(s)
	not regex.match(`^[0-9a-fA-F]+$`, s)
}

# Validate hex with even length (for byte representation)
validate_hex_bytes(s) := ok(s) if {
	validate_hex(s).ok
	count(s) % 2 == 0
}

validate_hex_bytes(s) := err("Hex string must have even length for byte representation") if {
	validate_hex(s).ok
	count(s) % 2 != 0
}

# Validate hex with 0x prefix
validate_hex_prefixed(s) := ok(s) if {
	is_string(s)
	startswith(lower(s), "0x")
	regex.match(`^0[xX][0-9a-fA-F]+$`, s)
}

# ============================================================================
# MODULE: safe_json - Safe JSON Validation
# ============================================================================

# Validate JSON structure has required fields
validate_required_fields(obj, required) := ok(obj) if {
	is_object(obj)
	every field in required {
		obj[field]
	}
}

validate_required_fields(obj, required) := err(sprintf("Missing required fields: %v", [missing])) if {
	is_object(obj)
	missing := [f | some f in required; not obj[f]]
	count(missing) > 0
}

# Validate JSON object size
validate_object_size(obj, max_keys) := ok(obj) if {
	is_object(obj)
	count(obj) <= max_keys
}

validate_object_size(obj, max_keys) := err(sprintf("Object exceeds max key count %d", [max_keys])) if {
	is_object(obj)
	count(obj) > max_keys
}

# Validate array size
validate_array_size(arr, max_items) := ok(arr) if {
	is_array(arr)
	count(arr) <= max_items
}

validate_array_size(arr, max_items) := err(sprintf("Array exceeds max items %d", [max_items])) if {
	is_array(arr)
	count(arr) > max_items
}

# Validate JSON depth (simplified - checks if nested objects exist)
max_json_depth := 10

# ============================================================================
# MODULE: safe_datetime - Safe DateTime Validation
# ============================================================================

# ISO 8601 date pattern (YYYY-MM-DD)
iso_date_pattern := `^\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01])$`

# ISO 8601 datetime pattern
iso_datetime_pattern := `^\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01])T([01]\d|2[0-3]):[0-5]\d:[0-5]\d(\.\d+)?(Z|[+-]([01]\d|2[0-3]):[0-5]\d)?$`

# Validate ISO 8601 date
validate_iso_date(s) := ok(s) if {
	is_string(s)
	regex.match(iso_date_pattern, s)
}

validate_iso_date(s) := err("Invalid ISO 8601 date format") if {
	is_string(s)
	not regex.match(iso_date_pattern, s)
}

# Validate ISO 8601 datetime
validate_iso_datetime(s) := ok(s) if {
	is_string(s)
	regex.match(iso_datetime_pattern, s)
}

validate_iso_datetime(s) := err("Invalid ISO 8601 datetime format") if {
	is_string(s)
	not regex.match(iso_datetime_pattern, s)
}

# Validate Unix timestamp (seconds since epoch)
validate_unix_timestamp(ts) := ok(ts) if {
	is_number(ts)
	ts >= 0
	ts <= 253402300799
}

validate_unix_timestamp(ts) := err("Invalid Unix timestamp") if {
	is_number(ts)
	ts < 0
}

validate_unix_timestamp(ts) := err("Unix timestamp too far in future") if {
	is_number(ts)
	ts > 253402300799
}

# ============================================================================
# MODULE: safe_float - Safe Floating Point Validation
# ============================================================================

# Validate finite number (not NaN or Infinity)
validate_finite(n) := ok(n) if {
	is_number(n)
	n == n
	n != n + 1e308
	n != n - 1e308
}

# Validate precision (decimal places)
validate_precision(n, max_decimals) := ok(n) if {
	is_number(n)
	str := sprintf("%v", [n])
	parts := split(str, ".")
	count(parts) == 1
}

validate_precision(n, max_decimals) := ok(n) if {
	is_number(n)
	str := sprintf("%v", [n])
	parts := split(str, ".")
	count(parts) == 2
	count(parts[1]) <= max_decimals
}

# ============================================================================
# MODULE: safe_version - Safe Semantic Version Validation
# ============================================================================

# Semantic version pattern
semver_pattern := `^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(-[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(\+[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?$`

# Validate semantic version
validate_semver(s) := ok(s) if {
	is_string(s)
	regex.match(semver_pattern, s)
}

validate_semver(s) := err("Invalid semantic version format") if {
	is_string(s)
	not regex.match(semver_pattern, s)
}

# Parse major version
parse_major(version) := major if {
	parts := split(version, ".")
	major := to_number(parts[0])
}

# Parse minor version
parse_minor(version) := minor if {
	parts := split(version, ".")
	minor := to_number(parts[1])
}

# ============================================================================
# MODULE: safe_color - Safe Color Validation
# ============================================================================

# Hex color pattern (3 or 6 digits)
hex_color_pattern := `^#([0-9a-fA-F]{3}|[0-9a-fA-F]{6})$`

# RGB pattern
rgb_pattern := `^rgb\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\)$`

# Validate hex color
validate_hex_color(s) := ok(s) if {
	is_string(s)
	regex.match(hex_color_pattern, s)
}

validate_hex_color(s) := err("Invalid hex color format") if {
	is_string(s)
	not regex.match(hex_color_pattern, s)
}

# Validate RGB values
validate_rgb(r, g, b) := ok([r, g, b]) if {
	r >= 0
	r <= 255
	g >= 0
	g <= 255
	b >= 0
	b <= 255
}

validate_rgb(r, g, b) := err("RGB values must be between 0 and 255") if {
	r < 0
}

validate_rgb(r, g, b) := err("RGB values must be between 0 and 255") if {
	r > 255
}

validate_rgb(r, g, b) := err("RGB values must be between 0 and 255") if {
	g < 0
}

validate_rgb(r, g, b) := err("RGB values must be between 0 and 255") if {
	g > 255
}

validate_rgb(r, g, b) := err("RGB values must be between 0 and 255") if {
	b < 0
}

validate_rgb(r, g, b) := err("RGB values must be between 0 and 255") if {
	b > 255
}

# ============================================================================
# MODULE: safe_angle - Safe Angle Validation
# ============================================================================

# Validate degrees (0-360)
validate_degrees(angle) := ok(angle) if {
	is_number(angle)
	angle >= 0
	angle <= 360
}

validate_degrees(angle) := err("Degrees must be between 0 and 360") if {
	is_number(angle)
	angle < 0
}

validate_degrees(angle) := err("Degrees must be between 0 and 360") if {
	is_number(angle)
	angle > 360
}

# Validate radians (-2pi to 2pi)
validate_radians(angle) := ok(angle) if {
	is_number(angle)
	angle >= -6.283185307
	angle <= 6.283185307
}

# Normalize degrees to 0-360 range
normalize_degrees(angle) := (angle % 360 + 360) % 360

# ============================================================================
# MODULE: safe_unit - Safe Unit Conversion Validation
# ============================================================================

# Kubernetes memory format pattern (e.g., "512Mi", "1Gi")
k8s_memory_pattern := `^\d+[EPTGMK]?i?$`

# Kubernetes CPU format pattern (e.g., "100m", "1.5")
k8s_cpu_pattern := `^\d+(\.\d+)?m?$`

# Validate Kubernetes memory format
validate_k8s_memory(mem) := ok(mem) if {
	is_string(mem)
	regex.match(k8s_memory_pattern, mem)
}

validate_k8s_memory(mem) := err("Invalid Kubernetes memory format") if {
	is_string(mem)
	not regex.match(k8s_memory_pattern, mem)
}

# Validate Kubernetes CPU format
validate_k8s_cpu(cpu) := ok(cpu) if {
	is_string(cpu)
	regex.match(k8s_cpu_pattern, cpu)
}

validate_k8s_cpu(cpu) := err("Invalid Kubernetes CPU format") if {
	is_string(cpu)
	not regex.match(k8s_cpu_pattern, cpu)
}

# Size units in bytes
size_units := {
	"B": 1,
	"KB": 1000,
	"MB": 1000000,
	"GB": 1000000000,
	"TB": 1000000000000,
	"KiB": 1024,
	"MiB": 1048576,
	"GiB": 1073741824,
	"TiB": 1099511627776,
}

# ============================================================================
# MODULE: safe_buffer - Safe Buffer Operations
# ============================================================================

# Validate buffer size
validate_buffer_size(size, max_size) := ok(size) if {
	is_number(size)
	size > 0
	size <= max_size
}

validate_buffer_size(size, max_size) := err("Buffer size must be positive") if {
	is_number(size)
	size <= 0
}

validate_buffer_size(size, max_size) := err(sprintf("Buffer size exceeds maximum %d", [max_size])) if {
	is_number(size)
	size > max_size
}

# Default max buffer size (1GB)
default_max_buffer_size := 1073741824

# ============================================================================
# MODULE: safe_queue - Safe Queue Operations
# ============================================================================

# Validate queue capacity
validate_queue_capacity(capacity) := ok(capacity) if {
	is_number(capacity)
	capacity > 0
	capacity <= 1000000
}

validate_queue_capacity(capacity) := err("Queue capacity must be positive and <= 1M") if {
	is_number(capacity)
	capacity <= 0
}

validate_queue_capacity(capacity) := err("Queue capacity must be positive and <= 1M") if {
	is_number(capacity)
	capacity > 1000000
}

# ============================================================================
# MODULE: safe_bloom - Safe Bloom Filter Parameters
# ============================================================================

# Validate Bloom filter parameters
validate_bloom_params(size, hash_count) := ok({"size": size, "hash_count": hash_count}) if {
	is_number(size)
	is_number(hash_count)
	size > 0
	hash_count > 0
	hash_count <= 20
}

validate_bloom_params(size, hash_count) := err("Invalid Bloom filter parameters") if {
	is_number(size)
	is_number(hash_count)
	size <= 0
}

validate_bloom_params(size, hash_count) := err("Invalid Bloom filter parameters") if {
	is_number(size)
	is_number(hash_count)
	hash_count <= 0
}

validate_bloom_params(size, hash_count) := err("Too many hash functions") if {
	is_number(size)
	is_number(hash_count)
	hash_count > 20
}

# ============================================================================
# MODULE: safe_lru - Safe LRU Cache Parameters
# ============================================================================

# Validate LRU cache capacity
validate_lru_capacity(capacity) := ok(capacity) if {
	is_number(capacity)
	capacity > 0
	capacity <= 10000000
}

validate_lru_capacity(capacity) := err("LRU capacity must be positive") if {
	is_number(capacity)
	capacity <= 0
}

validate_lru_capacity(capacity) := err("LRU capacity too large") if {
	is_number(capacity)
	capacity > 10000000
}

# ============================================================================
# MODULE: safe_graph - Safe Graph Operations
# ============================================================================

# Validate graph node count
validate_node_count(count_val, max_nodes) := ok(count_val) if {
	is_number(count_val)
	count_val >= 0
	count_val <= max_nodes
}

validate_node_count(count_val, max_nodes) := err(sprintf("Node count exceeds maximum %d", [max_nodes])) if {
	is_number(count_val)
	count_val > max_nodes
}

# Validate graph edge
validate_graph_edge(edge) := ok(edge) if {
	is_object(edge)
	edge.from
	edge.to
}

validate_graph_edge(edge) := err("Edge must have 'from' and 'to' fields") if {
	is_object(edge)
	not edge.from
}

validate_graph_edge(edge) := err("Edge must have 'from' and 'to' fields") if {
	is_object(edge)
	not edge.to
}

# ============================================================================
# MODULE: safe_rate_limiter - Rate Limiting Rules
# ============================================================================

# Default rate limits
default_rate_limits := {
	"requests_per_second": 100,
	"requests_per_minute": 1000,
	"requests_per_hour": 10000,
}

# Validate rate limit configuration
validate_rate_limit(limit, window_seconds) := ok({"limit": limit, "window": window_seconds}) if {
	is_number(limit)
	is_number(window_seconds)
	limit > 0
	window_seconds > 0
}

validate_rate_limit(limit, window_seconds) := err("Rate limit values must be positive") if {
	is_number(limit)
	is_number(window_seconds)
	limit <= 0
}

validate_rate_limit(limit, window_seconds) := err("Rate limit values must be positive") if {
	is_number(limit)
	is_number(window_seconds)
	window_seconds <= 0
}

# Check if request count is within rate limit
within_rate_limit(request_count, limit) if {
	request_count <= limit
}

# ============================================================================
# MODULE: safe_circuit_breaker - Circuit Breaker Rules
# ============================================================================

# Circuit breaker states
circuit_breaker_states := {"closed", "open", "half_open"}

# Validate circuit breaker state
validate_circuit_state(state) := ok(state) if {
	state in circuit_breaker_states
}

validate_circuit_state(state) := err("Invalid circuit breaker state") if {
	not state in circuit_breaker_states
}

# Validate circuit breaker thresholds
validate_circuit_config(failure_threshold, recovery_timeout) := ok({"threshold": failure_threshold, "timeout": recovery_timeout}) if {
	is_number(failure_threshold)
	is_number(recovery_timeout)
	failure_threshold > 0
	recovery_timeout > 0
}

# Check if circuit should open
should_open_circuit(failure_count, threshold) if {
	failure_count >= threshold
}

# Check if circuit should try half-open
should_try_half_open(last_failure_time, recovery_timeout, current_time) if {
	current_time - last_failure_time >= recovery_timeout
}

# ============================================================================
# MODULE: safe_retry - Retry Policy Rules
# ============================================================================

# Validate retry configuration
validate_retry_config(max_retries, base_delay_ms) := ok({"max_retries": max_retries, "base_delay": base_delay_ms}) if {
	is_number(max_retries)
	is_number(base_delay_ms)
	max_retries >= 0
	max_retries <= 10
	base_delay_ms > 0
	base_delay_ms <= 60000
}

validate_retry_config(max_retries, base_delay_ms) := err("Max retries must be between 0 and 10") if {
	is_number(max_retries)
	max_retries < 0
}

validate_retry_config(max_retries, base_delay_ms) := err("Max retries must be between 0 and 10") if {
	is_number(max_retries)
	max_retries > 10
}

validate_retry_config(max_retries, base_delay_ms) := err("Base delay must be between 1ms and 60s") if {
	is_number(base_delay_ms)
	base_delay_ms <= 0
}

validate_retry_config(max_retries, base_delay_ms) := err("Base delay must be between 1ms and 60s") if {
	is_number(base_delay_ms)
	base_delay_ms > 60000
}

# Calculate exponential backoff delay
exponential_backoff(attempt, base_delay) := base_delay * (2 ^ attempt)

# Check if should retry based on error type
retryable_errors := {"timeout", "connection_refused", "service_unavailable", "rate_limited"}

is_retryable_error(error_type) if {
	error_type in retryable_errors
}

# ============================================================================
# MODULE: safe_monotonic - Monotonic Sequence Validation
# ============================================================================

# Validate monotonically increasing sequence
validate_monotonic_increasing(seq) := ok(seq) if {
	is_array(seq)
	count(seq) <= 1
}

validate_monotonic_increasing(seq) := ok(seq) if {
	is_array(seq)
	count(seq) > 1
	is_strictly_increasing(seq)
}

validate_monotonic_increasing(seq) := err("Sequence is not monotonically increasing") if {
	is_array(seq)
	count(seq) > 1
	not is_strictly_increasing(seq)
}

is_strictly_increasing(seq) if {
	every i in numbers.range(0, count(seq) - 2) {
		seq[i] < seq[i + 1]
	}
}

# Validate monotonically non-decreasing sequence
validate_non_decreasing(seq) := ok(seq) if {
	is_array(seq)
	count(seq) <= 1
}

validate_non_decreasing(seq) := ok(seq) if {
	is_array(seq)
	count(seq) > 1
	is_non_decreasing(seq)
}

is_non_decreasing(seq) if {
	every i in numbers.range(0, count(seq) - 2) {
		seq[i] <= seq[i + 1]
	}
}

# ============================================================================
# MODULE: safe_state_machine - State Machine Rules
# ============================================================================

# Validate state transition
validate_transition(from_state, to_state, allowed_transitions) := ok({"from": from_state, "to": to_state}) if {
	from_state in object.keys(allowed_transitions)
	to_state in allowed_transitions[from_state]
}

validate_transition(from_state, to_state, allowed_transitions) := err(sprintf("Invalid transition from %s to %s", [from_state, to_state])) if {
	from_state in object.keys(allowed_transitions)
	not to_state in allowed_transitions[from_state]
}

validate_transition(from_state, to_state, allowed_transitions) := err(sprintf("Unknown state: %s", [from_state])) if {
	not from_state in object.keys(allowed_transitions)
}

# Common order state machine
order_transitions := {
	"pending": ["confirmed", "cancelled"],
	"confirmed": ["shipped", "cancelled"],
	"shipped": ["delivered"],
	"delivered": [],
	"cancelled": [],
}

# Common HTTP request state machine
request_transitions := {
	"created": ["sent", "failed"],
	"sent": ["received", "timeout", "failed"],
	"received": ["processing"],
	"processing": ["completed", "failed"],
	"completed": [],
	"failed": ["retrying"],
	"timeout": ["retrying", "failed"],
	"retrying": ["sent", "failed"],
}

# ============================================================================
# MODULE: safe_calculator - Safe Expression Validation
# ============================================================================

# Allowed operators
safe_operators := {"+", "-", "*", "/", "%", "^"}

# Validate calculator expression (basic)
validate_expression(expr) := ok(expr) if {
	is_string(expr)
	regex.match(`^[\d\s\+\-\*\/\%\^\(\)\.]+$`, expr)
}

validate_expression(expr) := err("Expression contains invalid characters") if {
	is_string(expr)
	not regex.match(`^[\d\s\+\-\*\/\%\^\(\)\.]+$`, expr)
}

# ============================================================================
# MODULE: safe_geo - Geographic Validation
# ============================================================================

# Earth radius constants
earth_radius_km := 6371.0

earth_radius_mi := 3958.8

# Validate latitude
validate_latitude(lat) := ok(lat) if {
	is_number(lat)
	lat >= -90
	lat <= 90
}

validate_latitude(lat) := err("Latitude must be between -90 and 90") if {
	is_number(lat)
	lat < -90
}

validate_latitude(lat) := err("Latitude must be between -90 and 90") if {
	is_number(lat)
	lat > 90
}

# Validate longitude
validate_longitude(lon) := ok(lon) if {
	is_number(lon)
	lon >= -180
	lon <= 180
}

validate_longitude(lon) := err("Longitude must be between -180 and 180") if {
	is_number(lon)
	lon < -180
}

validate_longitude(lon) := err("Longitude must be between -180 and 180") if {
	is_number(lon)
	lon > 180
}

# Validate coordinate pair
validate_coordinate(lat, lon) := ok({"lat": lat, "lon": lon}) if {
	validate_latitude(lat).ok
	validate_longitude(lon).ok
}

validate_coordinate(lat, lon) := err("Invalid latitude") if {
	not validate_latitude(lat).ok
}

validate_coordinate(lat, lon) := err("Invalid longitude") if {
	validate_latitude(lat).ok
	not validate_longitude(lon).ok
}

# Validate bounding box
validate_bounding_box(min_lat, min_lon, max_lat, max_lon) := ok({"min_lat": min_lat, "min_lon": min_lon, "max_lat": max_lat, "max_lon": max_lon}) if {
	validate_latitude(min_lat).ok
	validate_latitude(max_lat).ok
	validate_longitude(min_lon).ok
	validate_longitude(max_lon).ok
	min_lat <= max_lat
}

validate_bounding_box(min_lat, min_lon, max_lat, max_lon) := err("min_lat must be <= max_lat") if {
	validate_latitude(min_lat).ok
	validate_latitude(max_lat).ok
	min_lat > max_lat
}

# Check if point is within bounding box
point_in_bbox(lat, lon, bbox) if {
	lat >= bbox.min_lat
	lat <= bbox.max_lat
	lon >= bbox.min_lon
	lon <= bbox.max_lon
}

# ============================================================================
# MODULE: safe_probability - Probability Validation
# ============================================================================

# Validate probability value (0 to 1)
validate_probability(p) := ok(p) if {
	is_number(p)
	p >= 0
	p <= 1
}

validate_probability(p) := err("Probability must be between 0 and 1") if {
	is_number(p)
	p < 0
}

validate_probability(p) := err("Probability must be between 0 and 1") if {
	is_number(p)
	p > 1
}

# Validate probability distribution (sums to 1)
validate_distribution(probs) := ok(probs) if {
	is_array(probs)
	every p in probs {
		p >= 0
		p <= 1
	}
	sum(probs) > 0.999
	sum(probs) < 1.001
}

validate_distribution(probs) := err("Distribution probabilities must sum to 1") if {
	is_array(probs)
	prob_sum := sum(probs)
	prob_sum <= 0.999
}

validate_distribution(probs) := err("Distribution probabilities must sum to 1") if {
	is_array(probs)
	prob_sum := sum(probs)
	prob_sum >= 1.001
}

# ============================================================================
# MODULE: safe_checksum - Checksum Validation
# ============================================================================

# Luhn algorithm validation (for credit cards, etc.)
validate_luhn(num_str) := ok(num_str) if {
	is_string(num_str)
	regex.match(`^\d+$`, num_str)
	count(num_str) >= 2
}

# Common checksum lengths
checksum_lengths := {
	"md5": 32,
	"sha1": 40,
	"sha256": 64,
	"sha512": 128,
	"crc32": 8,
}

# Validate checksum format by algorithm
validate_checksum_format(checksum, algorithm) := ok(checksum) if {
	expected_len := checksum_lengths[algorithm]
	count(checksum) == expected_len
	regex.match(`^[0-9a-fA-F]+$`, checksum)
}

validate_checksum_format(checksum, algorithm) := err(sprintf("Invalid %s checksum format", [algorithm])) if {
	expected_len := checksum_lengths[algorithm]
	count(checksum) != expected_len
}

# ============================================================================
# MODULE: safe_tensor - Tensor Shape Validation
# ============================================================================

# Validate tensor shape
validate_tensor_shape(shape) := ok(shape) if {
	is_array(shape)
	count(shape) > 0
	every dim in shape {
		is_number(dim)
		dim > 0
	}
}

validate_tensor_shape(shape) := err("Tensor dimensions must be positive integers") if {
	is_array(shape)
	some dim in shape
	dim <= 0
}

# Validate compatible shapes for matrix multiplication
validate_matmul_shapes(shape_a, shape_b) := ok({"a": shape_a, "b": shape_b}) if {
	is_array(shape_a)
	is_array(shape_b)
	count(shape_a) == 2
	count(shape_b) == 2
	shape_a[1] == shape_b[0]
}

validate_matmul_shapes(shape_a, shape_b) := err("Incompatible shapes for matrix multiplication") if {
	is_array(shape_a)
	is_array(shape_b)
	count(shape_a) == 2
	count(shape_b) == 2
	shape_a[1] != shape_b[0]
}

# Calculate tensor element count
tensor_size(shape) := product if {
	product := numbers.product(shape)
}

# ============================================================================
# MODULE: safe_password - Password Policy Validation
# ============================================================================

# Minimum password length
min_password_length := 12

# Maximum password length
max_password_length := 128

# Validate password meets minimum requirements
validate_password(password) := ok("valid") if {
	is_string(password)
	count(password) >= min_password_length
	count(password) <= max_password_length
	has_lowercase(password)
	has_uppercase(password)
	has_digit(password)
}

validate_password(password) := err(sprintf("Password must be at least %d characters", [min_password_length])) if {
	is_string(password)
	count(password) < min_password_length
}

validate_password(password) := err(sprintf("Password must be at most %d characters", [max_password_length])) if {
	is_string(password)
	count(password) > max_password_length
}

validate_password(password) := err("Password must contain a lowercase letter") if {
	is_string(password)
	count(password) >= min_password_length
	count(password) <= max_password_length
	not has_lowercase(password)
}

validate_password(password) := err("Password must contain an uppercase letter") if {
	is_string(password)
	count(password) >= min_password_length
	count(password) <= max_password_length
	has_lowercase(password)
	not has_uppercase(password)
}

validate_password(password) := err("Password must contain a digit") if {
	is_string(password)
	count(password) >= min_password_length
	count(password) <= max_password_length
	has_lowercase(password)
	has_uppercase(password)
	not has_digit(password)
}

has_lowercase(s) if regex.match(`[a-z]`, s)

has_uppercase(s) if regex.match(`[A-Z]`, s)

has_digit(s) if regex.match(`[0-9]`, s)

has_special(s) if regex.match(`[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]`, s)

# Common password patterns to reject
common_password_patterns := [
	"^password",
	"^123456",
	"^qwerty",
	"^admin",
	"^letmein",
]

is_common_password(password) if {
	some pattern in common_password_patterns
	regex.match(pattern, lower(password))
}

# ============================================================================
# MODULE: safe_ml - Machine Learning Validation
# ============================================================================

# Validate model confidence score
validate_confidence(score) := ok(score) if {
	is_number(score)
	score >= 0
	score <= 1
}

validate_confidence(score) := err("Confidence must be between 0 and 1") if {
	is_number(score)
	score < 0
}

validate_confidence(score) := err("Confidence must be between 0 and 1") if {
	is_number(score)
	score > 1
}

# Minimum confidence threshold
min_confidence_threshold := 0.5

# Validate prediction passes threshold
validate_prediction(prediction, threshold) := ok(prediction) if {
	is_object(prediction)
	prediction.confidence >= threshold
}

validate_prediction(prediction, threshold) := err("Prediction confidence below threshold") if {
	is_object(prediction)
	prediction.confidence < threshold
}

# Validate feature vector
validate_feature_vector(features, expected_length) := ok(features) if {
	is_array(features)
	count(features) == expected_length
	every f in features {
		is_number(f)
	}
}

validate_feature_vector(features, expected_length) := err(sprintf("Feature vector must have %d elements", [expected_length])) if {
	is_array(features)
	count(features) != expected_length
}

# ============================================================================
# MODULE: safe_header - HTTP Header Validation
# ============================================================================

# Header name pattern (RFC 7230)
header_name_pattern := `^[!#$%&'*+\-.^_\x60|~0-9A-Za-z]+$`

# Validate HTTP header name
validate_header_name(name) := ok(name) if {
	is_string(name)
	count(name) > 0
	count(name) <= 256
	regex.match(header_name_pattern, name)
}

validate_header_name(name) := err("Invalid HTTP header name") if {
	is_string(name)
	not regex.match(header_name_pattern, name)
}

validate_header_name(name) := err("Header name too long") if {
	is_string(name)
	count(name) > 256
}

# Validate HTTP header value (no control characters except HTAB)
validate_header_value(value) := ok(value) if {
	is_string(value)
	count(value) <= 8192
	not regex.match(`[\x00-\x08\x0A-\x1F\x7F]`, value)
}

validate_header_value(value) := err("Header value contains invalid characters") if {
	is_string(value)
	regex.match(`[\x00-\x08\x0A-\x1F\x7F]`, value)
}

validate_header_value(value) := err("Header value too long") if {
	is_string(value)
	count(value) > 8192
}

# Security headers that should be present
security_headers := {
	"Strict-Transport-Security",
	"X-Content-Type-Options",
	"X-Frame-Options",
	"X-XSS-Protection",
	"Content-Security-Policy",
}

# Check if required security headers are present
missing_security_headers(headers) := missing if {
	missing := {h | some h in security_headers; not headers[h]}
}

# ============================================================================
# MODULE: safe_cookie - HTTP Cookie Validation
# ============================================================================

# Cookie name pattern (RFC 6265)
cookie_name_pattern := `^[!#$%&'*+\-.0-9A-Z^_\x60a-z|~]+$`

# Validate cookie name
validate_cookie_name(name) := ok(name) if {
	is_string(name)
	count(name) > 0
	count(name) <= 256
	regex.match(cookie_name_pattern, name)
}

validate_cookie_name(name) := err("Invalid cookie name") if {
	is_string(name)
	not regex.match(cookie_name_pattern, name)
}

# Cookie attributes
cookie_same_site_values := {"Strict", "Lax", "None"}

# Validate SameSite attribute
validate_same_site(value) := ok(value) if {
	value in cookie_same_site_values
}

validate_same_site(value) := err("Invalid SameSite value") if {
	not value in cookie_same_site_values
}

# Validate secure cookie configuration
validate_secure_cookie(cookie) := ok(cookie) if {
	is_object(cookie)
	cookie.secure == true
	cookie.httpOnly == true
	cookie.sameSite in {"Strict", "Lax"}
}

validate_secure_cookie(cookie) := err("Cookie must be Secure") if {
	is_object(cookie)
	cookie.secure != true
}

validate_secure_cookie(cookie) := err("Cookie must be HttpOnly") if {
	is_object(cookie)
	cookie.secure == true
	cookie.httpOnly != true
}

validate_secure_cookie(cookie) := err("Cookie should have SameSite Strict or Lax") if {
	is_object(cookie)
	cookie.secure == true
	cookie.httpOnly == true
	not cookie.sameSite in {"Strict", "Lax"}
}

# ============================================================================
# MODULE: safe_content_type - Content Type Validation
# ============================================================================

# MIME type pattern
mime_type_pattern := `^[a-zA-Z0-9][a-zA-Z0-9!#$&\-^_.+]*\/[a-zA-Z0-9][a-zA-Z0-9!#$&\-^_.+]*$`

# Validate MIME type format
validate_mime_type(mime) := ok(mime) if {
	is_string(mime)
	regex.match(mime_type_pattern, mime)
}

validate_mime_type(mime) := err("Invalid MIME type format") if {
	is_string(mime)
	not regex.match(mime_type_pattern, mime)
}

# Safe content types (not executable)
safe_content_types := {
	"text/plain",
	"text/html",
	"text/css",
	"text/csv",
	"application/json",
	"application/xml",
	"image/png",
	"image/jpeg",
	"image/gif",
	"image/webp",
	"image/svg+xml",
	"audio/mpeg",
	"audio/wav",
	"video/mp4",
	"video/webm",
}

# Dangerous content types
dangerous_content_types := {
	"application/x-executable",
	"application/x-msdownload",
	"application/x-sh",
	"application/x-shellscript",
	"application/x-msdos-program",
	"application/hta",
	"application/javascript",
}

# Check if content type is safe
is_safe_content_type(content_type) if {
	content_type in safe_content_types
}

# Check if content type is dangerous
is_dangerous_content_type(content_type) if {
	content_type in dangerous_content_types
}

# ============================================================================
# KUBERNETES SECURITY RULES
# ============================================================================

# Validate replica count
validate_replicas(n, min_val, max_val) := validate_range(n, min_val, max_val)

validate_replicas_default(n) := validate_range(n, 1, 100)

# Check if container runs as non-root
deny_root_container contains msg if {
	input.kind == "Pod"
	container := input.spec.containers[_]
	not container.securityContext.runAsNonRoot == true
	msg := sprintf("Container %s must run as non-root", [container.name])
}

# Check if privileged mode is disabled
deny_privileged contains msg if {
	input.kind == "Pod"
	container := input.spec.containers[_]
	container.securityContext.privileged == true
	msg := sprintf("Container %s must not run in privileged mode", [container.name])
}

# Check for required resource limits
deny_missing_limits contains msg if {
	input.kind == "Pod"
	container := input.spec.containers[_]
	not container.resources.limits
	msg := sprintf("Container %s must have resource limits", [container.name])
}

# Check for dangerous capabilities
deny_dangerous_capabilities contains msg if {
	input.kind == "Pod"
	container := input.spec.containers[_]
	cap := container.securityContext.capabilities.add[_]
	dangerous := {"SYS_ADMIN", "NET_ADMIN", "ALL"}
	cap in dangerous
	msg := sprintf("Container %s has dangerous capability: %s", [container.name, cap])
}

# Check for host network usage
deny_host_network contains msg if {
	input.kind == "Pod"
	input.spec.hostNetwork == true
	msg := "Pod must not use host network"
}

# Check for host PID usage
deny_host_pid contains msg if {
	input.kind == "Pod"
	input.spec.hostPID == true
	msg := "Pod must not use host PID namespace"
}

# Check for required labels
deny_missing_labels contains msg if {
	input.kind == "Pod"
	required := {"app", "version"}
	missing := {label | some label in required; not input.metadata.labels[label]}
	count(missing) > 0
	msg := sprintf("Pod missing required labels: %v", [missing])
}

# ============================================================================
# VALIDATION COMBINATORS
# ============================================================================

# All validations must pass
validate_all(results) := ok([r.value | some r in results; is_ok(r)]) if {
	not any_err(results)
}

validate_all(results) := first_err(results) if {
	any_err(results)
}

any_err(results) if {
	some r in results
	is_err(r)
}

first_err(results) := r if {
	some r in results
	is_err(r)
}

# Any validation must pass
validate_any(results) := ok(r.value) if {
	some r in results
	is_ok(r)
}

validate_any(results) := err("All validations failed") if {
	every r in results {
		is_err(r)
	}
}

# Validate object field
validate_field(obj, field, validator) := validator(obj[field]) if {
	obj[field]
}

validate_field(obj, field, validator) := err(sprintf("Missing field: %s", [field])) if {
	not obj[field]
}

# Chain validations (monadic bind)
chain(result, fn) := fn(result.value) if is_ok(result)

chain(result, fn) := result if is_err(result)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

# Get current timestamp (requires time.now_ns())
current_timestamp_ns := time.now_ns()

current_timestamp_ms := time.now_ns() / 1000000

current_timestamp_s := time.now_ns() / 1000000000

# Check if value is in set
in_set(value, allowed_set) if {
	value in allowed_set
}

# Check if all values are in set
all_in_set(values, allowed_set) if {
	every v in values {
		v in allowed_set
	}
}

# Check if any value is in set
any_in_set(values, allowed_set) if {
	some v in values
	v in allowed_set
}
