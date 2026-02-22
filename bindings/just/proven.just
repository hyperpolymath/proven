# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven Safety Library - Importable Just module
#
# Standalone recipe file that can be imported into any justfile:
#
#   import '/path/to/proven.just'
#
# ALL computation delegates to proven-cli (Idris 2 + Zig FFI).
# This file does NOT reimplement any safety-critical logic.
#
# Prerequisites:
#   - proven-cli on PATH, or PROVEN_CLI env var set

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

proven_cli := env("PROVEN_CLI", "proven-cli")

# ===========================================================================
# SafeMath - Arithmetic that cannot crash
# ===========================================================================

# Safe addition with overflow checking
# Usage: just safe-add 10 20
[doc("Add two integers safely (overflow-checked)")]
safe-add a b:
    @{{ proven_cli }} math add {{ a }} {{ b }}

# Safe subtraction with underflow checking
# Usage: just safe-sub 30 10
[doc("Subtract two integers safely (underflow-checked)")]
safe-sub a b:
    @{{ proven_cli }} math sub {{ a }} {{ b }}

# Safe multiplication with overflow checking
# Usage: just safe-mul 6 7
[doc("Multiply two integers safely (overflow-checked)")]
safe-mul a b:
    @{{ proven_cli }} math mul {{ a }} {{ b }}

# Safe division (division by zero returns error, not crash)
# Usage: just safe-div 100 3
[doc("Divide two integers safely (no div-by-zero crash)")]
safe-div a b:
    @{{ proven_cli }} math div {{ a }} {{ b }}

# Safe modulo (modulo by zero returns error, not crash)
# Usage: just safe-mod 17 5
[doc("Modulo two integers safely (no div-by-zero crash)")]
safe-mod a b:
    @{{ proven_cli }} math mod {{ a }} {{ b }}

# Safe absolute value (handles MIN_INT correctly)
# Usage: just safe-abs -42
[doc("Absolute value (handles MIN_INT edge case)")]
safe-abs n:
    @{{ proven_cli }} math abs {{ n }}

# Safe exponentiation with overflow checking
# Usage: just safe-pow 2 10
[doc("Exponentiation with overflow checking")]
safe-pow base exp:
    @{{ proven_cli }} math pow {{ base }} {{ exp }}

# Clamp a value to a [min, max] range
# Usage: just safe-clamp 0 100 150
[doc("Clamp a value to [min, max] range")]
safe-clamp min max value:
    @{{ proven_cli }} math clamp {{ min }} {{ max }} {{ value }}

# ===========================================================================
# SafeFloat - Floating-point operations
# ===========================================================================

# Safe float division
# Usage: just float-div 10.0 3.0
[doc("Divide two floats safely")]
float-div a b:
    @{{ proven_cli }} float div {{ a }} {{ b }}

# Safe square root (rejects negative inputs)
# Usage: just float-sqrt 16.0
[doc("Square root (rejects negatives)")]
float-sqrt n:
    @{{ proven_cli }} float sqrt {{ n }}

# Safe natural logarithm (rejects non-positive inputs)
# Usage: just float-ln 2.718
[doc("Natural logarithm (rejects non-positive)")]
float-ln n:
    @{{ proven_cli }} float ln {{ n }}

# ===========================================================================
# Validation - Input checking via proven-cli
# ===========================================================================

# Validate an email address (RFC 5321)
# Usage: just validate-email "user@example.com"
[doc("Validate an email address (RFC 5321)")]
validate-email email:
    @{{ proven_cli }} validate email "{{ email }}"

# Validate a URL
# Usage: just validate-url "https://example.com"
[doc("Validate a URL")]
validate-url url:
    @{{ proven_cli }} validate url "{{ url }}"

# Validate an IPv4 address
# Usage: just validate-ipv4 "192.168.1.1"
[doc("Validate an IPv4 address")]
validate-ipv4 ip:
    @{{ proven_cli }} network parse-ipv4 "{{ ip }}"

# Check if IPv4 address is private (RFC 1918)
# Usage: just ipv4-is-private "10.0.0.1"
[doc("Check if IPv4 is private (RFC 1918)")]
ipv4-is-private ip:
    @{{ proven_cli }} network ipv4-is-private "{{ ip }}"

# Check if IPv4 address is loopback
# Usage: just ipv4-is-loopback "127.0.0.1"
[doc("Check if IPv4 is loopback")]
ipv4-is-loopback ip:
    @{{ proven_cli }} network ipv4-is-loopback "{{ ip }}"

# Validate a filesystem path (checks for traversal attacks)
# Usage: just validate-path "../etc/passwd"
[doc("Check path for directory traversal")]
validate-path path:
    @{{ proven_cli }} path has-traversal "{{ path }}"

# Sanitize a filename (remove dangerous characters)
# Usage: just sanitize-filename "../../etc/passwd"
[doc("Sanitize a filename for safe filesystem use")]
sanitize-filename name:
    @{{ proven_cli }} path sanitize-filename "{{ name }}"

# Validate a JSON string
# Usage: just validate-json '{"key": "value"}'
[doc("Check if a string is valid JSON")]
validate-json input:
    @{{ proven_cli }} json is-valid "{{ input }}"

# Validate JSON from a file (reads stdin)
# Usage: just validate-json-file data.json
[doc("Validate JSON read from a file")]
validate-json-file file:
    @{{ proven_cli }} json is-valid "$(cat "{{ file }}")"

# Get JSON value type
# Usage: just json-type '42'
[doc("Determine the JSON value type")]
json-type input:
    @{{ proven_cli }} json get-type "{{ input }}"

# Validate password strength
# Usage: just validate-password "MyP@ssw0rd!"
[doc("Check password strength")]
validate-password password:
    @{{ proven_cli }} password validate "{{ password }}"

# Check if password is common (dictionary attack list)
# Usage: just is-common-password "password123"
[doc("Check if password appears in common password lists")]
is-common-password password:
    @{{ proven_cli }} password is-common "{{ password }}"

# Validate a phone number
# Usage: just validate-phone "+14155551234"
[doc("Parse and validate a phone number")]
validate-phone number:
    @{{ proven_cli }} phone parse "{{ number }}"

# ===========================================================================
# SafeString - Text operations
# ===========================================================================

# Sanitize a string (general-purpose input cleaning)
# Usage: just sanitize-string "<script>alert(1)</script>"
[doc("Sanitize string input (escape dangerous characters)")]
sanitize-string input:
    @{{ proven_cli }} string escape-html "{{ input }}"

# Escape string for SQL
# Usage: just escape-sql "Robert'; DROP TABLE students;--"
[doc("Escape a string for safe SQL interpolation")]
escape-sql input:
    @{{ proven_cli }} string escape-sql "{{ input }}"

# Escape string for JavaScript
# Usage: just escape-js "alert('xss')"
[doc("Escape a string for safe JS interpolation")]
escape-js input:
    @{{ proven_cli }} string escape-js "{{ input }}"

# Check if string is valid UTF-8
# Usage: just is-valid-utf8 "hello"
[doc("Check if a string is valid UTF-8")]
is-valid-utf8 input:
    @{{ proven_cli }} string is-valid-utf8 "{{ input }}"

# ===========================================================================
# SafeCrypto - Cryptographic primitives
# ===========================================================================

# SHA-256 hash of input
# Usage: just hash-sha256 "hello world"
[doc("Compute SHA-256 hash of input")]
hash-sha256 input:
    @{{ proven_cli }} crypto sha256 "{{ input }}"

# Constant-time string comparison (timing-attack safe)
# Usage: just constant-time-eq "secret" "guess"
[doc("Constant-time string comparison (timing-safe)")]
constant-time-eq a b:
    @{{ proven_cli }} crypto constant-time-eq "{{ a }}" "{{ b }}"

# Generate random bytes (hex-encoded)
# Usage: just random-hex 32
[doc("Generate N random bytes (hex-encoded)")]
random-hex nbytes="32":
    @{{ proven_cli }} crypto random-bytes {{ nbytes }}

# CRC32 checksum
# Usage: just checksum-crc32 "data"
[doc("Compute CRC32 checksum")]
checksum-crc32 input:
    @{{ proven_cli }} checksum crc32 "{{ input }}"

# ===========================================================================
# Hex encoding / decoding
# ===========================================================================

# Encode a string to hexadecimal
# Usage: just hex-encode "Hello"
[doc("Encode string to hexadecimal")]
hex-encode input:
    @{{ proven_cli }} hex encode "{{ input }}"

# Decode hexadecimal to string
# Usage: just hex-decode "48656c6c6f"
[doc("Decode hexadecimal to string")]
hex-decode input:
    @{{ proven_cli }} hex decode "{{ input }}"

# ===========================================================================
# SafeDateTime - ISO 8601 date/time
# ===========================================================================

# Parse an ISO 8601 datetime string
# Usage: just format-datetime "2026-01-15T10:30:00Z"
[doc("Parse and format an ISO 8601 datetime")]
format-datetime datetime:
    @{{ proven_cli }} datetime parse "{{ datetime }}"

# Check if a year is a leap year
# Usage: just is-leap-year 2024
[doc("Check if a year is a leap year")]
is-leap-year year:
    @{{ proven_cli }} datetime is-leap-year {{ year }}

# Get days in a given month
# Usage: just days-in-month 2024 2
[doc("Get the number of days in a month")]
days-in-month year month:
    @{{ proven_cli }} datetime days-in-month {{ year }} {{ month }}

# ===========================================================================
# SafeColor - Color parsing
# ===========================================================================

# Parse a hex color string
# Usage: just parse-color "#FF5733"
[doc("Parse a hex color string to RGB components")]
parse-color color:
    @{{ proven_cli }} color parse-hex "{{ color }}"

# ===========================================================================
# SafeVersion - Semantic versioning
# ===========================================================================

# Parse a semantic version string
# Usage: just parse-version "1.2.3-alpha"
[doc("Parse a semantic version string")]
parse-version version:
    @{{ proven_cli }} version parse "{{ version }}"

# Compare two semantic versions
# Usage: just version-compare "1.2.3" "1.3.0"
[doc("Compare two semantic versions (-1, 0, or 1)")]
version-compare a b:
    @{{ proven_cli }} version compare "{{ a }}" "{{ b }}"

# ===========================================================================
# SafeAngle - Angle conversions
# ===========================================================================

# Convert degrees to radians
# Usage: just deg-to-rad 180
[doc("Convert degrees to radians")]
deg-to-rad degrees:
    @{{ proven_cli }} angle deg-to-rad {{ degrees }}

# Convert radians to degrees
# Usage: just rad-to-deg 3.14159
[doc("Convert radians to degrees")]
rad-to-deg radians:
    @{{ proven_cli }} angle rad-to-deg {{ radians }}

# ===========================================================================
# SafeUnit - Physical unit conversions
# ===========================================================================

# Convert length between units
# Usage: just convert-length 5280 feet meters
[doc("Convert length between units")]
convert-length value from to:
    @{{ proven_cli }} unit convert-length {{ value }} {{ from }} {{ to }}

# Convert temperature between units
# Usage: just convert-temp 100 celsius fahrenheit
[doc("Convert temperature between units")]
convert-temp value from to:
    @{{ proven_cli }} unit convert-temp {{ value }} {{ from }} {{ to }}

# ===========================================================================
# SafeUUID - UUID generation
# ===========================================================================

# Generate a UUID v4
# Usage: just uuid-v4
[doc("Generate a random UUID v4")]
uuid-v4:
    @{{ proven_cli }} uuid v4

# Parse and validate a UUID
# Usage: just parse-uuid "550e8400-e29b-41d4-a716-446655440000"
[doc("Parse and validate a UUID string")]
parse-uuid uuid:
    @{{ proven_cli }} uuid parse "{{ uuid }}"

# ===========================================================================
# SafeCurrency - Monetary values
# ===========================================================================

# Parse a currency amount
# Usage: just parse-currency "USD 123.45"
[doc("Parse a currency amount string")]
parse-currency amount:
    @{{ proven_cli }} currency parse "{{ amount }}"

# ===========================================================================
# Calculator - Expression evaluation
# ===========================================================================

# Evaluate an arithmetic expression safely
# Usage: just calc-eval "2 + 3 * 4"
[doc("Evaluate an arithmetic expression safely")]
calc-eval expression:
    @{{ proven_cli }} calculator eval "{{ expression }}"
