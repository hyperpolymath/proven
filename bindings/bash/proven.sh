#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven Safety Library - Bash binding
#
# Thin CLI wrapper: ALL computation delegates to libproven (Idris 2 + Zig)
# via the proven-cli binary. This file does NOT reimplement any logic.
#
# Prerequisites:
#   - proven-cli must be on PATH (built from ffi/zig/)
#   - Alternatively, set PROVEN_CLI to the path of the binary
#
# Usage:
#   source /path/to/proven.sh
#   safe_div 10 2 && echo "$PROVEN_RESULT"
#
# All functions set:
#   PROVEN_RESULT  - the output value on success
#   PROVEN_ERROR   - the error description on failure
#   return code    - 0 on success, 1 on failure

# ---------------------------------------------------------------------------
# CLI binary resolution
# ---------------------------------------------------------------------------

PROVEN_CLI="${PROVEN_CLI:-proven-cli}"

# Verify the CLI binary is available
_proven_check_cli() {
    if ! command -v "$PROVEN_CLI" >/dev/null 2>&1; then
        PROVEN_ERROR="proven-cli not found. Install from ffi/zig/ or set PROVEN_CLI."
        return 1
    fi
    return 0
}

# Internal: call proven-cli and parse the JSON output
# Usage: _proven_call <module> <function> [args...]
# Sets PROVEN_RESULT and PROVEN_ERROR
_proven_call() {
    PROVEN_RESULT=""
    PROVEN_ERROR=""

    if ! _proven_check_cli; then
        return 1
    fi

    local output
    output=$("$PROVEN_CLI" "$@" 2>&1)
    local exit_code=$?

    if [[ $exit_code -ne 0 ]]; then
        PROVEN_ERROR="$output"
        return 1
    fi

    PROVEN_RESULT="$output"
    return 0
}

# ---------------------------------------------------------------------------
# SafeMath - Arithmetic that cannot crash
# ---------------------------------------------------------------------------

# Safe division
# Usage: safe_div 10 2 && echo "$PROVEN_RESULT"
safe_div() {
    _proven_call math div "$1" "$2"
}

# Safe modulo
# Usage: safe_mod 10 3 && echo "$PROVEN_RESULT"
safe_mod() {
    _proven_call math mod "$1" "$2"
}

# Checked addition
# Usage: safe_add 5 3 && echo "$PROVEN_RESULT"
safe_add() {
    _proven_call math add "$1" "$2"
}

# Checked subtraction
# Usage: safe_sub 10 3 && echo "$PROVEN_RESULT"
safe_sub() {
    _proven_call math sub "$1" "$2"
}

# Checked multiplication
# Usage: safe_mul 5 3 && echo "$PROVEN_RESULT"
safe_mul() {
    _proven_call math mul "$1" "$2"
}

# Safe absolute value
# Usage: safe_abs -5 && echo "$PROVEN_RESULT"
safe_abs() {
    _proven_call math abs "$1"
}

# Clamp value to range
# Usage: safe_clamp 0 100 150 && echo "$PROVEN_RESULT"
safe_clamp() {
    _proven_call math clamp "$1" "$2" "$3"
}

# Checked exponentiation
# Usage: safe_pow 2 10 && echo "$PROVEN_RESULT"
safe_pow() {
    _proven_call math pow "$1" "$2"
}

# ---------------------------------------------------------------------------
# SafeString - Text operations that handle encoding safely
# ---------------------------------------------------------------------------

# Check if input is valid UTF-8
# Usage: is_valid_utf8 "$string" && echo "valid"
is_valid_utf8() {
    _proven_call string is-valid-utf8 "$1"
}

# Escape string for SQL
# Usage: safe_sql=$(escape_sql "$user_input")
escape_sql() {
    _proven_call string escape-sql "$1"
}

# Escape string for HTML (prevent XSS)
# Usage: safe_html=$(escape_html "$user_input")
escape_html() {
    _proven_call string escape-html "$1"
}

# Escape string for JavaScript
# Usage: safe_js=$(escape_js "$user_input")
escape_js() {
    _proven_call string escape-js "$1"
}

# ---------------------------------------------------------------------------
# SafePath - Filesystem traversal prevention
# ---------------------------------------------------------------------------

# Check if path contains directory traversal
# Usage: has_traversal "../etc/passwd" && echo "DANGER"
has_traversal() {
    _proven_call path has-traversal "$1"
}

# Sanitize a filename
# Usage: safe_name=$(sanitize_filename "$input")
sanitize_filename() {
    _proven_call path sanitize-filename "$1"
}

# ---------------------------------------------------------------------------
# SafeCrypto - Cryptographic primitives
# ---------------------------------------------------------------------------

# Constant-time comparison
# Usage: constant_time_compare "$secret" "$input" && echo "match"
constant_time_compare() {
    _proven_call crypto constant-time-eq "$1" "$2"
}

# Generate random bytes (hex encoded)
# Usage: random=$(random_hex 32)
random_hex() {
    _proven_call crypto random-bytes "${1:-32}"
}

# ---------------------------------------------------------------------------
# SafeEmail - Email validation
# ---------------------------------------------------------------------------

# Validate email address
# Usage: is_valid_email "user@example.com" && echo "valid"
is_valid_email() {
    _proven_call email is-valid "$1"
}

# ---------------------------------------------------------------------------
# SafeUrl - URL parsing
# ---------------------------------------------------------------------------

# Parse URL
# Usage: parse_url "https://example.com/path"
parse_url() {
    _proven_call url parse "$1"
}

# ---------------------------------------------------------------------------
# SafeNetwork - IP address operations
# ---------------------------------------------------------------------------

# Parse IPv4 address
# Usage: parse_ipv4 "192.168.1.1"
parse_ipv4() {
    _proven_call network parse-ipv4 "$1"
}

# Check if IPv4 is private
# Usage: is_private_ip "10.0.0.1" && echo "private"
is_private_ip() {
    _proven_call network ipv4-is-private "$1"
}

# Check if IPv4 is loopback
# Usage: is_loopback "127.0.0.1" && echo "loopback"
is_loopback() {
    _proven_call network ipv4-is-loopback "$1"
}

# ---------------------------------------------------------------------------
# SafeJson - JSON validation
# ---------------------------------------------------------------------------

# Check if string is valid JSON
# Usage: is_valid_json '{"key": "value"}' && echo "valid"
is_valid_json() {
    _proven_call json is-valid "$1"
}

# Get JSON value type
# Usage: json_type=$(get_json_type '42')
get_json_type() {
    _proven_call json get-type "$1"
}

# ---------------------------------------------------------------------------
# SafeUUID - UUID generation and validation
# ---------------------------------------------------------------------------

# Generate UUID v4
# Usage: uuid=$(uuid_v4)
uuid_v4() {
    _proven_call uuid v4
}

# Parse UUID
# Usage: parse_uuid "550e8400-e29b-41d4-a716-446655440000"
parse_uuid() {
    _proven_call uuid parse "$1"
}

# ---------------------------------------------------------------------------
# SafeFloat - Safe floating-point operations
# ---------------------------------------------------------------------------

# Safe float division
# Usage: float_div 10.0 3.0 && echo "$PROVEN_RESULT"
float_div() {
    _proven_call float div "$1" "$2"
}

# Safe square root
# Usage: float_sqrt 16.0 && echo "$PROVEN_RESULT"
float_sqrt() {
    _proven_call float sqrt "$1"
}

# Safe natural log
# Usage: float_ln 2.718 && echo "$PROVEN_RESULT"
float_ln() {
    _proven_call float ln "$1"
}

# ---------------------------------------------------------------------------
# SafeDateTime - ISO 8601 date/time
# ---------------------------------------------------------------------------

# Parse ISO 8601 date
# Usage: parse_datetime "2026-01-15T10:30:00Z"
parse_datetime() {
    _proven_call datetime parse "$1"
}

# Check if leap year
# Usage: is_leap_year 2024 && echo "leap year"
is_leap_year() {
    _proven_call datetime is-leap-year "$1"
}

# Days in month
# Usage: days=$(days_in_month 2024 2)
days_in_month() {
    _proven_call datetime days-in-month "$1" "$2"
}

# ---------------------------------------------------------------------------
# SafeHex - Hexadecimal encoding/decoding
# ---------------------------------------------------------------------------

# Encode bytes to hex
# Usage: hex=$(hex_encode "$data")
hex_encode() {
    _proven_call hex encode "$1"
}

# Decode hex to bytes
# Usage: data=$(hex_decode "48656c6c6f")
hex_decode() {
    _proven_call hex decode "$1"
}

# ---------------------------------------------------------------------------
# SafeVersion - Semantic versioning
# ---------------------------------------------------------------------------

# Parse semantic version
# Usage: parse_version "1.2.3-alpha"
parse_version() {
    _proven_call version parse "$1"
}

# Compare versions
# Usage: compare_version "1.2.3" "1.3.0"
compare_version() {
    _proven_call version compare "$1" "$2"
}

# ---------------------------------------------------------------------------
# SafePassword - Password validation
# ---------------------------------------------------------------------------

# Validate password strength
# Usage: validate_password "MyP@ssw0rd!"
validate_password() {
    _proven_call password validate "$1"
}

# Check if password is common
# Usage: is_common_password "password123" && echo "common"
is_common_password() {
    _proven_call password is-common "$1"
}

# ---------------------------------------------------------------------------
# SafeChecksum - CRC and hash verification
# ---------------------------------------------------------------------------

# Calculate CRC32
# Usage: crc=$(checksum_crc32 "$data")
checksum_crc32() {
    _proven_call checksum crc32 "$1"
}

# ---------------------------------------------------------------------------
# SafeAngle - Angle conversions
# ---------------------------------------------------------------------------

# Convert degrees to radians
# Usage: rad=$(deg_to_rad 180)
deg_to_rad() {
    _proven_call angle deg-to-rad "$1"
}

# Convert radians to degrees
# Usage: deg=$(rad_to_deg 3.14159)
rad_to_deg() {
    _proven_call angle rad-to-deg "$1"
}

# ---------------------------------------------------------------------------
# SafeUnit - Physical unit conversions
# ---------------------------------------------------------------------------

# Convert length
# Usage: meters=$(convert_length 5280 feet meters)
convert_length() {
    _proven_call unit convert-length "$1" "$2" "$3"
}

# Convert temperature
# Usage: fahrenheit=$(convert_temp 100 celsius fahrenheit)
convert_temp() {
    _proven_call unit convert-temp "$1" "$2" "$3"
}

# ---------------------------------------------------------------------------
# SafeColor - Color operations
# ---------------------------------------------------------------------------

# Parse hex color
# Usage: parse_color "#FF5733"
parse_color() {
    _proven_call color parse-hex "$1"
}

# ---------------------------------------------------------------------------
# SafePhone - Phone number operations
# ---------------------------------------------------------------------------

# Parse phone number
# Usage: parse_phone "+14155551234"
parse_phone() {
    _proven_call phone parse "$1"
}

# ---------------------------------------------------------------------------
# SafeCurrency - Monetary values
# ---------------------------------------------------------------------------

# Parse currency amount
# Usage: parse_currency "USD 123.45"
parse_currency() {
    _proven_call currency parse "$1"
}

# ---------------------------------------------------------------------------
# SafeCalculator - Expression evaluation
# ---------------------------------------------------------------------------

# Evaluate arithmetic expression
# Usage: result=$(calc_eval "2 + 3 * 4")
calc_eval() {
    _proven_call calculator eval "$1"
}

# ---------------------------------------------------------------------------
# Library info
# ---------------------------------------------------------------------------

# Get library version
# Usage: ver=$(proven_version)
proven_version() {
    _proven_call version info
}

# Get module count
# Usage: count=$(proven_module_count)
proven_module_count() {
    _proven_call version module-count
}
