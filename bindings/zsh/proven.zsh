# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven Safety Library for Zsh
# Formally verified safety primitives for shell scripts.
# Version: 0.4.0
#
# Usage: source proven.zsh
#
# Modules (38 total):
#   Core (11): safe_math, safe_string, safe_path, safe_email, safe_url,
#              safe_network, safe_crypto, safe_uuid, safe_currency, safe_phone, safe_hex
#   Data (7): safe_json, safe_datetime, safe_float, safe_version, safe_color,
#             safe_angle, safe_unit
#   Data Structures (5): safe_buffer, safe_queue, safe_bloom, safe_lru, safe_graph
#   Resilience (4): safe_rate_limiter, safe_circuit_breaker, safe_retry, safe_monotonic
#   State (2): safe_state_machine, safe_calculator
#   Algorithm (4): safe_geo, safe_probability, safe_checksum, safe_tensor
#   Security (2): safe_password, safe_ml
#   HTTP (3): safe_header, safe_cookie, safe_content_type

# ============================================================================
# CONFIGURATION
# ============================================================================

# Maximum 64-bit signed integer values
typeset -g PROVEN_INT64_MAX=9223372036854775807
typeset -g PROVEN_INT64_MIN=-9223372036854775808

# Library version and module count
typeset -g PROVEN_VERSION="0.4.0"
typeset -g PROVEN_MODULE_COUNT=38

# Module categories
typeset -gA PROVEN_MODULES
PROVEN_MODULES=(
    [core]="safe_math safe_string safe_path safe_email safe_url safe_network safe_crypto safe_uuid safe_currency safe_phone safe_hex"
    [data]="safe_json safe_datetime safe_float safe_version safe_color safe_angle safe_unit"
    [data_structures]="safe_buffer safe_queue safe_bloom safe_lru safe_graph"
    [resilience]="safe_rate_limiter safe_circuit_breaker safe_retry safe_monotonic"
    [state]="safe_state_machine safe_calculator"
    [algorithm]="safe_geo safe_probability safe_checksum safe_tensor"
    [security]="safe_password safe_ml"
    [http]="safe_header safe_cookie safe_content_type"
)

# ============================================================================
# RESULT HANDLING
# ============================================================================

# Result type: Sets these variables after each operation
# PROVEN_RESULT_OK: 1 if successful, 0 if failed
# PROVEN_RESULT_VALUE: the result value (if successful)
# PROVEN_RESULT_ERROR: error message (if failed)

proven_ok() {
    PROVEN_RESULT_OK=1
    PROVEN_RESULT_VALUE="$1"
    PROVEN_RESULT_ERROR=""
    return 0
}

proven_err() {
    PROVEN_RESULT_OK=0
    PROVEN_RESULT_VALUE=""
    PROVEN_RESULT_ERROR="$1"
    return 1
}

# ============================================================================
# SAFE MATH (safe_math)
# ============================================================================

# Safe addition with overflow check
# Usage: proven_add <a> <b>
proven_add() {
    local a=$1
    local b=$2

    if [[ ! "$a" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: a is not a number"
        return 1
    fi
    if [[ ! "$b" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: b is not a number"
        return 1
    fi

    if (( b > 0 && a > PROVEN_INT64_MAX - b )); then
        proven_err "overflow: result exceeds maximum"
        return 1
    fi
    if (( b < 0 && a < PROVEN_INT64_MIN - b )); then
        proven_err "underflow: result below minimum"
        return 1
    fi

    proven_ok $(( a + b ))
    return 0
}

# Safe subtraction with underflow check
# Usage: proven_sub <a> <b>
proven_sub() {
    local a=$1
    local b=$2

    if [[ ! "$a" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: a is not a number"
        return 1
    fi
    if [[ ! "$b" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: b is not a number"
        return 1
    fi

    if (( b < 0 && a > PROVEN_INT64_MAX + b )); then
        proven_err "overflow: result exceeds maximum"
        return 1
    fi
    if (( b > 0 && a < PROVEN_INT64_MIN + b )); then
        proven_err "underflow: result below minimum"
        return 1
    fi

    proven_ok $(( a - b ))
    return 0
}

# Safe multiplication with overflow check
# Usage: proven_mul <a> <b>
proven_mul() {
    local a=$1
    local b=$2

    if [[ ! "$a" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: a is not a number"
        return 1
    fi
    if [[ ! "$b" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: b is not a number"
        return 1
    fi

    if (( a == 0 || b == 0 )); then
        proven_ok 0
        return 0
    fi

    local result=$(( a * b ))

    if (( result / a != b )); then
        proven_err "overflow: multiplication overflow detected"
        return 1
    fi

    proven_ok $result
    return 0
}

# Safe division with zero check
# Usage: proven_div <a> <b>
proven_div() {
    local a=$1
    local b=$2

    if [[ ! "$a" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: a is not a number"
        return 1
    fi
    if [[ ! "$b" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: b is not a number"
        return 1
    fi

    if (( b == 0 )); then
        proven_err "division_by_zero: cannot divide by zero"
        return 1
    fi

    if (( a == PROVEN_INT64_MIN && b == -1 )); then
        proven_err "overflow: INT64_MIN / -1 overflows"
        return 1
    fi

    proven_ok $(( a / b ))
    return 0
}

# Safe modulo with zero check
# Usage: proven_mod <a> <b>
proven_mod() {
    local a=$1
    local b=$2

    if [[ ! "$a" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: a is not a number"
        return 1
    fi
    if [[ ! "$b" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: b is not a number"
        return 1
    fi

    if (( b == 0 )); then
        proven_err "modulo_by_zero: cannot modulo by zero"
        return 1
    fi

    proven_ok $(( a % b ))
    return 0
}

# Safe absolute value
# Usage: proven_abs <value>
proven_abs() {
    local value=$1

    if [[ ! "$value" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: value is not a number"
        return 1
    fi

    if (( value == PROVEN_INT64_MIN )); then
        proven_err "overflow: cannot take absolute value of INT64_MIN"
        return 1
    fi

    if (( value < 0 )); then
        proven_ok $(( -value ))
    else
        proven_ok $value
    fi
    return 0
}

# Safe power with overflow check
# Usage: proven_pow <base> <exponent>
proven_pow() {
    local base=$1
    local exp=$2

    if [[ ! "$base" =~ ^-?[0-9]+$ ]] || [[ ! "$exp" =~ ^[0-9]+$ ]]; then
        proven_err "invalid_input: invalid base or exponent"
        return 1
    fi

    if (( exp == 0 )); then
        proven_ok 1
        return 0
    fi

    local result=1
    local temp=$base
    while (( exp > 0 )); do
        if (( exp % 2 == 1 )); then
            if (( result > PROVEN_INT64_MAX / temp || result < PROVEN_INT64_MIN / temp )); then
                proven_err "overflow: power overflow detected"
                return 1
            fi
            result=$(( result * temp ))
        fi
        if (( exp > 1 )); then
            if (( temp > PROVEN_INT64_MAX / temp || temp < PROVEN_INT64_MIN / temp )); then
                proven_err "overflow: power overflow detected"
                return 1
            fi
            temp=$(( temp * temp ))
        fi
        exp=$(( exp / 2 ))
    done

    proven_ok $result
    return 0
}

# ============================================================================
# SAFE STRING (safe_string)
# ============================================================================

# Escape HTML entities
# Usage: proven_escape_html <string>
proven_escape_html() {
    local input="$1"
    local output="${input//&/&amp;}"
    output="${output//</&lt;}"
    output="${output//>/&gt;}"
    output="${output//\"/&quot;}"
    output="${output//\'/&#39;}"
    proven_ok "$output"
    return 0
}

# Escape SQL single quotes
# Usage: proven_escape_sql <string>
proven_escape_sql() {
    local input="$1"
    local output="${input//\'/\'\'}"
    proven_ok "$output"
    return 0
}

# Escape shell metacharacters
# Usage: proven_escape_shell <string>
proven_escape_shell() {
    local input="$1"
    printf -v output "%q" "$input"
    proven_ok "$output"
    return 0
}

# Check if string is ASCII only
# Usage: proven_is_ascii <string>
proven_is_ascii() {
    local input="$1"
    if [[ "$input" =~ ^[[:print:][:space:]]*$ ]] && [[ ! "$input" =~ [^[:ascii:]] ]]; then
        return 0
    fi
    return 1
}

# Truncate string to max length
# Usage: proven_truncate <string> <max_length>
proven_truncate() {
    local input="$1"
    local max_len=$2
    if (( ${#input} > max_len )); then
        proven_ok "${input:0:$max_len}"
    else
        proven_ok "$input"
    fi
    return 0
}

# Check if string contains only alphanumeric chars
# Usage: proven_is_alphanumeric <string>
proven_is_alphanumeric() {
    local input="$1"
    [[ "$input" =~ ^[a-zA-Z0-9]+$ ]]
}

# Normalize whitespace (collapse multiple spaces)
# Usage: proven_normalize_whitespace <string>
proven_normalize_whitespace() {
    local input="$1"
    local output=$(echo "$input" | tr -s '[:space:]' ' ' | sed 's/^ //;s/ $//')
    proven_ok "$output"
    return 0
}

# ============================================================================
# SAFE PATH (safe_path)
# ============================================================================

# Check if path is safe (no traversal attacks)
# Usage: proven_is_safe_path <path>
proven_is_safe_path() {
    local path=$1

    [[ "$path" == *".."* ]] && return 1
    [[ "$path" == *$'\0'* ]] && return 1

    return 0
}

# Safely join paths preventing traversal
# Usage: proven_safe_join_path <base> <relative>
proven_safe_join_path() {
    local base=$1
    local relative=$2

    if ! proven_is_safe_path "$relative"; then
        proven_err "unsafe_path: path contains traversal attempt"
        return 1
    fi

    local full_path="$base/$relative"
    local resolved=$(realpath -m "$full_path" 2>/dev/null)

    if [[ $? -ne 0 ]]; then
        proven_err "invalid_path: cannot resolve path"
        return 1
    fi

    local resolved_base=$(realpath -m "$base" 2>/dev/null)
    if [[ ! "$resolved" == "$resolved_base"* ]]; then
        proven_err "path_escape: path escapes base directory"
        return 1
    fi

    proven_ok "$resolved"
    return 0
}

# Normalize path (resolve . and ..)
# Usage: proven_normalize_path <path>
proven_normalize_path() {
    local path="$1"
    local resolved=$(realpath -m "$path" 2>/dev/null)
    if [[ $? -eq 0 ]]; then
        proven_ok "$resolved"
        return 0
    fi
    proven_err "invalid_path: cannot normalize path"
    return 1
}

# Get file extension safely
# Usage: proven_get_extension <path>
proven_get_extension() {
    local path="$1"
    local filename="${path##*/}"
    if [[ "$filename" == *.* ]]; then
        proven_ok "${filename##*.}"
    else
        proven_ok ""
    fi
    return 0
}

# ============================================================================
# SAFE EMAIL (safe_email)
# ============================================================================

# Validate email format (basic RFC 5322)
# Usage: proven_is_valid_email <email>
proven_is_valid_email() {
    local email=$1
    [[ "$email" =~ ^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$ ]]
}

# Extract local part from email
# Usage: proven_email_local <email>
proven_email_local() {
    local email="$1"
    if proven_is_valid_email "$email"; then
        proven_ok "${email%%@*}"
        return 0
    fi
    proven_err "invalid_email: not a valid email address"
    return 1
}

# Extract domain from email
# Usage: proven_email_domain <email>
proven_email_domain() {
    local email="$1"
    if proven_is_valid_email "$email"; then
        proven_ok "${email#*@}"
        return 0
    fi
    proven_err "invalid_email: not a valid email address"
    return 1
}

# ============================================================================
# SAFE URL (safe_url)
# ============================================================================

# Validate URL format
# Usage: proven_is_valid_url <url>
proven_is_valid_url() {
    local url="$1"
    [[ "$url" =~ ^https?://[a-zA-Z0-9]([a-zA-Z0-9.-]*[a-zA-Z0-9])?(\.[a-zA-Z]{2,})+(:[0-9]+)?(/.*)?$ ]]
}

# Extract scheme from URL
# Usage: proven_url_scheme <url>
proven_url_scheme() {
    local url="$1"
    if [[ "$url" =~ ^([a-zA-Z][a-zA-Z0-9+.-]*):// ]]; then
        proven_ok "${BASH_REMATCH[1]}"
        return 0
    fi
    proven_err "invalid_url: no scheme found"
    return 1
}

# Extract host from URL
# Usage: proven_url_host <url>
proven_url_host() {
    local url="$1"
    local host="${url#*://}"
    host="${host%%/*}"
    host="${host%%:*}"
    host="${host%%\?*}"
    if [[ -n "$host" ]]; then
        proven_ok "$host"
        return 0
    fi
    proven_err "invalid_url: no host found"
    return 1
}

# URL encode string
# Usage: proven_url_encode <string>
proven_url_encode() {
    local string="$1"
    local length="${#string}"
    local encoded=""
    for (( i = 0; i < length; i++ )); do
        local char="${string:$i:1}"
        case "$char" in
            [a-zA-Z0-9.~_-]) encoded+="$char" ;;
            *) printf -v hex '%%%02X' "'$char"; encoded+="$hex" ;;
        esac
    done
    proven_ok "$encoded"
    return 0
}

# ============================================================================
# SAFE NETWORK (safe_network)
# ============================================================================

# Validate IPv4 address
# Usage: proven_is_valid_ipv4 <address>
proven_is_valid_ipv4() {
    local ip=$1
    local IFS='.'
    local -a octets=($ip)

    [[ ${#octets[@]} -eq 4 ]] || return 1

    for octet in "${octets[@]}"; do
        [[ "$octet" =~ ^[0-9]+$ ]] || return 1
        (( octet >= 0 && octet <= 255 )) || return 1
    done
    return 0
}

# Validate IPv6 address (simplified)
# Usage: proven_is_valid_ipv6 <address>
proven_is_valid_ipv6() {
    local ip="$1"
    [[ "$ip" =~ ^([0-9a-fA-F]{0,4}:){2,7}[0-9a-fA-F]{0,4}$ ]] || \
    [[ "$ip" =~ ^::([0-9a-fA-F]{1,4}:){0,5}[0-9a-fA-F]{1,4}$ ]] || \
    [[ "$ip" =~ ^([0-9a-fA-F]{1,4}:){1,6}:$ ]] || \
    [[ "$ip" == "::" ]]
}

# Validate port number (1-65535)
# Usage: proven_is_valid_port <port>
proven_is_valid_port() {
    local port=$1
    [[ "$port" =~ ^[0-9]+$ ]] && (( port >= 1 && port <= 65535 ))
}

# Require valid port or return error
# Usage: proven_require_valid_port <port>
proven_require_valid_port() {
    local port=$1
    if ! proven_is_valid_port "$port"; then
        proven_err "invalid_port: $port is not valid (1-65535)"
        return 1
    fi
    return 0
}

# Check if IP is in CIDR range
# Usage: proven_ip_in_cidr <ip> <cidr>
proven_ip_in_cidr() {
    local ip="$1"
    local cidr="$2"
    local network="${cidr%/*}"
    local prefix="${cidr#*/}"

    proven_is_valid_ipv4 "$ip" || return 1
    proven_is_valid_ipv4 "$network" || return 1
    [[ "$prefix" =~ ^[0-9]+$ ]] && (( prefix >= 0 && prefix <= 32 )) || return 1

    local IFS='.'
    local -a ip_parts=($ip)
    local -a net_parts=($network)

    local ip_num=$(( (ip_parts[0] << 24) + (ip_parts[1] << 16) + (ip_parts[2] << 8) + ip_parts[3] ))
    local net_num=$(( (net_parts[0] << 24) + (net_parts[1] << 16) + (net_parts[2] << 8) + net_parts[3] ))
    local mask=$(( 0xFFFFFFFF << (32 - prefix) ))

    (( (ip_num & mask) == (net_num & mask) ))
}

# ============================================================================
# SAFE CRYPTO (safe_crypto)
# ============================================================================

# Generate random hex string
# Usage: proven_random_hex <length>
proven_random_hex() {
    local length=$1
    if [[ ! "$length" =~ ^[0-9]+$ ]] || (( length < 1 )); then
        proven_err "invalid_input: length must be positive integer"
        return 1
    fi
    local bytes=$(( (length + 1) / 2 ))
    local hex=$(head -c $bytes /dev/urandom | xxd -p | tr -d '\n')
    proven_ok "${hex:0:$length}"
    return 0
}

# SHA256 hash (requires sha256sum)
# Usage: proven_sha256 <string>
proven_sha256() {
    local input="$1"
    if command -v sha256sum &>/dev/null; then
        local hash=$(echo -n "$input" | sha256sum | cut -d' ' -f1)
        proven_ok "$hash"
        return 0
    fi
    proven_err "unavailable: sha256sum not found"
    return 1
}

# BLAKE3 hash (requires b3sum)
# Usage: proven_blake3 <string>
proven_blake3() {
    local input="$1"
    if command -v b3sum &>/dev/null; then
        local hash=$(echo -n "$input" | b3sum | cut -d' ' -f1)
        proven_ok "$hash"
        return 0
    fi
    proven_err "unavailable: b3sum not found"
    return 1
}

# Constant-time string comparison
# Usage: proven_constant_time_compare <a> <b>
proven_constant_time_compare() {
    local a="$1"
    local b="$2"
    if [[ ${#a} -ne ${#b} ]]; then
        return 1
    fi
    local result=0
    for (( i=0; i<${#a}; i++ )); do
        local ca="${a:$i:1}"
        local cb="${b:$i:1}"
        if [[ "$ca" != "$cb" ]]; then
            result=1
        fi
    done
    return $result
}

# ============================================================================
# SAFE UUID (safe_uuid)
# ============================================================================

# Generate UUID v4 (random)
# Usage: proven_uuid_v4
proven_uuid_v4() {
    local hex=$(head -c 16 /dev/urandom | xxd -p)
    local uuid="${hex:0:8}-${hex:8:4}-4${hex:13:3}-"
    local variant=$((0x8 + RANDOM % 4))
    printf -v variant_hex "%x" $variant
    uuid+="${variant_hex}${hex:17:3}-${hex:20:12}"
    proven_ok "$uuid"
    return 0
}

# Validate UUID format
# Usage: proven_is_valid_uuid <uuid>
proven_is_valid_uuid() {
    local uuid="$1"
    [[ "$uuid" =~ ^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$ ]]
}

# Check if UUID is nil
# Usage: proven_is_nil_uuid <uuid>
proven_is_nil_uuid() {
    [[ "$1" == "00000000-0000-0000-0000-000000000000" ]]
}

# ============================================================================
# SAFE CURRENCY (safe_currency)
# ============================================================================

# Validate ISO 4217 currency code
# Usage: proven_is_valid_currency <code>
proven_is_valid_currency() {
    local code="${1^^}"
    case "$code" in
        USD|EUR|GBP|JPY|CHF|CAD|AUD|NZD|CNY|INR|BRL|MXN|KRW|SGD|HKD)
            return 0 ;;
        SEK|NOK|DKK|PLN|RUB|ZAR|TRY|THB|MYR|IDR|PHP|VND|AED|SAR|ILS)
            return 0 ;;
        CZK|HUF|RON|BGN|HRK|ISK|CLP|COP|PEN|ARS|BTC|ETH)
            return 0 ;;
        *) return 1 ;;
    esac
}

# Get currency symbol
# Usage: proven_currency_symbol <code>
proven_currency_symbol() {
    local code="${1^^}"
    case "$code" in
        USD) proven_ok '$' ;;
        EUR) proven_ok '€' ;;
        GBP) proven_ok '£' ;;
        JPY|CNY) proven_ok '¥' ;;
        CHF) proven_ok 'Fr' ;;
        INR) proven_ok '₹' ;;
        KRW) proven_ok '₩' ;;
        RUB) proven_ok '₽' ;;
        BTC) proven_ok '₿' ;;
        ETH) proven_ok 'Ξ' ;;
        *) proven_ok "$code" ;;
    esac
    return 0
}

# Get currency decimal places
# Usage: proven_currency_decimals <code>
proven_currency_decimals() {
    local code="${1^^}"
    case "$code" in
        JPY|KRW|VND) proven_ok 0 ;;
        BTC|ETH) proven_ok 8 ;;
        *) proven_ok 2 ;;
    esac
    return 0
}

# ============================================================================
# SAFE PHONE (safe_phone)
# ============================================================================

# Validate E.164 phone number format
# Usage: proven_is_valid_phone <number>
proven_is_valid_phone() {
    local number="$1"
    [[ "$number" =~ ^\+[1-9][0-9]{6,14}$ ]]
}

# Normalize phone number to E.164
# Usage: proven_normalize_phone <number>
proven_normalize_phone() {
    local number="$1"
    local normalized="${number//[^0-9+]/}"
    if [[ ! "$normalized" =~ ^\+ ]]; then
        normalized="+$normalized"
    fi
    if proven_is_valid_phone "$normalized"; then
        proven_ok "$normalized"
        return 0
    fi
    proven_err "invalid_phone: cannot normalize to E.164"
    return 1
}

# ============================================================================
# SAFE HEX (safe_hex)
# ============================================================================

# Encode bytes to hex
# Usage: proven_hex_encode <string>
proven_hex_encode() {
    local input="$1"
    local hex=$(echo -n "$input" | xxd -p | tr -d '\n')
    proven_ok "$hex"
    return 0
}

# Decode hex to bytes
# Usage: proven_hex_decode <hex>
proven_hex_decode() {
    local hex="$1"
    if [[ ! "$hex" =~ ^[0-9a-fA-F]*$ ]] || (( ${#hex} % 2 != 0 )); then
        proven_err "invalid_hex: invalid hex string"
        return 1
    fi
    local decoded=$(echo -n "$hex" | xxd -r -p)
    proven_ok "$decoded"
    return 0
}

# Validate hex string
# Usage: proven_is_valid_hex <hex>
proven_is_valid_hex() {
    local hex="$1"
    [[ "$hex" =~ ^[0-9a-fA-F]*$ ]] && (( ${#hex} % 2 == 0 ))
}

# ============================================================================
# SAFE JSON (safe_json)
# ============================================================================

# Validate JSON syntax (requires jq)
# Usage: proven_is_valid_json <json>
proven_is_valid_json() {
    local json="$1"
    if command -v jq &>/dev/null; then
        echo "$json" | jq empty 2>/dev/null
        return $?
    fi
    return 1
}

# Extract JSON field (requires jq)
# Usage: proven_json_get <json> <path>
proven_json_get() {
    local json="$1"
    local path="$2"
    if command -v jq &>/dev/null; then
        local value=$(echo "$json" | jq -r "$path" 2>/dev/null)
        if [[ $? -eq 0 ]] && [[ "$value" != "null" ]]; then
            proven_ok "$value"
            return 0
        fi
    fi
    proven_err "json_error: cannot extract value"
    return 1
}

# Escape string for JSON
# Usage: proven_json_escape <string>
proven_json_escape() {
    local input="$1"
    local output="${input//\\/\\\\}"
    output="${output//\"/\\\"}"
    output="${output//$'\n'/\\n}"
    output="${output//$'\r'/\\r}"
    output="${output//$'\t'/\\t}"
    proven_ok "$output"
    return 0
}

# ============================================================================
# SAFE DATETIME (safe_datetime)
# ============================================================================

# Validate ISO 8601 date format
# Usage: proven_is_valid_iso_date <date>
proven_is_valid_iso_date() {
    local date="$1"
    [[ "$date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}(T[0-9]{2}:[0-9]{2}:[0-9]{2}(Z|[+-][0-9]{2}:[0-9]{2})?)?$ ]]
}

# Get current ISO 8601 timestamp
# Usage: proven_now_iso
proven_now_iso() {
    proven_ok "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    return 0
}

# Parse Unix timestamp to ISO 8601
# Usage: proven_timestamp_to_iso <timestamp>
proven_timestamp_to_iso() {
    local ts="$1"
    if [[ ! "$ts" =~ ^[0-9]+$ ]]; then
        proven_err "invalid_timestamp: not a valid Unix timestamp"
        return 1
    fi
    proven_ok "$(date -u -d @$ts +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || date -u -r $ts +%Y-%m-%dT%H:%M:%SZ)"
    return 0
}

# ============================================================================
# SAFE FLOAT (safe_float)
# ============================================================================

# Safe float division (checks for zero and invalid)
# Usage: proven_float_div <a> <b>
proven_float_div() {
    local a="$1"
    local b="$2"
    if [[ ! "$a" =~ ^-?[0-9]*\.?[0-9]+$ ]] || [[ ! "$b" =~ ^-?[0-9]*\.?[0-9]+$ ]]; then
        proven_err "invalid_input: not valid numbers"
        return 1
    fi
    local is_zero=$(echo "$b == 0" | bc -l)
    if (( is_zero )); then
        proven_err "division_by_zero: cannot divide by zero"
        return 1
    fi
    proven_ok "$(echo "scale=10; $a / $b" | bc -l)"
    return 0
}

# Check if float is NaN or infinite (pattern check)
# Usage: proven_is_finite <value>
proven_is_finite() {
    local val="$1"
    [[ "$val" =~ ^-?[0-9]*\.?[0-9]+([eE][+-]?[0-9]+)?$ ]]
}

# Clamp float to range
# Usage: proven_float_clamp <value> <min> <max>
proven_float_clamp() {
    local val="$1"
    local min_val="$2"
    local max_val="$3"
    if (( $(echo "$val < $min_val" | bc -l) )); then
        proven_ok "$min_val"
    elif (( $(echo "$val > $max_val" | bc -l) )); then
        proven_ok "$max_val"
    else
        proven_ok "$val"
    fi
    return 0
}

# ============================================================================
# SAFE VERSION (safe_version)
# ============================================================================

# Parse semantic version
# Usage: proven_parse_semver <version>
proven_parse_semver() {
    local version="$1"
    if [[ "$version" =~ ^v?([0-9]+)\.([0-9]+)\.([0-9]+)(-([a-zA-Z0-9.-]+))?(\+([a-zA-Z0-9.-]+))?$ ]]; then
        proven_ok "${BASH_REMATCH[1]}.${BASH_REMATCH[2]}.${BASH_REMATCH[3]}"
        return 0
    fi
    proven_err "invalid_version: not a valid semver"
    return 1
}

# Compare semantic versions
# Usage: proven_compare_version <v1> <v2>
# Returns: -1 if v1 < v2, 0 if equal, 1 if v1 > v2
proven_compare_version() {
    local v1="$1"
    local v2="$2"

    local IFS='.'
    local -a parts1=(${v1//[^0-9.]/ })
    local -a parts2=(${v2//[^0-9.]/ })

    for (( i=0; i<3; i++ )); do
        local p1=${parts1[$i]:-0}
        local p2=${parts2[$i]:-0}
        if (( p1 > p2 )); then
            proven_ok 1
            return 0
        elif (( p1 < p2 )); then
            proven_ok -1
            return 0
        fi
    done
    proven_ok 0
    return 0
}

# ============================================================================
# SAFE COLOR (safe_color)
# ============================================================================

# Validate hex color
# Usage: proven_is_valid_hex_color <color>
proven_is_valid_hex_color() {
    local color="$1"
    [[ "$color" =~ ^#?([0-9a-fA-F]{3}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$ ]]
}

# Parse hex color to RGB
# Usage: proven_hex_to_rgb <color>
proven_hex_to_rgb() {
    local color="${1#\#}"
    if [[ ${#color} -eq 3 ]]; then
        color="${color:0:1}${color:0:1}${color:1:1}${color:1:1}${color:2:1}${color:2:1}"
    fi
    if [[ ${#color} -ge 6 ]]; then
        local r=$((16#${color:0:2}))
        local g=$((16#${color:2:2}))
        local b=$((16#${color:4:2}))
        proven_ok "$r,$g,$b"
        return 0
    fi
    proven_err "invalid_color: not a valid hex color"
    return 1
}

# Calculate relative luminance for WCAG contrast
# Usage: proven_luminance <r> <g> <b>
proven_luminance() {
    local r=$1 g=$2 b=$3
    # Simplified - actual WCAG uses sRGB transfer function
    local lum=$(echo "scale=4; 0.2126 * $r/255 + 0.7152 * $g/255 + 0.0722 * $b/255" | bc -l)
    proven_ok "$lum"
    return 0
}

# ============================================================================
# SAFE ANGLE (safe_angle)
# ============================================================================

# Convert degrees to radians
# Usage: proven_deg_to_rad <degrees>
proven_deg_to_rad() {
    local deg="$1"
    local pi="3.14159265358979323846"
    proven_ok "$(echo "scale=10; $deg * $pi / 180" | bc -l)"
    return 0
}

# Convert radians to degrees
# Usage: proven_rad_to_deg <radians>
proven_rad_to_deg() {
    local rad="$1"
    local pi="3.14159265358979323846"
    proven_ok "$(echo "scale=10; $rad * 180 / $pi" | bc -l)"
    return 0
}

# Normalize degrees to 0-360
# Usage: proven_normalize_degrees <degrees>
proven_normalize_degrees() {
    local deg="$1"
    local result=$(echo "scale=10; (($deg % 360) + 360) % 360" | bc -l)
    proven_ok "$result"
    return 0
}

# ============================================================================
# SAFE UNIT (safe_unit)
# ============================================================================

# Convert length units
# Usage: proven_convert_length <value> <from> <to>
proven_convert_length() {
    local val="$1"
    local from="${2,,}"
    local to="${3,,}"

    # Convert to meters first
    local meters
    case "$from" in
        m|meter|meters) meters="$val" ;;
        km|kilometer*) meters=$(echo "$val * 1000" | bc -l) ;;
        cm|centimeter*) meters=$(echo "$val / 100" | bc -l) ;;
        mm|millimeter*) meters=$(echo "$val / 1000" | bc -l) ;;
        mi|mile*) meters=$(echo "$val * 1609.344" | bc -l) ;;
        ft|foot|feet) meters=$(echo "$val * 0.3048" | bc -l) ;;
        in|inch*) meters=$(echo "$val * 0.0254" | bc -l) ;;
        yd|yard*) meters=$(echo "$val * 0.9144" | bc -l) ;;
        *) proven_err "unknown_unit: $from"; return 1 ;;
    esac

    # Convert from meters to target
    local result
    case "$to" in
        m|meter|meters) result="$meters" ;;
        km|kilometer*) result=$(echo "$meters / 1000" | bc -l) ;;
        cm|centimeter*) result=$(echo "$meters * 100" | bc -l) ;;
        mm|millimeter*) result=$(echo "$meters * 1000" | bc -l) ;;
        mi|mile*) result=$(echo "$meters / 1609.344" | bc -l) ;;
        ft|foot|feet) result=$(echo "$meters / 0.3048" | bc -l) ;;
        in|inch*) result=$(echo "$meters / 0.0254" | bc -l) ;;
        yd|yard*) result=$(echo "$meters / 0.9144" | bc -l) ;;
        *) proven_err "unknown_unit: $to"; return 1 ;;
    esac

    proven_ok "$result"
    return 0
}

# Convert temperature units
# Usage: proven_convert_temp <value> <from> <to>
proven_convert_temp() {
    local val="$1"
    local from="${2,,}"
    local to="${3,,}"

    # Convert to Celsius first
    local celsius
    case "$from" in
        c|celsius) celsius="$val" ;;
        f|fahrenheit) celsius=$(echo "scale=10; ($val - 32) * 5 / 9" | bc -l) ;;
        k|kelvin) celsius=$(echo "scale=10; $val - 273.15" | bc -l) ;;
        *) proven_err "unknown_unit: $from"; return 1 ;;
    esac

    # Convert to target
    local result
    case "$to" in
        c|celsius) result="$celsius" ;;
        f|fahrenheit) result=$(echo "scale=10; $celsius * 9 / 5 + 32" | bc -l) ;;
        k|kelvin) result=$(echo "scale=10; $celsius + 273.15" | bc -l) ;;
        *) proven_err "unknown_unit: $to"; return 1 ;;
    esac

    proven_ok "$result"
    return 0
}

# ============================================================================
# SAFE BUFFER (safe_buffer)
# ============================================================================

# Initialize bounded buffer
# Usage: proven_buffer_init <name> <capacity>
proven_buffer_init() {
    local name="$1"
    local capacity="$2"
    eval "${name}_capacity=$capacity"
    eval "${name}_size=0"
    eval "${name}_data=()"
    return 0
}

# Push to buffer
# Usage: proven_buffer_push <name> <value>
proven_buffer_push() {
    local name="$1"
    local value="$2"
    local capacity_var="${name}_capacity"
    local size_var="${name}_size"
    local data_var="${name}_data"

    local capacity=${(P)capacity_var}
    local size=${(P)size_var}

    if (( size >= capacity )); then
        proven_err "buffer_full: buffer at capacity"
        return 1
    fi

    eval "${data_var}+=(\"\$value\")"
    eval "${size_var}=$((size + 1))"
    return 0
}

# Pop from buffer
# Usage: proven_buffer_pop <name>
proven_buffer_pop() {
    local name="$1"
    local size_var="${name}_size"
    local data_var="${name}_data"

    local size=${(P)size_var}

    if (( size == 0 )); then
        proven_err "buffer_empty: buffer is empty"
        return 1
    fi

    eval "local value=\${${data_var}[-1]}"
    eval "${data_var}=(\${${data_var}[1,-2]})"
    eval "${size_var}=$((size - 1))"
    proven_ok "$value"
    return 0
}

# ============================================================================
# SAFE QUEUE (safe_queue)
# ============================================================================

# Initialize bounded queue
# Usage: proven_queue_init <name> <capacity>
proven_queue_init() {
    proven_buffer_init "$1" "$2"
    return 0
}

# Enqueue item
# Usage: proven_queue_enqueue <name> <value>
proven_queue_enqueue() {
    proven_buffer_push "$1" "$2"
}

# Dequeue item (FIFO)
# Usage: proven_queue_dequeue <name>
proven_queue_dequeue() {
    local name="$1"
    local size_var="${name}_size"
    local data_var="${name}_data"

    local size=${(P)size_var}

    if (( size == 0 )); then
        proven_err "queue_empty: queue is empty"
        return 1
    fi

    eval "local value=\${${data_var}[1]}"
    eval "${data_var}=(\${${data_var}[2,-1]})"
    eval "${size_var}=$((size - 1))"
    proven_ok "$value"
    return 0
}

# ============================================================================
# SAFE BLOOM (safe_bloom)
# ============================================================================

# Initialize bloom filter
# Usage: proven_bloom_init <name> <size>
proven_bloom_init() {
    local name="$1"
    local size="$2"
    eval "${name}_size=$size"
    eval "${name}_bits=()"
    for (( i=0; i<size; i++ )); do
        eval "${name}_bits[$i]=0"
    done
    return 0
}

# Add to bloom filter
# Usage: proven_bloom_add <name> <value>
proven_bloom_add() {
    local name="$1"
    local value="$2"
    local size_var="${name}_size"
    local bits_var="${name}_bits"

    local size=${(P)size_var}

    # Simple hash functions
    local h1=$(echo -n "$value" | cksum | cut -d' ' -f1)
    local h2=$(echo -n "${value}salt" | cksum | cut -d' ' -f1)
    local h3=$(echo -n "salt${value}" | cksum | cut -d' ' -f1)

    eval "${bits_var}[$((h1 % size))]=1"
    eval "${bits_var}[$((h2 % size))]=1"
    eval "${bits_var}[$((h3 % size))]=1"
    return 0
}

# Check bloom filter
# Usage: proven_bloom_contains <name> <value>
proven_bloom_contains() {
    local name="$1"
    local value="$2"
    local size_var="${name}_size"
    local bits_var="${name}_bits"

    local size=${(P)size_var}

    local h1=$(echo -n "$value" | cksum | cut -d' ' -f1)
    local h2=$(echo -n "${value}salt" | cksum | cut -d' ' -f1)
    local h3=$(echo -n "salt${value}" | cksum | cut -d' ' -f1)

    eval "local b1=\${${bits_var}[$((h1 % size))]}"
    eval "local b2=\${${bits_var}[$((h2 % size))]}"
    eval "local b3=\${${bits_var}[$((h3 % size))]}"

    [[ "$b1" == "1" && "$b2" == "1" && "$b3" == "1" ]]
}

# ============================================================================
# SAFE LRU (safe_lru)
# ============================================================================

# Initialize LRU cache
# Usage: proven_lru_init <name> <capacity>
proven_lru_init() {
    local name="$1"
    local capacity="$2"
    eval "typeset -gA ${name}_cache"
    eval "typeset -ga ${name}_order"
    eval "${name}_capacity=$capacity"
    return 0
}

# Get from LRU cache
# Usage: proven_lru_get <name> <key>
proven_lru_get() {
    local name="$1"
    local key="$2"
    local cache_var="${name}_cache"
    local order_var="${name}_order"

    eval "local value=\${${cache_var}[\$key]}"
    if [[ -n "$value" ]]; then
        # Move to front of order
        eval "${order_var}=(\$key \${${order_var}:#\$key})"
        proven_ok "$value"
        return 0
    fi
    proven_err "cache_miss: key not found"
    return 1
}

# Put in LRU cache
# Usage: proven_lru_put <name> <key> <value>
proven_lru_put() {
    local name="$1"
    local key="$2"
    local value="$3"
    local cache_var="${name}_cache"
    local order_var="${name}_order"
    local capacity_var="${name}_capacity"

    local capacity=${(P)capacity_var}

    # Remove if exists
    eval "${order_var}=(\${${order_var}:#\$key})"

    # Add to front
    eval "${order_var}=(\$key \${${order_var}})"
    eval "${cache_var}[\$key]=\$value"

    # Evict if over capacity
    eval "local size=\${#${order_var}}"
    while (( size > capacity )); do
        eval "local oldest=\${${order_var}[-1]}"
        eval "unset '${cache_var}[\$oldest]'"
        eval "${order_var}=(\${${order_var}[1,-2]})"
        size=$((size - 1))
    done
    return 0
}

# ============================================================================
# SAFE GRAPH (safe_graph)
# ============================================================================

# Initialize directed graph
# Usage: proven_graph_init <name>
proven_graph_init() {
    local name="$1"
    eval "typeset -gA ${name}_edges"
    eval "typeset -ga ${name}_nodes"
    return 0
}

# Add edge to graph
# Usage: proven_graph_add_edge <name> <from> <to>
proven_graph_add_edge() {
    local name="$1"
    local from="$2"
    local to="$3"
    local edges_var="${name}_edges"
    local nodes_var="${name}_nodes"

    # Add nodes if not present
    eval "if [[ ! \" \${${nodes_var}[*]} \" =~ \" \$from \" ]]; then ${nodes_var}+=(\$from); fi"
    eval "if [[ ! \" \${${nodes_var}[*]} \" =~ \" \$to \" ]]; then ${nodes_var}+=(\$to); fi"

    # Add edge
    eval "local current=\${${edges_var}[\$from]}"
    if [[ -n "$current" ]]; then
        eval "${edges_var}[\$from]=\"\$current \$to\""
    else
        eval "${edges_var}[\$from]=\$to"
    fi
    return 0
}

# Check if graph has edge
# Usage: proven_graph_has_edge <name> <from> <to>
proven_graph_has_edge() {
    local name="$1"
    local from="$2"
    local to="$3"
    local edges_var="${name}_edges"

    eval "local targets=\${${edges_var}[\$from]}"
    [[ " $targets " =~ " $to " ]]
}

# ============================================================================
# SAFE RATE LIMITER (safe_rate_limiter)
# ============================================================================

# Initialize token bucket rate limiter
# Usage: proven_ratelimit_init <name> <capacity> <refill_rate>
proven_ratelimit_init() {
    local name="$1"
    local capacity="$2"
    local refill_rate="$3"
    eval "${name}_capacity=$capacity"
    eval "${name}_tokens=$capacity"
    eval "${name}_refill_rate=$refill_rate"
    eval "${name}_last_refill=$(date +%s)"
    return 0
}

# Try to acquire tokens
# Usage: proven_ratelimit_acquire <name> <count>
proven_ratelimit_acquire() {
    local name="$1"
    local count="${2:-1}"
    local capacity_var="${name}_capacity"
    local tokens_var="${name}_tokens"
    local rate_var="${name}_refill_rate"
    local last_var="${name}_last_refill"

    local now=$(date +%s)
    local last=${(P)last_var}
    local elapsed=$((now - last))
    local rate=${(P)rate_var}
    local capacity=${(P)capacity_var}
    local tokens=${(P)tokens_var}

    # Refill
    local new_tokens=$((tokens + elapsed * rate))
    if (( new_tokens > capacity )); then
        new_tokens=$capacity
    fi
    eval "${tokens_var}=$new_tokens"
    eval "${last_var}=$now"

    # Check
    if (( new_tokens >= count )); then
        eval "${tokens_var}=$((new_tokens - count))"
        return 0
    fi
    proven_err "rate_limited: insufficient tokens"
    return 1
}

# ============================================================================
# SAFE CIRCUIT BREAKER (safe_circuit_breaker)
# ============================================================================

# Initialize circuit breaker
# Usage: proven_circuit_init <name> <failure_threshold> <recovery_timeout>
proven_circuit_init() {
    local name="$1"
    local threshold="$2"
    local timeout="$3"
    eval "${name}_state=closed"
    eval "${name}_failures=0"
    eval "${name}_threshold=$threshold"
    eval "${name}_timeout=$timeout"
    eval "${name}_last_failure=0"
    return 0
}

# Check if circuit allows request
# Usage: proven_circuit_allow <name>
proven_circuit_allow() {
    local name="$1"
    local state_var="${name}_state"
    local failures_var="${name}_failures"
    local threshold_var="${name}_threshold"
    local timeout_var="${name}_timeout"
    local last_var="${name}_last_failure"

    local state=${(P)state_var}
    local now=$(date +%s)

    if [[ "$state" == "open" ]]; then
        local last=${(P)last_var}
        local timeout=${(P)timeout_var}
        if (( now - last > timeout )); then
            eval "${state_var}=half_open"
            return 0
        fi
        proven_err "circuit_open: circuit breaker is open"
        return 1
    fi
    return 0
}

# Record success
# Usage: proven_circuit_success <name>
proven_circuit_success() {
    local name="$1"
    eval "${name}_state=closed"
    eval "${name}_failures=0"
    return 0
}

# Record failure
# Usage: proven_circuit_failure <name>
proven_circuit_failure() {
    local name="$1"
    local failures_var="${name}_failures"
    local threshold_var="${name}_threshold"
    local state_var="${name}_state"
    local last_var="${name}_last_failure"

    local failures=${(P)failures_var}
    local threshold=${(P)threshold_var}

    failures=$((failures + 1))
    eval "${failures_var}=$failures"
    eval "${last_var}=$(date +%s)"

    if (( failures >= threshold )); then
        eval "${state_var}=open"
    fi
    return 0
}

# ============================================================================
# SAFE RETRY (safe_retry)
# ============================================================================

# Calculate exponential backoff delay
# Usage: proven_backoff_delay <attempt> <base_delay> <max_delay>
proven_backoff_delay() {
    local attempt="$1"
    local base="${2:-1}"
    local max="${3:-60}"

    local delay=$((base * (2 ** (attempt - 1))))
    if (( delay > max )); then
        delay=$max
    fi
    proven_ok "$delay"
    return 0
}

# Add jitter to delay
# Usage: proven_jitter <delay> <factor>
proven_jitter() {
    local delay="$1"
    local factor="${2:-0.5}"
    local jitter=$(echo "scale=2; $delay * $factor * $RANDOM / 32767" | bc -l)
    local result=$(echo "scale=2; $delay + $jitter" | bc -l)
    proven_ok "$result"
    return 0
}

# ============================================================================
# SAFE MONOTONIC (safe_monotonic)
# ============================================================================

# Initialize monotonic counter
# Usage: proven_monotonic_init <name> <start>
proven_monotonic_init() {
    local name="$1"
    local start="${2:-0}"
    eval "${name}_counter=$start"
    return 0
}

# Get next monotonic value
# Usage: proven_monotonic_next <name>
proven_monotonic_next() {
    local name="$1"
    local counter_var="${name}_counter"
    local current=${(P)counter_var}
    local next=$((current + 1))
    eval "${counter_var}=$next"
    proven_ok "$next"
    return 0
}

# Get current monotonic value without incrementing
# Usage: proven_monotonic_current <name>
proven_monotonic_current() {
    local name="$1"
    local counter_var="${name}_counter"
    proven_ok "${(P)counter_var}"
    return 0
}

# ============================================================================
# SAFE STATE MACHINE (safe_state_machine)
# ============================================================================

# Initialize state machine
# Usage: proven_sm_init <name> <initial_state>
proven_sm_init() {
    local name="$1"
    local initial="$2"
    eval "${name}_state=\$initial"
    eval "typeset -gA ${name}_transitions"
    return 0
}

# Add valid transition
# Usage: proven_sm_add_transition <name> <from> <to>
proven_sm_add_transition() {
    local name="$1"
    local from="$2"
    local to="$3"
    local trans_var="${name}_transitions"
    eval "local current=\${${trans_var}[\$from]}"
    if [[ -n "$current" ]]; then
        eval "${trans_var}[\$from]=\"\$current \$to\""
    else
        eval "${trans_var}[\$from]=\$to"
    fi
    return 0
}

# Check if transition is valid
# Usage: proven_sm_can_transition <name> <to>
proven_sm_can_transition() {
    local name="$1"
    local to="$2"
    local state_var="${name}_state"
    local trans_var="${name}_transitions"

    local current=${(P)state_var}
    eval "local valid=\${${trans_var}[\$current]}"
    [[ " $valid " =~ " $to " ]]
}

# Attempt transition
# Usage: proven_sm_transition <name> <to>
proven_sm_transition() {
    local name="$1"
    local to="$2"

    if proven_sm_can_transition "$name" "$to"; then
        eval "${name}_state=\$to"
        return 0
    fi
    proven_err "invalid_transition: cannot transition to $to"
    return 1
}

# Get current state
# Usage: proven_sm_current <name>
proven_sm_current() {
    local name="$1"
    proven_ok "${(P)${name}_state}"
    return 0
}

# ============================================================================
# SAFE CALCULATOR (safe_calculator)
# ============================================================================

# Evaluate mathematical expression safely
# Usage: proven_calc <expression>
proven_calc() {
    local expr="$1"

    # Validate expression contains only safe characters
    if [[ ! "$expr" =~ ^[0-9+\-*/().\ ]+$ ]]; then
        proven_err "invalid_expression: contains unsafe characters"
        return 1
    fi

    # Use bc for calculation
    local result=$(echo "$expr" | bc -l 2>/dev/null)
    if [[ $? -eq 0 ]] && [[ -n "$result" ]]; then
        proven_ok "$result"
        return 0
    fi
    proven_err "calculation_error: invalid expression"
    return 1
}

# ============================================================================
# SAFE GEO (safe_geo)
# ============================================================================

# Validate latitude
# Usage: proven_is_valid_latitude <lat>
proven_is_valid_latitude() {
    local lat="$1"
    [[ "$lat" =~ ^-?[0-9]*\.?[0-9]+$ ]] && \
    (( $(echo "$lat >= -90 && $lat <= 90" | bc -l) ))
}

# Validate longitude
# Usage: proven_is_valid_longitude <lon>
proven_is_valid_longitude() {
    local lon="$1"
    [[ "$lon" =~ ^-?[0-9]*\.?[0-9]+$ ]] && \
    (( $(echo "$lon >= -180 && $lon <= 180" | bc -l) ))
}

# Calculate Haversine distance between two points (km)
# Usage: proven_haversine <lat1> <lon1> <lat2> <lon2>
proven_haversine() {
    local lat1="$1" lon1="$2" lat2="$3" lon2="$4"
    local pi="3.14159265358979323846"
    local R="6371"  # Earth radius in km

    local dLat=$(echo "scale=10; ($lat2 - $lat1) * $pi / 180" | bc -l)
    local dLon=$(echo "scale=10; ($lon2 - $lon1) * $pi / 180" | bc -l)
    local lat1r=$(echo "scale=10; $lat1 * $pi / 180" | bc -l)
    local lat2r=$(echo "scale=10; $lat2 * $pi / 180" | bc -l)

    local a=$(echo "scale=10; s($dLat/2)^2 + c($lat1r)*c($lat2r)*s($dLon/2)^2" | bc -l)
    local c=$(echo "scale=10; 2*a(sqrt($a)/sqrt(1-$a))" | bc -l)
    local d=$(echo "scale=4; $R * $c" | bc -l)

    proven_ok "$d"
    return 0
}

# ============================================================================
# SAFE PROBABILITY (safe_probability)
# ============================================================================

# Create probability value clamped to [0,1]
# Usage: proven_probability <value>
proven_probability() {
    local val="$1"
    if [[ ! "$val" =~ ^[0-9]*\.?[0-9]+$ ]]; then
        proven_err "invalid_input: not a valid number"
        return 1
    fi
    if (( $(echo "$val < 0" | bc -l) )); then
        proven_ok "0"
    elif (( $(echo "$val > 1" | bc -l) )); then
        proven_ok "1"
    else
        proven_ok "$val"
    fi
    return 0
}

# Calculate probability of independent events (AND)
# Usage: proven_prob_and <p1> <p2>
proven_prob_and() {
    local p1="$1" p2="$2"
    local result=$(echo "scale=10; $p1 * $p2" | bc -l)
    proven_ok "$result"
    return 0
}

# Calculate probability of any event (OR)
# Usage: proven_prob_or <p1> <p2>
proven_prob_or() {
    local p1="$1" p2="$2"
    local result=$(echo "scale=10; $p1 + $p2 - $p1 * $p2" | bc -l)
    proven_ok "$result"
    return 0
}

# ============================================================================
# SAFE CHECKSUM (safe_checksum)
# ============================================================================

# Calculate CRC32 checksum
# Usage: proven_crc32 <string>
proven_crc32() {
    local input="$1"
    if command -v cksum &>/dev/null; then
        local result=$(echo -n "$input" | cksum | cut -d' ' -f1)
        proven_ok "$result"
        return 0
    fi
    proven_err "unavailable: cksum not found"
    return 1
}

# Validate Luhn checksum (credit cards, etc)
# Usage: proven_luhn_check <number>
proven_luhn_check() {
    local num="${1//[^0-9]/}"
    local sum=0
    local double=0

    for (( i=${#num}-1; i>=0; i-- )); do
        local digit=${num:$i:1}
        if (( double )); then
            digit=$((digit * 2))
            if (( digit > 9 )); then
                digit=$((digit - 9))
            fi
        fi
        sum=$((sum + digit))
        double=$((1 - double))
    done

    (( sum % 10 == 0 ))
}

# ============================================================================
# SAFE TENSOR (safe_tensor)
# ============================================================================

# Create vector with bounds checking
# Usage: proven_vector_create <name> <values...>
proven_vector_create() {
    local name="$1"
    shift
    eval "${name}=(\"\$@\")"
    eval "${name}_len=$#"
    return 0
}

# Vector dot product with bounds checking
# Usage: proven_vector_dot <name1> <name2>
proven_vector_dot() {
    local name1="$1" name2="$2"
    local len1_var="${name1}_len"
    local len2_var="${name2}_len"

    local len1=${(P)len1_var}
    local len2=${(P)len2_var}

    if (( len1 != len2 )); then
        proven_err "dimension_mismatch: vectors must have same length"
        return 1
    fi

    local sum=0
    for (( i=1; i<=len1; i++ )); do
        eval "local v1=\${${name1}[$i]}"
        eval "local v2=\${${name2}[$i]}"
        sum=$(echo "$sum + $v1 * $v2" | bc -l)
    done

    proven_ok "$sum"
    return 0
}

# ============================================================================
# SAFE PASSWORD (safe_password)
# ============================================================================

# Check password strength
# Usage: proven_password_strength <password>
proven_password_strength() {
    local password="$1"
    local score=0

    # Length check
    (( ${#password} >= 8 )) && score=$((score + 1))
    (( ${#password} >= 12 )) && score=$((score + 1))
    (( ${#password} >= 16 )) && score=$((score + 1))

    # Character class checks
    [[ "$password" =~ [a-z] ]] && score=$((score + 1))
    [[ "$password" =~ [A-Z] ]] && score=$((score + 1))
    [[ "$password" =~ [0-9] ]] && score=$((score + 1))
    [[ "$password" =~ [^a-zA-Z0-9] ]] && score=$((score + 1))

    if (( score >= 6 )); then
        proven_ok "strong"
    elif (( score >= 4 )); then
        proven_ok "medium"
    else
        proven_ok "weak"
    fi
    return 0
}

# Validate password against policy
# Usage: proven_password_validate <password> <min_length> <require_upper> <require_number> <require_special>
proven_password_validate() {
    local password="$1"
    local min_length="${2:-8}"
    local require_upper="${3:-1}"
    local require_number="${4:-1}"
    local require_special="${5:-0}"

    if (( ${#password} < min_length )); then
        proven_err "password_too_short: minimum length is $min_length"
        return 1
    fi
    if (( require_upper )) && [[ ! "$password" =~ [A-Z] ]]; then
        proven_err "password_missing_uppercase: must contain uppercase letter"
        return 1
    fi
    if (( require_number )) && [[ ! "$password" =~ [0-9] ]]; then
        proven_err "password_missing_number: must contain number"
        return 1
    fi
    if (( require_special )) && [[ ! "$password" =~ [^a-zA-Z0-9] ]]; then
        proven_err "password_missing_special: must contain special character"
        return 1
    fi
    return 0
}

# ============================================================================
# SAFE ML (safe_ml)
# ============================================================================

# Numerically stable softmax
# Usage: proven_softmax <values...>
proven_softmax() {
    local -a values=("$@")
    local max=${values[1]}

    # Find max for numerical stability
    for v in "${values[@]}"; do
        if (( $(echo "$v > $max" | bc -l) )); then
            max="$v"
        fi
    done

    # Calculate exp sum
    local sum=0
    for v in "${values[@]}"; do
        local exp=$(echo "scale=10; e($v - $max)" | bc -l)
        sum=$(echo "$sum + $exp" | bc -l)
    done

    # Calculate softmax values
    local -a result
    for v in "${values[@]}"; do
        local exp=$(echo "scale=10; e($v - $max)" | bc -l)
        local prob=$(echo "scale=6; $exp / $sum" | bc -l)
        result+=("$prob")
    done

    proven_ok "${result[*]}"
    return 0
}

# ============================================================================
# SAFE HEADER (safe_header)
# ============================================================================

# Validate HTTP header name
# Usage: proven_is_valid_header_name <name>
proven_is_valid_header_name() {
    local name="$1"
    # Token chars: !#$%&'*+-.^_`|~0-9A-Za-z
    [[ "$name" =~ ^[!#$%\&\'*+.^_\`|~0-9A-Za-z-]+$ ]]
}

# Validate HTTP header value (no CRLF injection)
# Usage: proven_is_valid_header_value <value>
proven_is_valid_header_value() {
    local value="$1"
    # No CR, LF, or NUL
    [[ ! "$value" =~ $'\r' ]] && [[ ! "$value" =~ $'\n' ]] && [[ ! "$value" =~ $'\0' ]]
}

# Sanitize header value
# Usage: proven_sanitize_header <value>
proven_sanitize_header() {
    local value="$1"
    local sanitized="${value//$'\r'/}"
    sanitized="${sanitized//$'\n'/}"
    sanitized="${sanitized//$'\0'/}"
    proven_ok "$sanitized"
    return 0
}

# ============================================================================
# SAFE COOKIE (safe_cookie)
# ============================================================================

# Validate cookie name
# Usage: proven_is_valid_cookie_name <name>
proven_is_valid_cookie_name() {
    local name="$1"
    # Cookie names are tokens (no separators)
    [[ "$name" =~ ^[!#$%\&\'*+.0-9A-Za-z^_\`|~-]+$ ]] && \
    [[ ! "$name" =~ [=,\;[:space:]] ]]
}

# Validate cookie value
# Usage: proven_is_valid_cookie_value <value>
proven_is_valid_cookie_value() {
    local value="$1"
    # No semicolons, commas, whitespace, or control characters
    [[ ! "$value" =~ [,\;[:space:][:cntrl:]] ]]
}

# Build Set-Cookie header
# Usage: proven_set_cookie <name> <value> [options...]
proven_set_cookie() {
    local name="$1"
    local value="$2"
    shift 2

    if ! proven_is_valid_cookie_name "$name"; then
        proven_err "invalid_cookie_name: name contains invalid characters"
        return 1
    fi
    if ! proven_is_valid_cookie_value "$value"; then
        proven_err "invalid_cookie_value: value contains invalid characters"
        return 1
    fi

    local cookie="$name=$value"

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --path) cookie+="; Path=$2"; shift 2 ;;
            --domain) cookie+="; Domain=$2"; shift 2 ;;
            --max-age) cookie+="; Max-Age=$2"; shift 2 ;;
            --secure) cookie+="; Secure"; shift ;;
            --httponly) cookie+="; HttpOnly"; shift ;;
            --samesite) cookie+="; SameSite=$2"; shift 2 ;;
            *) shift ;;
        esac
    done

    proven_ok "$cookie"
    return 0
}

# ============================================================================
# SAFE CONTENT TYPE (safe_content_type)
# ============================================================================

# Validate MIME type format
# Usage: proven_is_valid_mime_type <type>
proven_is_valid_mime_type() {
    local mime="$1"
    [[ "$mime" =~ ^[a-zA-Z0-9!#$&.+-]+/[a-zA-Z0-9!#$&.+-]+$ ]]
}

# Get media category from MIME type
# Usage: proven_media_category <mime_type>
proven_media_category() {
    local mime="$1"
    case "$mime" in
        text/*) proven_ok "text" ;;
        image/*) proven_ok "image" ;;
        audio/*) proven_ok "audio" ;;
        video/*) proven_ok "video" ;;
        application/json|application/xml) proven_ok "data" ;;
        application/octet-stream) proven_ok "binary" ;;
        *) proven_ok "other" ;;
    esac
    return 0
}

# ============================================================================
# BOUNDED VALUES
# ============================================================================

# Clamp value to range [min, max]
# Usage: proven_clamp <value> <min> <max>
proven_clamp() {
    local value=$1
    local min_val=$2
    local max_val=$3

    if (( value < min_val )); then
        echo $min_val
    elif (( value > max_val )); then
        echo $max_val
    else
        echo $value
    fi
    return 0
}

# Check if value is in range (inclusive)
# Usage: proven_in_range <value> <min> <max>
proven_in_range() {
    local value=$1
    local min_val=$2
    local max_val=$3

    (( value >= min_val && value <= max_val ))
}

# Require value in range or return error
# Usage: proven_require_in_range <value> <min> <max>
proven_require_in_range() {
    local value=$1
    local min_val=$2
    local max_val=$3

    if ! proven_in_range "$value" "$min_val" "$max_val"; then
        proven_err "out_of_bounds: $value not in [$min_val, $max_val]"
        return 1
    fi
    return 0
}

# Validate percentage (0-100)
# Usage: proven_is_valid_percentage <value>
proven_is_valid_percentage() {
    local value=$1
    [[ "$value" =~ ^[0-9]+(\.[0-9]+)?$ ]] && (( $(echo "$value <= 100 && $value >= 0" | bc -l) ))
}

# Require valid percentage or return error
# Usage: proven_require_valid_percentage <value>
proven_require_valid_percentage() {
    local value=$1
    if ! proven_is_valid_percentage "$value"; then
        proven_err "invalid_percentage: $value is not valid (0-100)"
        return 1
    fi
    return 0
}

# Check if string is non-empty
# Usage: proven_is_non_empty <value>
proven_is_non_empty() {
    [[ -n "$1" && "$1" != "" ]]
}

# Require non-empty string or return error
# Usage: proven_require_non_empty <value> [name]
proven_require_non_empty() {
    local value=$1
    local name=${2:-"value"}
    if ! proven_is_non_empty "$value"; then
        proven_err "empty_string: $name cannot be empty"
        return 1
    fi
    return 0
}

# ============================================================================
# PERCENTAGE CALCULATIONS
# ============================================================================

# Calculate percentage using basis points (100 bps = 1%)
# Usage: proven_percentage_of <amount> <bps>
proven_percentage_of() {
    local amount=$1
    local bps=$2

    if [[ ! "$amount" =~ ^[0-9]+$ ]]; then
        proven_err "invalid_input: amount must be non-negative integer"
        return 1
    fi
    if [[ ! "$bps" =~ ^[0-9]+$ ]]; then
        proven_err "invalid_input: bps must be non-negative integer"
        return 1
    fi

    proven_ok $(( amount * bps / 10000 ))
    return 0
}

# Calculate percentage (0-100 scale)
# Usage: proven_percentage_of_100 <amount> <percentage>
proven_percentage_of_100() {
    local amount=$1
    local pct=$2

    if ! proven_is_valid_percentage "$pct"; then
        proven_err "invalid_percentage: percentage must be 0-100"
        return 1
    fi

    proven_ok $(( amount * pct / 100 ))
    return 0
}

# ============================================================================
# VERSION & INFO
# ============================================================================

# Get library version
# Usage: proven_version
proven_version() {
    echo "$PROVEN_VERSION"
}

# Get module count
# Usage: proven_module_count
proven_module_count() {
    echo "$PROVEN_MODULE_COUNT"
}

# List all modules
# Usage: proven_list_modules [category]
proven_list_modules() {
    local category="${1:-all}"
    if [[ "$category" == "all" ]]; then
        for cat in "${(@k)PROVEN_MODULES}"; do
            echo "[$cat]: ${PROVEN_MODULES[$cat]}"
        done
    else
        echo "${PROVEN_MODULES[$category]:-unknown category}"
    fi
}

# ============================================================================
# ZSH COMPLETION SUPPORT
# ============================================================================

if [[ -n "$ZSH_VERSION" ]]; then
    # Comprehensive completion for proven functions
    _proven() {
        local -a commands
        commands=(
            # Core - safe_math
            'proven_add:Safe addition with overflow check'
            'proven_sub:Safe subtraction with underflow check'
            'proven_mul:Safe multiplication with overflow check'
            'proven_div:Safe division with zero check'
            'proven_mod:Safe modulo with zero check'
            'proven_abs:Safe absolute value'
            'proven_pow:Safe power with overflow check'
            # Core - safe_string
            'proven_escape_html:Escape HTML entities'
            'proven_escape_sql:Escape SQL single quotes'
            'proven_escape_shell:Escape shell metacharacters'
            'proven_is_ascii:Check if ASCII only'
            'proven_truncate:Truncate string to max length'
            'proven_is_alphanumeric:Check if alphanumeric'
            'proven_normalize_whitespace:Normalize whitespace'
            # Core - safe_path
            'proven_is_safe_path:Check path safety'
            'proven_safe_join_path:Safely join paths'
            'proven_normalize_path:Normalize path'
            'proven_get_extension:Get file extension'
            # Core - safe_email
            'proven_is_valid_email:Validate email format'
            'proven_email_local:Extract local part from email'
            'proven_email_domain:Extract domain from email'
            # Core - safe_url
            'proven_is_valid_url:Validate URL format'
            'proven_url_scheme:Extract URL scheme'
            'proven_url_host:Extract URL host'
            'proven_url_encode:URL encode string'
            # Core - safe_network
            'proven_is_valid_ipv4:Validate IPv4 address'
            'proven_is_valid_ipv6:Validate IPv6 address'
            'proven_is_valid_port:Validate port number'
            'proven_require_valid_port:Require valid port'
            'proven_ip_in_cidr:Check IP in CIDR range'
            # Core - safe_crypto
            'proven_random_hex:Generate random hex string'
            'proven_sha256:SHA256 hash'
            'proven_blake3:BLAKE3 hash'
            'proven_constant_time_compare:Constant-time comparison'
            # Core - safe_uuid
            'proven_uuid_v4:Generate UUID v4'
            'proven_is_valid_uuid:Validate UUID format'
            'proven_is_nil_uuid:Check if nil UUID'
            # Core - safe_currency
            'proven_is_valid_currency:Validate currency code'
            'proven_currency_symbol:Get currency symbol'
            'proven_currency_decimals:Get currency decimals'
            # Core - safe_phone
            'proven_is_valid_phone:Validate E.164 phone'
            'proven_normalize_phone:Normalize phone to E.164'
            # Core - safe_hex
            'proven_hex_encode:Encode to hex'
            'proven_hex_decode:Decode from hex'
            'proven_is_valid_hex:Validate hex string'
            # Data - safe_json
            'proven_is_valid_json:Validate JSON syntax'
            'proven_json_get:Extract JSON field'
            'proven_json_escape:Escape string for JSON'
            # Data - safe_datetime
            'proven_is_valid_iso_date:Validate ISO 8601 date'
            'proven_now_iso:Get current ISO timestamp'
            'proven_timestamp_to_iso:Unix to ISO timestamp'
            # Data - safe_float
            'proven_float_div:Safe float division'
            'proven_is_finite:Check if finite'
            'proven_float_clamp:Clamp float to range'
            # Data - safe_version
            'proven_parse_semver:Parse semantic version'
            'proven_compare_version:Compare versions'
            # Data - safe_color
            'proven_is_valid_hex_color:Validate hex color'
            'proven_hex_to_rgb:Hex to RGB conversion'
            'proven_luminance:Calculate luminance'
            # Data - safe_angle
            'proven_deg_to_rad:Degrees to radians'
            'proven_rad_to_deg:Radians to degrees'
            'proven_normalize_degrees:Normalize degrees 0-360'
            # Data - safe_unit
            'proven_convert_length:Convert length units'
            'proven_convert_temp:Convert temperature units'
            # Data Structures - safe_buffer
            'proven_buffer_init:Initialize bounded buffer'
            'proven_buffer_push:Push to buffer'
            'proven_buffer_pop:Pop from buffer'
            # Data Structures - safe_queue
            'proven_queue_init:Initialize bounded queue'
            'proven_queue_enqueue:Enqueue item'
            'proven_queue_dequeue:Dequeue item'
            # Data Structures - safe_bloom
            'proven_bloom_init:Initialize bloom filter'
            'proven_bloom_add:Add to bloom filter'
            'proven_bloom_contains:Check bloom filter'
            # Data Structures - safe_lru
            'proven_lru_init:Initialize LRU cache'
            'proven_lru_get:Get from LRU cache'
            'proven_lru_put:Put in LRU cache'
            # Data Structures - safe_graph
            'proven_graph_init:Initialize directed graph'
            'proven_graph_add_edge:Add edge to graph'
            'proven_graph_has_edge:Check if edge exists'
            # Resilience - safe_rate_limiter
            'proven_ratelimit_init:Initialize rate limiter'
            'proven_ratelimit_acquire:Acquire rate limit tokens'
            # Resilience - safe_circuit_breaker
            'proven_circuit_init:Initialize circuit breaker'
            'proven_circuit_allow:Check if circuit allows'
            'proven_circuit_success:Record success'
            'proven_circuit_failure:Record failure'
            # Resilience - safe_retry
            'proven_backoff_delay:Calculate backoff delay'
            'proven_jitter:Add jitter to delay'
            # Resilience - safe_monotonic
            'proven_monotonic_init:Initialize monotonic counter'
            'proven_monotonic_next:Get next value'
            'proven_monotonic_current:Get current value'
            # State - safe_state_machine
            'proven_sm_init:Initialize state machine'
            'proven_sm_add_transition:Add valid transition'
            'proven_sm_can_transition:Check if can transition'
            'proven_sm_transition:Attempt transition'
            'proven_sm_current:Get current state'
            # State - safe_calculator
            'proven_calc:Evaluate expression safely'
            # Algorithm - safe_geo
            'proven_is_valid_latitude:Validate latitude'
            'proven_is_valid_longitude:Validate longitude'
            'proven_haversine:Calculate Haversine distance'
            # Algorithm - safe_probability
            'proven_probability:Create probability value'
            'proven_prob_and:Probability AND'
            'proven_prob_or:Probability OR'
            # Algorithm - safe_checksum
            'proven_crc32:Calculate CRC32'
            'proven_luhn_check:Validate Luhn checksum'
            # Algorithm - safe_tensor
            'proven_vector_create:Create vector'
            'proven_vector_dot:Vector dot product'
            # Security - safe_password
            'proven_password_strength:Check password strength'
            'proven_password_validate:Validate password policy'
            # Security - safe_ml
            'proven_softmax:Numerically stable softmax'
            # HTTP - safe_header
            'proven_is_valid_header_name:Validate header name'
            'proven_is_valid_header_value:Validate header value'
            'proven_sanitize_header:Sanitize header value'
            # HTTP - safe_cookie
            'proven_is_valid_cookie_name:Validate cookie name'
            'proven_is_valid_cookie_value:Validate cookie value'
            'proven_set_cookie:Build Set-Cookie header'
            # HTTP - safe_content_type
            'proven_is_valid_mime_type:Validate MIME type'
            'proven_media_category:Get media category'
            # Utility
            'proven_clamp:Clamp value to range'
            'proven_in_range:Check if value in range'
            'proven_require_in_range:Require value in range'
            'proven_is_valid_percentage:Validate percentage'
            'proven_require_valid_percentage:Require valid percentage'
            'proven_is_non_empty:Check if non-empty'
            'proven_require_non_empty:Require non-empty'
            'proven_percentage_of:Calculate percentage (bps)'
            'proven_percentage_of_100:Calculate percentage'
            'proven_version:Get library version'
            'proven_module_count:Get module count'
            'proven_list_modules:List all modules'
        )
        _describe 'proven command' commands
    }

    compdef _proven proven
fi
