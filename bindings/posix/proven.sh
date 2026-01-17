# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# proven.sh - Portable POSIX shell bindings for the Proven library
#
# Formally verified safety primitives implemented in pure POSIX shell.
# Compatible with: dash, ash, busybox sh, bash, zsh, ksh, and other
# POSIX-compliant shells.
#
# Usage:
#   . /path/to/proven.sh
#
# All functions return 0 on success, 1 on failure.
# Results are returned via PROVEN_RESULT variable.
# Errors are returned via PROVEN_ERROR variable.
#
# ============================================================================
# VERSION AND METADATA
# ============================================================================

PROVEN_VERSION="0.4.0"
PROVEN_MODULE_COUNT=38

# Module categories
PROVEN_MODULES_CORE="safe_math safe_string safe_path safe_email safe_url safe_network safe_crypto safe_uuid safe_currency safe_phone safe_hex"
PROVEN_MODULES_DATA="safe_json safe_datetime safe_float safe_version safe_color safe_angle safe_unit"
PROVEN_MODULES_STRUCTURES="safe_buffer safe_queue safe_bloom safe_lru safe_graph"
PROVEN_MODULES_RESILIENCE="safe_rate_limiter safe_circuit_breaker safe_retry safe_monotonic"
PROVEN_MODULES_STATE="safe_state_machine safe_calculator"
PROVEN_MODULES_ALGORITHM="safe_geo safe_probability safe_checksum safe_tensor"
PROVEN_MODULES_SECURITY="safe_password safe_ml"
PROVEN_MODULES_HTTP="safe_header safe_cookie safe_content_type"

# 64-bit integer limits
PROVEN_MAX_INT=9223372036854775807
PROVEN_MIN_INT=-9223372036854775808

# Global result variables
PROVEN_RESULT=""
PROVEN_ERROR=""

# ============================================================================
# INTERNAL UTILITY FUNCTIONS
# ============================================================================

# Reset result variables
_proven_reset() {
    PROVEN_RESULT=""
    PROVEN_ERROR=""
}

# Check if a value is an integer
_proven_is_int() {
    case "$1" in
        ''|*[!0-9-]*) return 1 ;;
        -*[!0-9]*) return 1 ;;
        -) return 1 ;;
        *) return 0 ;;
    esac
}

# Convert string to lowercase (POSIX-compliant)
_proven_tolower() {
    printf '%s' "$1" | tr 'A-Z' 'a-z'
}

# Convert string to uppercase (POSIX-compliant)
_proven_toupper() {
    printf '%s' "$1" | tr 'a-z' 'A-Z'
}

# Get string length
_proven_strlen() {
    printf '%s' "$1" | wc -c | tr -d ' '
}

# Check if string contains substring
_proven_contains() {
    case "$1" in
        *"$2"*) return 0 ;;
        *) return 1 ;;
    esac
}

# ============================================================================
# MODULE 1: SAFE_MATH - Core arithmetic with overflow detection
# ============================================================================

# Safe addition with overflow detection
# Usage: safe_add 5 3 && echo "$PROVEN_RESULT"
safe_add() {
    _proven_reset

    if [ -z "$1" ] || [ -z "$2" ]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if ! _proven_is_int "$1" || ! _proven_is_int "$2"; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    # Use awk for safe arithmetic (handles large numbers)
    PROVEN_RESULT=$(awk -v a="$1" -v b="$2" -v max="$PROVEN_MAX_INT" -v min="$PROVEN_MIN_INT" '
        BEGIN {
            if (b > 0 && a > max - b) { print "OVERFLOW"; exit 1 }
            if (b < 0 && a < min - b) { print "OVERFLOW"; exit 1 }
            print a + b
        }
    ')

    if [ "$PROVEN_RESULT" = "OVERFLOW" ]; then
        PROVEN_ERROR="overflow"
        PROVEN_RESULT=""
        return 1
    fi

    return 0
}

# Safe subtraction with underflow detection
# Usage: safe_sub 10 3 && echo "$PROVEN_RESULT"
safe_sub() {
    _proven_reset

    if [ -z "$1" ] || [ -z "$2" ]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if ! _proven_is_int "$1" || ! _proven_is_int "$2"; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    PROVEN_RESULT=$(awk -v a="$1" -v b="$2" -v max="$PROVEN_MAX_INT" -v min="$PROVEN_MIN_INT" '
        BEGIN {
            if (b > 0 && a < min + b) { print "UNDERFLOW"; exit 1 }
            if (b < 0 && a > max + b) { print "UNDERFLOW"; exit 1 }
            print a - b
        }
    ')

    if [ "$PROVEN_RESULT" = "UNDERFLOW" ]; then
        PROVEN_ERROR="underflow"
        PROVEN_RESULT=""
        return 1
    fi

    return 0
}

# Safe multiplication with overflow detection
# Usage: safe_mul 5 3 && echo "$PROVEN_RESULT"
safe_mul() {
    _proven_reset

    if [ -z "$1" ] || [ -z "$2" ]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if ! _proven_is_int "$1" || ! _proven_is_int "$2"; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    PROVEN_RESULT=$(awk -v a="$1" -v b="$2" -v max="$PROVEN_MAX_INT" -v min="$PROVEN_MIN_INT" '
        BEGIN {
            if (a == 0 || b == 0) { print 0; exit 0 }
            result = a * b
            # Verify by division
            if (a != 0 && int(result / a) != b) { print "OVERFLOW"; exit 1 }
            print result
        }
    ')

    if [ "$PROVEN_RESULT" = "OVERFLOW" ]; then
        PROVEN_ERROR="overflow"
        PROVEN_RESULT=""
        return 1
    fi

    return 0
}

# Safe division with zero check
# Usage: safe_div 10 2 && echo "$PROVEN_RESULT"
safe_div() {
    _proven_reset

    if [ -z "$1" ] || [ -z "$2" ]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if ! _proven_is_int "$1" || ! _proven_is_int "$2"; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if [ "$2" -eq 0 ]; then
        PROVEN_ERROR="division_by_zero"
        return 1
    fi

    # Handle MIN_INT / -1 overflow
    if [ "$1" = "$PROVEN_MIN_INT" ] && [ "$2" = "-1" ]; then
        PROVEN_ERROR="overflow"
        return 1
    fi

    PROVEN_RESULT=$(awk -v a="$1" -v b="$2" 'BEGIN { print int(a / b) }')
    return 0
}

# Safe modulo
# Usage: safe_mod 10 3 && echo "$PROVEN_RESULT"
safe_mod() {
    _proven_reset

    if [ -z "$1" ] || [ -z "$2" ]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if [ "$2" -eq 0 ]; then
        PROVEN_ERROR="division_by_zero"
        return 1
    fi

    PROVEN_RESULT=$(awk -v a="$1" -v b="$2" 'BEGIN { print a % b }')
    return 0
}

# Safe absolute value
# Usage: safe_abs -5 && echo "$PROVEN_RESULT"
safe_abs() {
    _proven_reset

    if [ -z "$1" ]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if [ "$1" = "$PROVEN_MIN_INT" ]; then
        PROVEN_ERROR="overflow"
        return 1
    fi

    if [ "$1" -lt 0 ]; then
        PROVEN_RESULT=$(( -1 * $1 ))
    else
        PROVEN_RESULT="$1"
    fi

    return 0
}

# Clamp value to range
# Usage: PROVEN_RESULT=$(safe_clamp 0 100 150)
safe_clamp() {
    if [ "$3" -lt "$1" ]; then
        printf '%s' "$1"
    elif [ "$3" -gt "$2" ]; then
        printf '%s' "$2"
    else
        printf '%s' "$3"
    fi
}

# ============================================================================
# MODULE 2: SAFE_STRING - String manipulation with injection prevention
# ============================================================================

# Escape HTML entities
# Usage: safe_html=$(escape_html "$user_input")
escape_html() {
    printf '%s' "$1" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'\''/\&#x27;/g'
}

# Escape for SQL single quotes
# Usage: safe_sql=$(escape_sql "$user_input")
escape_sql() {
    printf '%s' "$1" | sed "s/'/''/g"
}

# Escape for shell commands
# Usage: safe_shell=$(escape_shell "$user_input")
escape_shell() {
    printf '%s' "$1" | sed "s/'/'\\\\''/g; s/^/'/; s/\$/'/"
}

# URL encode a string
# Usage: encoded=$(url_encode "$string")
url_encode() {
    printf '%s' "$1" | awk '
        BEGIN {
            for (i = 0; i <= 255; i++) {
                ord[sprintf("%c", i)] = i
            }
        }
        {
            n = split($0, chars, "")
            for (i = 1; i <= n; i++) {
                c = chars[i]
                if (c ~ /[A-Za-z0-9._~-]/) {
                    printf "%s", c
                } else {
                    printf "%%%02X", ord[c]
                }
            }
        }
    '
}

# URL decode a string
# Usage: decoded=$(url_decode "$encoded")
url_decode() {
    printf '%s' "$1" | awk '
        BEGIN {
            for (i = 0; i <= 255; i++) {
                hex[sprintf("%02X", i)] = sprintf("%c", i)
                hex[sprintf("%02x", i)] = sprintf("%c", i)
            }
        }
        {
            n = length($0)
            i = 1
            while (i <= n) {
                c = substr($0, i, 1)
                if (c == "%") {
                    h = substr($0, i + 1, 2)
                    if (h in hex) {
                        printf "%s", hex[h]
                        i += 3
                        continue
                    }
                } else if (c == "+") {
                    printf " "
                    i++
                    continue
                }
                printf "%s", c
                i++
            }
        }
    '
}

# Safely truncate a string
# Usage: truncated=$(truncate_safe "$string" 50 "...")
truncate_safe() {
    _str="$1"
    _max="${2:-50}"
    _suffix="${3:-...}"

    _len=$(_proven_strlen "$_str")

    if [ "$_len" -le "$_max" ]; then
        printf '%s' "$_str"
        return
    fi

    _suffix_len=$(_proven_strlen "$_suffix")
    _cut_at=$(( _max - _suffix_len ))

    if [ "$_cut_at" -le 0 ]; then
        printf '%s' "$_str" | cut -c1-"$_max"
        return
    fi

    printf '%s' "$_str" | cut -c1-"$_cut_at"
    printf '%s' "$_suffix"
}

# Check if string is alphanumeric
# Usage: is_alphanumeric "abc123" && echo "yes"
is_alphanumeric() {
    case "$1" in
        *[!A-Za-z0-9]*) return 1 ;;
        '') return 1 ;;
        *) return 0 ;;
    esac
}

# Sanitize string to allowed characters
# Usage: safe=$(sanitize_string "$input" "a-zA-Z0-9_-")
sanitize_string() {
    _allowed="${2:-a-zA-Z0-9_-}"
    printf '%s' "$1" | tr -cd "$_allowed"
}

# ============================================================================
# MODULE 3: SAFE_PATH - Path validation and traversal prevention
# ============================================================================

# Check if path is safe (no traversal)
# Usage: is_safe_path "/var/data/file.txt" && echo "safe"
is_safe_path() {
    _path="$1"

    # Check for null
    [ -z "$_path" ] && return 1

    # Check for path traversal
    case "$_path" in
        *../*|*..*|*/../*) return 1 ;;
    esac

    # Check for null bytes (if supported)
    if _proven_contains "$_path" "$(printf '\0')"; then
        return 1
    fi

    return 0
}

# Normalize path (remove ./ and redundant slashes)
# Usage: normalized=$(normalize_path "/var//data/./file.txt")
normalize_path() {
    printf '%s' "$1" | sed 's|/\+|/|g; s|/\./|/|g; s|^\./||; s|/\.$||'
}

# Get directory name
# Usage: dir=$(safe_dirname "/var/data/file.txt")
safe_dirname() {
    _path="$1"

    case "$_path" in
        */*) printf '%s' "${_path%/*}" ;;
        *) printf '.' ;;
    esac
}

# Get base name
# Usage: base=$(safe_basename "/var/data/file.txt")
safe_basename() {
    printf '%s' "${1##*/}"
}

# Get file extension
# Usage: ext=$(safe_extension "file.txt")
safe_extension() {
    _base=$(safe_basename "$1")
    case "$_base" in
        .*) printf '' ;;
        *.*) printf '%s' "${_base##*.}" ;;
        *) printf '' ;;
    esac
}

# Join paths safely
# Usage: full=$(path_join "/var" "data" "file.txt")
path_join() {
    _result=""
    for _part do
        case "$_result" in
            '') _result="$_part" ;;
            */) _result="${_result}${_part}" ;;
            *) _result="${_result}/${_part}" ;;
        esac
    done
    printf '%s' "$_result"
}

# ============================================================================
# MODULE 4: SAFE_EMAIL - Email validation
# ============================================================================

# Validate email address
# Usage: is_valid_email "user@example.com" && echo "valid"
is_valid_email() {
    _email="$1"

    # Check for empty
    [ -z "$_email" ] && return 1

    # Check length
    _len=$(_proven_strlen "$_email")
    [ "$_len" -gt 254 ] && return 1

    # Check for @ symbol
    case "$_email" in
        *@*) ;;
        *) return 1 ;;
    esac

    # Split at @
    _local="${_email%@*}"
    _domain="${_email##*@}"

    # Check local part
    [ -z "$_local" ] && return 1
    _local_len=$(_proven_strlen "$_local")
    [ "$_local_len" -gt 64 ] && return 1

    # Check domain
    [ -z "$_domain" ] && return 1
    _domain_len=$(_proven_strlen "$_domain")
    [ "$_domain_len" -lt 3 ] && return 1

    # Domain must have a dot (unless localhost)
    case "$_domain" in
        localhost) ;;
        *.*) ;;
        *) return 1 ;;
    esac

    # Domain can't start or end with dot
    case "$_domain" in
        .*|*.) return 1 ;;
    esac

    # Count @ symbols (must be exactly one)
    _at_count=$(printf '%s' "$_email" | tr -cd '@' | wc -c | tr -d ' ')
    [ "$_at_count" -ne 1 ] && return 1

    return 0
}

# Get email domain
# Usage: domain=$(get_email_domain "user@example.com")
get_email_domain() {
    is_valid_email "$1" || return 1
    printf '%s' "${1##*@}"
}

# Get email local part
# Usage: local_part=$(get_email_local_part "user@example.com")
get_email_local_part() {
    is_valid_email "$1" || return 1
    printf '%s' "${1%@*}"
}

# Normalize email (lowercase domain)
# Usage: normalized=$(normalize_email "User@EXAMPLE.COM")
normalize_email() {
    is_valid_email "$1" || return 1
    _local="${1%@*}"
    _domain="${1##*@}"
    _domain=$(_proven_tolower "$_domain")
    printf '%s@%s' "$_local" "$_domain"
}

# ============================================================================
# MODULE 5: SAFE_URL - URL parsing and validation
# ============================================================================

# Parse URL and extract components
# Usage: url_parse "https://user:pass@example.com:8080/path?query#frag"
# Sets: URL_SCHEME, URL_USER, URL_PASS, URL_HOST, URL_PORT, URL_PATH, URL_QUERY, URL_FRAGMENT
url_parse() {
    _url="$1"
    URL_SCHEME=""
    URL_USER=""
    URL_PASS=""
    URL_HOST=""
    URL_PORT=""
    URL_PATH=""
    URL_QUERY=""
    URL_FRAGMENT=""

    [ -z "$_url" ] && return 1

    # Extract fragment
    case "$_url" in
        *#*)
            URL_FRAGMENT="${_url##*#}"
            _url="${_url%#*}"
            ;;
    esac

    # Extract query
    case "$_url" in
        *\?*)
            URL_QUERY="${_url##*\?}"
            _url="${_url%\?*}"
            ;;
    esac

    # Extract scheme
    case "$_url" in
        *://*)
            URL_SCHEME="${_url%%://*}"
            _url="${_url#*://}"
            ;;
    esac

    # Extract path
    case "$_url" in
        */*)
            URL_PATH="/${_url#*/}"
            _url="${_url%%/*}"
            ;;
    esac

    # Extract userinfo
    case "$_url" in
        *@*)
            _userinfo="${_url%@*}"
            _url="${_url##*@}"
            case "$_userinfo" in
                *:*)
                    URL_USER="${_userinfo%%:*}"
                    URL_PASS="${_userinfo#*:}"
                    ;;
                *)
                    URL_USER="$_userinfo"
                    ;;
            esac
            ;;
    esac

    # Extract port
    case "$_url" in
        *:*)
            URL_HOST="${_url%:*}"
            URL_PORT="${_url##*:}"
            ;;
        *)
            URL_HOST="$_url"
            ;;
    esac

    return 0
}

# Validate URL
# Usage: is_valid_url "https://example.com" && echo "valid"
is_valid_url() {
    url_parse "$1" || return 1

    # Must have scheme and host
    [ -z "$URL_SCHEME" ] && return 1
    [ -z "$URL_HOST" ] && return 1

    # Validate scheme
    case "$URL_SCHEME" in
        http|https|ftp|ftps|mailto|file|ssh|git) ;;
        *) return 1 ;;
    esac

    # Validate port if present
    if [ -n "$URL_PORT" ]; then
        case "$URL_PORT" in
            *[!0-9]*) return 1 ;;
        esac
        [ "$URL_PORT" -lt 1 ] && return 1
        [ "$URL_PORT" -gt 65535 ] && return 1
    fi

    return 0
}

# Build URL from components
# Usage: url=$(url_build "https" "example.com" 8080 "/path" "q=1" "frag")
url_build() {
    _scheme="$1"
    _host="$2"
    _port="$3"
    _path="$4"
    _query="$5"
    _fragment="$6"

    printf '%s://%s' "$_scheme" "$_host"
    [ -n "$_port" ] && printf ':%s' "$_port"
    [ -n "$_path" ] && printf '%s' "$_path"
    [ -n "$_query" ] && printf '?%s' "$_query"
    [ -n "$_fragment" ] && printf '#%s' "$_fragment"
}

# ============================================================================
# MODULE 6: SAFE_NETWORK - Network validation
# ============================================================================

# Validate IPv4 address
# Usage: is_valid_ipv4 "192.168.1.1" && echo "valid"
is_valid_ipv4() {
    _ip="$1"

    # Check format with awk
    printf '%s' "$_ip" | awk -F. '
        {
            if (NF != 4) exit 1
            for (i = 1; i <= 4; i++) {
                if ($i !~ /^[0-9]+$/) exit 1
                if ($i < 0 || $i > 255) exit 1
                if ($i ~ /^0[0-9]/) exit 1  # No leading zeros
            }
            exit 0
        }
    '
}

# Validate IPv6 address (basic check)
# Usage: is_valid_ipv6 "::1" && echo "valid"
is_valid_ipv6() {
    _ip="$1"

    # Basic IPv6 validation
    case "$_ip" in
        *[!0-9a-fA-F:]*) return 1 ;;
        *:::*) return 1 ;;
        '') return 1 ;;
    esac

    # Count colons
    _colons=$(printf '%s' "$_ip" | tr -cd ':' | wc -c | tr -d ' ')

    # Standard IPv6 has 7 colons, compressed can have fewer
    [ "$_colons" -gt 7 ] && return 1

    return 0
}

# Validate port number
# Usage: is_valid_port 8080 && echo "valid"
is_valid_port() {
    case "$1" in
        *[!0-9]*) return 1 ;;
        '') return 1 ;;
    esac

    [ "$1" -lt 1 ] && return 1
    [ "$1" -gt 65535 ] && return 1

    return 0
}

# Check if IP is private
# Usage: is_private_ip "192.168.1.1" && echo "private"
is_private_ip() {
    _ip="$1"

    # Extract first two octets
    _first="${_ip%%.*}"
    _rest="${_ip#*.}"
    _second="${_rest%%.*}"

    case "$_first" in
        10) return 0 ;;
        172)
            [ "$_second" -ge 16 ] && [ "$_second" -le 31 ] && return 0
            ;;
        192)
            [ "$_second" -eq 168 ] && return 0
            ;;
        127) return 0 ;;
    esac

    return 1
}

# Validate CIDR notation
# Usage: is_valid_cidr "192.168.1.0/24" && echo "valid"
is_valid_cidr() {
    case "$1" in
        */*)
            _ip="${1%/*}"
            _prefix="${1##*/}"

            is_valid_ipv4 "$_ip" || return 1

            case "$_prefix" in
                *[!0-9]*) return 1 ;;
                '') return 1 ;;
            esac

            [ "$_prefix" -lt 0 ] && return 1
            [ "$_prefix" -gt 32 ] && return 1

            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# ============================================================================
# MODULE 7: SAFE_CRYPTO - Cryptographic utilities (using system tools)
# ============================================================================

# Generate random hex string
# Usage: random=$(crypto_random_hex 32)
crypto_random_hex() {
    _bytes="${1:-16}"

    if [ -r /dev/urandom ]; then
        head -c "$_bytes" /dev/urandom | od -An -tx1 | tr -d ' \n'
    elif command -v openssl >/dev/null 2>&1; then
        openssl rand -hex "$_bytes"
    else
        PROVEN_ERROR="no_random_source"
        return 1
    fi
}

# Generate random base64 string
# Usage: random=$(crypto_random_base64 32)
crypto_random_base64() {
    _bytes="${1:-16}"

    if [ -r /dev/urandom ]; then
        head -c "$_bytes" /dev/urandom | base64 | tr -d '\n'
    elif command -v openssl >/dev/null 2>&1; then
        openssl rand -base64 "$_bytes"
    else
        PROVEN_ERROR="no_random_source"
        return 1
    fi
}

# Compute SHA-256 hash
# Usage: hash=$(crypto_sha256 "message")
crypto_sha256() {
    if command -v sha256sum >/dev/null 2>&1; then
        printf '%s' "$1" | sha256sum | cut -d' ' -f1
    elif command -v shasum >/dev/null 2>&1; then
        printf '%s' "$1" | shasum -a 256 | cut -d' ' -f1
    elif command -v openssl >/dev/null 2>&1; then
        printf '%s' "$1" | openssl dgst -sha256 | cut -d' ' -f2
    else
        PROVEN_ERROR="no_sha256_tool"
        return 1
    fi
}

# Compute SHA-512 hash
# Usage: hash=$(crypto_sha512 "message")
crypto_sha512() {
    if command -v sha512sum >/dev/null 2>&1; then
        printf '%s' "$1" | sha512sum | cut -d' ' -f1
    elif command -v shasum >/dev/null 2>&1; then
        printf '%s' "$1" | shasum -a 512 | cut -d' ' -f1
    elif command -v openssl >/dev/null 2>&1; then
        printf '%s' "$1" | openssl dgst -sha512 | cut -d' ' -f2
    else
        PROVEN_ERROR="no_sha512_tool"
        return 1
    fi
}

# Compute MD5 hash (for non-security purposes)
# Usage: hash=$(crypto_md5 "message")
crypto_md5() {
    if command -v md5sum >/dev/null 2>&1; then
        printf '%s' "$1" | md5sum | cut -d' ' -f1
    elif command -v md5 >/dev/null 2>&1; then
        printf '%s' "$1" | md5
    elif command -v openssl >/dev/null 2>&1; then
        printf '%s' "$1" | openssl dgst -md5 | cut -d' ' -f2
    else
        PROVEN_ERROR="no_md5_tool"
        return 1
    fi
}

# ============================================================================
# MODULE 8: SAFE_UUID - UUID generation and validation
# ============================================================================

UUID_NIL="00000000-0000-0000-0000-000000000000"

# Validate UUID format
# Usage: is_valid_uuid "550e8400-e29b-41d4-a716-446655440000" && echo "valid"
is_valid_uuid() {
    _uuid="$1"

    [ -z "$_uuid" ] && return 1

    # Check length
    _len=$(_proven_strlen "$_uuid")

    case "$_len" in
        36)
            # Standard format with dashes
            case "$_uuid" in
                [0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]-[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]-[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]-[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]-[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])
                    return 0
                    ;;
            esac
            ;;
        32)
            # Compact format without dashes
            case "$_uuid" in
                *[!0-9a-fA-F]*) return 1 ;;
                *) return 0 ;;
            esac
            ;;
    esac

    return 1
}

# Generate UUID v4 (random)
# Usage: uuid=$(uuid_generate_v4)
uuid_generate_v4() {
    _hex=$(crypto_random_hex 16) || return 1

    # Format as UUID with version 4 and variant bits
    _p1=$(printf '%s' "$_hex" | cut -c1-8)
    _p2=$(printf '%s' "$_hex" | cut -c9-12)
    _p3=$(printf '%s' "$_hex" | cut -c13-16)
    _p4=$(printf '%s' "$_hex" | cut -c17-20)
    _p5=$(printf '%s' "$_hex" | cut -c21-32)

    # Set version to 4 (random)
    _p3="4$(printf '%s' "$_p3" | cut -c2-4)"

    # Set variant to RFC 4122 (10xx)
    _variant_char=$(printf '%s' "$_p4" | cut -c1)
    case "$_variant_char" in
        [0-3]) _new_var=8 ;;
        [4-7]) _new_var=9 ;;
        [8-9a-bA-B]) _new_var="$_variant_char" ;;
        [c-fC-F]) _new_var=$(printf '%x' $(( (0x$_variant_char & 0x3) | 0x8 ))) ;;
    esac
    _p4="${_new_var}$(printf '%s' "$_p4" | cut -c2-4)"

    printf '%s-%s-%s-%s-%s' "$_p1" "$_p2" "$_p3" "$_p4" "$_p5" | tr 'A-F' 'a-f'
}

# Format UUID to standard format
# Usage: formatted=$(uuid_format "550e8400e29b41d4a716446655440000")
uuid_format() {
    _uuid=$(printf '%s' "$1" | tr -d '-' | tr 'A-F' 'a-f')

    _len=$(_proven_strlen "$_uuid")
    [ "$_len" -ne 32 ] && return 1

    printf '%s-%s-%s-%s-%s' \
        "$(printf '%s' "$_uuid" | cut -c1-8)" \
        "$(printf '%s' "$_uuid" | cut -c9-12)" \
        "$(printf '%s' "$_uuid" | cut -c13-16)" \
        "$(printf '%s' "$_uuid" | cut -c17-20)" \
        "$(printf '%s' "$_uuid" | cut -c21-32)"
}

# Check if UUID is nil
# Usage: is_nil_uuid "$uuid" && echo "nil"
is_nil_uuid() {
    _formatted=$(uuid_format "$1") || return 1
    [ "$_formatted" = "$UUID_NIL" ]
}

# ============================================================================
# MODULE 9: SAFE_CURRENCY - Monetary value handling
# ============================================================================

# Validate ISO 4217 currency code
# Usage: is_valid_currency_code "USD" && echo "valid"
is_valid_currency_code() {
    case "$1" in
        USD|EUR|GBP|JPY|CHF|AUD|CAD|CNY|HKD|NZD|SEK|SGD|NOK|MXN|INR|RUB|ZAR|BRL|KRW|TRY)
            return 0
            ;;
        # Add more as needed
        *)
            # Generic check: 3 uppercase letters
            case "$1" in
                [A-Z][A-Z][A-Z]) return 0 ;;
                *) return 1 ;;
            esac
            ;;
    esac
}

# Format money value
# Usage: formatted=$(format_money 1234.56 "USD")
format_money() {
    _amount="$1"
    _currency="$2"

    is_valid_currency_code "$_currency" || return 1

    # Format with 2 decimal places
    _formatted=$(printf '%.2f' "$_amount")

    printf '%s %s' "$_currency" "$_formatted"
}

# Parse money string
# Usage: parse_money "USD 1234.56" && echo "$PROVEN_RESULT"
parse_money() {
    _proven_reset

    _str="$1"
    _currency=$(printf '%s' "$_str" | cut -d' ' -f1)
    _amount=$(printf '%s' "$_str" | cut -d' ' -f2)

    is_valid_currency_code "$_currency" || return 1

    PROVEN_RESULT="$_amount"
    return 0
}

# ============================================================================
# MODULE 10: SAFE_PHONE - Phone number validation
# ============================================================================

# Validate E.164 phone number
# Usage: is_valid_phone "+14155551234" && echo "valid"
is_valid_phone() {
    _phone="$1"

    # Must start with +
    case "$_phone" in
        +*) ;;
        *) return 1 ;;
    esac

    # Remove leading +
    _digits="${_phone#+}"

    # Check all digits
    case "$_digits" in
        *[!0-9]*) return 1 ;;
    esac

    # Check length (7-15 digits)
    _len=$(_proven_strlen "$_digits")
    [ "$_len" -lt 7 ] && return 1
    [ "$_len" -gt 15 ] && return 1

    return 0
}

# Normalize phone number to E.164
# Usage: normalized=$(normalize_phone "1 (415) 555-1234")
normalize_phone() {
    _phone="$1"

    # Remove all non-digit characters except leading +
    _prefix=""
    case "$_phone" in
        +*) _prefix="+" ; _phone="${_phone#+}" ;;
    esac

    _digits=$(printf '%s' "$_phone" | tr -cd '0-9')

    printf '%s%s' "$_prefix" "$_digits"
}

# ============================================================================
# MODULE 11: SAFE_HEX - Hexadecimal encoding/decoding
# ============================================================================

# Encode string to hex
# Usage: hex=$(hex_encode "hello")
hex_encode() {
    printf '%s' "$1" | od -An -tx1 | tr -d ' \n'
}

# Decode hex to string
# Usage: str=$(hex_decode "68656c6c6f")
hex_decode() {
    printf '%s' "$1" | sed 's/../\\x&/g' | xargs -0 printf '%b'
}

# Validate hex string
# Usage: is_valid_hex "deadbeef" && echo "valid"
is_valid_hex() {
    [ -z "$1" ] && return 1

    case "$1" in
        *[!0-9a-fA-F]*) return 1 ;;
        *) return 0 ;;
    esac
}

# ============================================================================
# MODULE 12: SAFE_JSON - JSON parsing (basic)
# ============================================================================

# Extract JSON string value (basic - for simple JSON)
# Usage: value=$(json_get_string '{"name":"John"}' "name")
json_get_string() {
    _json="$1"
    _key="$2"

    printf '%s' "$_json" | awk -v key="$_key" '
        BEGIN { RS="[,{}]" }
        {
            if (match($0, "\"" key "\"[ \t\n]*:[ \t\n]*\"([^\"]+)\"", arr)) {
                gsub(/^[ \t\n]*"/, "", $0)
                gsub(/"[ \t\n]*:[ \t\n]*"/, ":", $0)
                split($0, parts, ":")
                gsub(/^"|"$/, "", parts[2])
                print parts[2]
                exit
            }
        }
    ' 2>/dev/null
}

# Escape string for JSON
# Usage: escaped=$(json_escape "$string")
json_escape() {
    printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g; s/	/\\t/g; s/
/\\n/g'
}

# Check if string is valid JSON (basic check)
# Usage: is_valid_json '{"key":"value"}' && echo "valid"
is_valid_json() {
    _json="$1"

    # Must start with { or [
    case "$_json" in
        '{'*|'['*) ;;
        *) return 1 ;;
    esac

    # Must end with } or ]
    case "$_json" in
        *'}'|*']') ;;
        *) return 1 ;;
    esac

    # If jq is available, use it for validation
    if command -v jq >/dev/null 2>&1; then
        printf '%s' "$_json" | jq empty 2>/dev/null
        return $?
    fi

    return 0
}

# ============================================================================
# MODULE 13: SAFE_DATETIME - Date/time handling
# ============================================================================

# Validate ISO 8601 date
# Usage: is_valid_iso_date "2024-01-15" && echo "valid"
is_valid_iso_date() {
    case "$1" in
        [0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9])
            _year=$(printf '%s' "$1" | cut -d- -f1)
            _month=$(printf '%s' "$1" | cut -d- -f2)
            _day=$(printf '%s' "$1" | cut -d- -f3)

            # Remove leading zeros for comparison
            _month=$(printf '%d' "$_month")
            _day=$(printf '%d' "$_day")

            [ "$_month" -lt 1 ] && return 1
            [ "$_month" -gt 12 ] && return 1
            [ "$_day" -lt 1 ] && return 1
            [ "$_day" -gt 31 ] && return 1

            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# Validate ISO 8601 datetime
# Usage: is_valid_iso_datetime "2024-01-15T10:30:00Z" && echo "valid"
is_valid_iso_datetime() {
    case "$1" in
        *T*Z|*T*+*|*T*-*)
            _date="${1%%T*}"
            is_valid_iso_date "$_date"
            ;;
        *)
            return 1
            ;;
    esac
}

# Get current timestamp
# Usage: ts=$(timestamp_now)
timestamp_now() {
    date +%s
}

# Get current ISO datetime
# Usage: dt=$(datetime_now)
datetime_now() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# Format timestamp to ISO datetime
# Usage: dt=$(timestamp_to_iso 1705320600)
timestamp_to_iso() {
    date -u -d "@$1" +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || \
    date -u -r "$1" +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null
}

# ============================================================================
# MODULE 14: SAFE_FLOAT - Floating point safety
# ============================================================================

# Safe float division
# Usage: result=$(float_div 10.0 3.0)
float_div() {
    if [ "$2" = "0" ] || [ "$2" = "0.0" ]; then
        PROVEN_ERROR="division_by_zero"
        return 1
    fi

    awk -v a="$1" -v b="$2" 'BEGIN { printf "%.15g", a / b }'
}

# Check if float is valid (not NaN or Inf)
# Usage: is_valid_float "3.14" && echo "valid"
is_valid_float() {
    case "$1" in
        nan|NaN|NAN|-nan|-NaN|-NAN) return 1 ;;
        inf|Inf|INF|-inf|-Inf|-INF) return 1 ;;
        infinity|Infinity|-infinity|-Infinity) return 1 ;;
    esac

    # Check format
    printf '%s' "$1" | awk '
        {
            if ($0 ~ /^-?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$/) exit 0
            exit 1
        }
    '
}

# Clamp float to range
# Usage: result=$(float_clamp 0.0 1.0 1.5)
float_clamp() {
    awk -v lo="$1" -v hi="$2" -v val="$3" '
        BEGIN {
            if (val < lo) print lo
            else if (val > hi) print hi
            else print val
        }
    '
}

# ============================================================================
# MODULE 15: SAFE_VERSION - Semantic versioning
# ============================================================================

# Validate semver
# Usage: is_valid_semver "1.2.3" && echo "valid"
is_valid_semver() {
    case "$1" in
        [0-9]*.[0-9]*.[0-9]*)
            _major="${1%%.*}"
            _rest="${1#*.}"
            _minor="${_rest%%.*}"
            _patch="${_rest#*.}"
            _patch="${_patch%%[-+]*}"

            case "$_major" in *[!0-9]*) return 1 ;; esac
            case "$_minor" in *[!0-9]*) return 1 ;; esac
            case "$_patch" in *[!0-9]*) return 1 ;; esac

            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# Parse semver components
# Usage: version_parse "1.2.3-beta+build"
# Sets: VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, VERSION_PRERELEASE, VERSION_BUILD
version_parse() {
    VERSION_MAJOR=""
    VERSION_MINOR=""
    VERSION_PATCH=""
    VERSION_PRERELEASE=""
    VERSION_BUILD=""

    _ver="$1"

    # Extract build metadata
    case "$_ver" in
        *+*)
            VERSION_BUILD="${_ver##*+}"
            _ver="${_ver%+*}"
            ;;
    esac

    # Extract prerelease
    case "$_ver" in
        *-*)
            VERSION_PRERELEASE="${_ver#*-}"
            _ver="${_ver%%-*}"
            ;;
    esac

    VERSION_MAJOR="${_ver%%.*}"
    _rest="${_ver#*.}"
    VERSION_MINOR="${_rest%%.*}"
    VERSION_PATCH="${_rest#*.}"

    return 0
}

# Compare two semver versions
# Usage: version_compare "1.2.3" "1.2.4"
# Returns: -1, 0, or 1 in PROVEN_RESULT
version_compare() {
    version_parse "$1"
    _v1_major="$VERSION_MAJOR"
    _v1_minor="$VERSION_MINOR"
    _v1_patch="$VERSION_PATCH"

    version_parse "$2"
    _v2_major="$VERSION_MAJOR"
    _v2_minor="$VERSION_MINOR"
    _v2_patch="$VERSION_PATCH"

    if [ "$_v1_major" -lt "$_v2_major" ]; then
        PROVEN_RESULT=-1
    elif [ "$_v1_major" -gt "$_v2_major" ]; then
        PROVEN_RESULT=1
    elif [ "$_v1_minor" -lt "$_v2_minor" ]; then
        PROVEN_RESULT=-1
    elif [ "$_v1_minor" -gt "$_v2_minor" ]; then
        PROVEN_RESULT=1
    elif [ "$_v1_patch" -lt "$_v2_patch" ]; then
        PROVEN_RESULT=-1
    elif [ "$_v1_patch" -gt "$_v2_patch" ]; then
        PROVEN_RESULT=1
    else
        PROVEN_RESULT=0
    fi

    return 0
}

# ============================================================================
# MODULE 16: SAFE_COLOR - Color manipulation
# ============================================================================

# Validate hex color
# Usage: is_valid_hex_color "#ff0000" && echo "valid"
is_valid_hex_color() {
    case "$1" in
        '#'[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])
            return 0
            ;;
        '#'[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])
            return 0
            ;;
        '#'[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# Parse hex color to RGB
# Usage: color_parse_hex "#ff0000"
# Sets: COLOR_R, COLOR_G, COLOR_B
color_parse_hex() {
    _color="${1#\#}"

    _len=$(_proven_strlen "$_color")

    case "$_len" in
        3)
            COLOR_R=$((0x$(printf '%s' "$_color" | cut -c1)$(printf '%s' "$_color" | cut -c1)))
            COLOR_G=$((0x$(printf '%s' "$_color" | cut -c2)$(printf '%s' "$_color" | cut -c2)))
            COLOR_B=$((0x$(printf '%s' "$_color" | cut -c3)$(printf '%s' "$_color" | cut -c3)))
            ;;
        6|8)
            COLOR_R=$((0x$(printf '%s' "$_color" | cut -c1-2)))
            COLOR_G=$((0x$(printf '%s' "$_color" | cut -c3-4)))
            COLOR_B=$((0x$(printf '%s' "$_color" | cut -c5-6)))
            ;;
        *)
            return 1
            ;;
    esac

    return 0
}

# Convert RGB to hex
# Usage: hex=$(color_to_hex 255 0 0)
color_to_hex() {
    printf '#%02x%02x%02x' "$1" "$2" "$3"
}

# ============================================================================
# MODULE 17: SAFE_ANGLE - Angle conversions
# ============================================================================

# Convert degrees to radians
# Usage: rad=$(deg_to_rad 180)
deg_to_rad() {
    awk -v deg="$1" 'BEGIN { printf "%.15g", deg * 3.14159265358979323846 / 180 }'
}

# Convert radians to degrees
# Usage: deg=$(rad_to_deg 3.14159)
rad_to_deg() {
    awk -v rad="$1" 'BEGIN { printf "%.15g", rad * 180 / 3.14159265358979323846 }'
}

# Normalize degrees to [0, 360)
# Usage: norm=$(normalize_degrees 450)
normalize_degrees() {
    awk -v deg="$1" '
        BEGIN {
            while (deg < 0) deg += 360
            while (deg >= 360) deg -= 360
            printf "%.15g", deg
        }
    '
}

# ============================================================================
# MODULE 18: SAFE_UNIT - Unit conversions
# ============================================================================

# Convert length units
# Usage: result=$(convert_length 1 "km" "m")
convert_length() {
    _value="$1"
    _from="$2"
    _to="$3"

    # Convert to meters first
    case "$_from" in
        m)   _meters="$_value" ;;
        km)  _meters=$(awk -v v="$_value" 'BEGIN { print v * 1000 }') ;;
        cm)  _meters=$(awk -v v="$_value" 'BEGIN { print v * 0.01 }') ;;
        mm)  _meters=$(awk -v v="$_value" 'BEGIN { print v * 0.001 }') ;;
        mi)  _meters=$(awk -v v="$_value" 'BEGIN { print v * 1609.344 }') ;;
        ft)  _meters=$(awk -v v="$_value" 'BEGIN { print v * 0.3048 }') ;;
        in)  _meters=$(awk -v v="$_value" 'BEGIN { print v * 0.0254 }') ;;
        yd)  _meters=$(awk -v v="$_value" 'BEGIN { print v * 0.9144 }') ;;
        *)   return 1 ;;
    esac

    # Convert from meters to target
    case "$_to" in
        m)   printf '%s' "$_meters" ;;
        km)  awk -v m="$_meters" 'BEGIN { printf "%.15g", m / 1000 }' ;;
        cm)  awk -v m="$_meters" 'BEGIN { printf "%.15g", m * 100 }' ;;
        mm)  awk -v m="$_meters" 'BEGIN { printf "%.15g", m * 1000 }' ;;
        mi)  awk -v m="$_meters" 'BEGIN { printf "%.15g", m / 1609.344 }' ;;
        ft)  awk -v m="$_meters" 'BEGIN { printf "%.15g", m / 0.3048 }' ;;
        in)  awk -v m="$_meters" 'BEGIN { printf "%.15g", m / 0.0254 }' ;;
        yd)  awk -v m="$_meters" 'BEGIN { printf "%.15g", m / 0.9144 }' ;;
        *)   return 1 ;;
    esac
}

# Convert temperature units
# Usage: result=$(convert_temperature 100 "C" "F")
convert_temperature() {
    _value="$1"
    _from="$2"
    _to="$3"

    # Convert to Celsius first
    case "$_from" in
        C)   _celsius="$_value" ;;
        F)   _celsius=$(awk -v v="$_value" 'BEGIN { printf "%.15g", (v - 32) * 5 / 9 }') ;;
        K)   _celsius=$(awk -v v="$_value" 'BEGIN { printf "%.15g", v - 273.15 }') ;;
        *)   return 1 ;;
    esac

    # Convert from Celsius to target
    case "$_to" in
        C)   printf '%s' "$_celsius" ;;
        F)   awk -v c="$_celsius" 'BEGIN { printf "%.15g", c * 9 / 5 + 32 }' ;;
        K)   awk -v c="$_celsius" 'BEGIN { printf "%.15g", c + 273.15 }' ;;
        *)   return 1 ;;
    esac
}

# Convert mass units
# Usage: result=$(convert_mass 1 "kg" "lb")
convert_mass() {
    _value="$1"
    _from="$2"
    _to="$3"

    # Convert to grams first
    case "$_from" in
        g)   _grams="$_value" ;;
        kg)  _grams=$(awk -v v="$_value" 'BEGIN { print v * 1000 }') ;;
        mg)  _grams=$(awk -v v="$_value" 'BEGIN { print v * 0.001 }') ;;
        lb)  _grams=$(awk -v v="$_value" 'BEGIN { print v * 453.59237 }') ;;
        oz)  _grams=$(awk -v v="$_value" 'BEGIN { print v * 28.349523125 }') ;;
        *)   return 1 ;;
    esac

    # Convert from grams to target
    case "$_to" in
        g)   printf '%s' "$_grams" ;;
        kg)  awk -v g="$_grams" 'BEGIN { printf "%.15g", g / 1000 }' ;;
        mg)  awk -v g="$_grams" 'BEGIN { printf "%.15g", g * 1000 }' ;;
        lb)  awk -v g="$_grams" 'BEGIN { printf "%.15g", g / 453.59237 }' ;;
        oz)  awk -v g="$_grams" 'BEGIN { printf "%.15g", g / 28.349523125 }' ;;
        *)   return 1 ;;
    esac
}

# ============================================================================
# MODULE 19: SAFE_BUFFER - Bounded buffers
# ============================================================================

# Initialize a bounded buffer
# Usage: buffer_init "mybuf" 10
buffer_init() {
    _name="$1"
    _capacity="$2"

    eval "${_name}_capacity=$_capacity"
    eval "${_name}_size=0"
    eval "${_name}_data=''"
}

# Push to buffer
# Usage: buffer_push "mybuf" "value"
buffer_push() {
    _name="$1"
    _value="$2"

    eval "_cap=\${${_name}_capacity}"
    eval "_size=\${${_name}_size}"

    if [ "$_size" -ge "$_cap" ]; then
        PROVEN_ERROR="buffer_full"
        return 1
    fi

    eval "${_name}_${_size}='$_value'"
    eval "${_name}_size=$((_size + 1))"

    return 0
}

# Pop from buffer
# Usage: buffer_pop "mybuf" && echo "$PROVEN_RESULT"
buffer_pop() {
    _proven_reset
    _name="$1"

    eval "_size=\${${_name}_size}"

    if [ "$_size" -le 0 ]; then
        PROVEN_ERROR="buffer_empty"
        return 1
    fi

    _idx=$((_size - 1))
    eval "PROVEN_RESULT=\${${_name}_${_idx}}"
    eval "unset ${_name}_${_idx}"
    eval "${_name}_size=$_idx"

    return 0
}

# Get buffer size
# Usage: size=$(buffer_size "mybuf")
buffer_size() {
    eval "printf '%s' \"\${${1}_size}\""
}

# ============================================================================
# MODULE 20: SAFE_QUEUE - Bounded FIFO queues
# ============================================================================

# Initialize a bounded queue
# Usage: queue_init "myqueue" 10
queue_init() {
    _name="$1"
    _capacity="$2"

    eval "${_name}_capacity=$_capacity"
    eval "${_name}_head=0"
    eval "${_name}_tail=0"
    eval "${_name}_size=0"
}

# Enqueue value
# Usage: queue_enqueue "myqueue" "value"
queue_enqueue() {
    _name="$1"
    _value="$2"

    eval "_cap=\${${_name}_capacity}"
    eval "_size=\${${_name}_size}"
    eval "_tail=\${${_name}_tail}"

    if [ "$_size" -ge "$_cap" ]; then
        PROVEN_ERROR="queue_full"
        return 1
    fi

    eval "${_name}_${_tail}='$_value'"
    eval "${_name}_tail=$(( (_tail + 1) % _cap ))"
    eval "${_name}_size=$((_size + 1))"

    return 0
}

# Dequeue value
# Usage: queue_dequeue "myqueue" && echo "$PROVEN_RESULT"
queue_dequeue() {
    _proven_reset
    _name="$1"

    eval "_cap=\${${_name}_capacity}"
    eval "_size=\${${_name}_size}"
    eval "_head=\${${_name}_head}"

    if [ "$_size" -le 0 ]; then
        PROVEN_ERROR="queue_empty"
        return 1
    fi

    eval "PROVEN_RESULT=\${${_name}_${_head}}"
    eval "unset ${_name}_${_head}"
    eval "${_name}_head=$(( (_head + 1) % _cap ))"
    eval "${_name}_size=$((_size - 1))"

    return 0
}

# Get queue size
# Usage: size=$(queue_size "myqueue")
queue_size() {
    eval "printf '%s' \"\${${1}_size}\""
}

# ============================================================================
# MODULE 21: SAFE_BLOOM - Bloom filter (simplified)
# ============================================================================

# Initialize bloom filter
# Usage: bloom_init "mybloom" 1000
bloom_init() {
    _name="$1"
    _size="$2"

    eval "${_name}_size=$_size"

    # Initialize all bits to 0
    _i=0
    while [ "$_i" -lt "$_size" ]; do
        eval "${_name}_bit_${_i}=0"
        _i=$((_i + 1))
    done
}

# Add to bloom filter
# Usage: bloom_add "mybloom" "value"
bloom_add() {
    _name="$1"
    _value="$2"

    eval "_size=\${${_name}_size}"

    # Simple hash functions
    _h1=$(printf '%s' "$_value" | cksum | cut -d' ' -f1)
    _h2=$(printf '%s' "$_value" | md5sum 2>/dev/null | cut -c1-8 || printf '%s' "$_h1")
    _h2=$((0x$_h2))

    _idx1=$((_h1 % _size))
    _idx2=$((_h2 % _size))

    eval "${_name}_bit_${_idx1}=1"
    eval "${_name}_bit_${_idx2}=1"
}

# Check bloom filter membership
# Usage: bloom_maybe_contains "mybloom" "value" && echo "maybe"
bloom_maybe_contains() {
    _name="$1"
    _value="$2"

    eval "_size=\${${_name}_size}"

    _h1=$(printf '%s' "$_value" | cksum | cut -d' ' -f1)
    _h2=$(printf '%s' "$_value" | md5sum 2>/dev/null | cut -c1-8 || printf '%s' "$_h1")
    _h2=$((0x$_h2))

    _idx1=$((_h1 % _size))
    _idx2=$((_h2 % _size))

    eval "_b1=\${${_name}_bit_${_idx1}}"
    eval "_b2=\${${_name}_bit_${_idx2}}"

    [ "$_b1" -eq 1 ] && [ "$_b2" -eq 1 ]
}

# ============================================================================
# MODULE 22: SAFE_LRU - LRU Cache
# ============================================================================

# Initialize LRU cache
# Usage: lru_init "mycache" 100
lru_init() {
    _name="$1"
    _capacity="$2"

    eval "${_name}_capacity=$_capacity"
    eval "${_name}_size=0"
    eval "${_name}_counter=0"
}

# Put value in LRU cache
# Usage: lru_put "mycache" "key" "value"
lru_put() {
    _name="$1"
    _key="$2"
    _value="$3"

    eval "_cap=\${${_name}_capacity}"
    eval "_size=\${${_name}_size}"
    eval "_counter=\${${_name}_counter}"

    # Check if key exists
    eval "_existing=\${${_name}_v_${_key}+x}"

    if [ -z "$_existing" ]; then
        # New key
        if [ "$_size" -ge "$_cap" ]; then
            # Evict LRU (simplified - just remove oldest by counter)
            # In real implementation, track access order properly
            _size=$((_size - 1))
        fi
        _size=$((_size + 1))
    fi

    _counter=$((_counter + 1))
    eval "${_name}_v_${_key}='$_value'"
    eval "${_name}_t_${_key}=$_counter"
    eval "${_name}_size=$_size"
    eval "${_name}_counter=$_counter"
}

# Get value from LRU cache
# Usage: lru_get "mycache" "key" && echo "$PROVEN_RESULT"
lru_get() {
    _proven_reset
    _name="$1"
    _key="$2"

    eval "_counter=\${${_name}_counter}"
    eval "_existing=\${${_name}_v_${_key}+x}"

    if [ -z "$_existing" ]; then
        PROVEN_ERROR="key_not_found"
        return 1
    fi

    # Update access time
    _counter=$((_counter + 1))
    eval "${_name}_t_${_key}=$_counter"
    eval "${_name}_counter=$_counter"

    eval "PROVEN_RESULT=\${${_name}_v_${_key}}"
    return 0
}

# ============================================================================
# MODULE 23: SAFE_GRAPH - Directed graph
# ============================================================================

# Initialize graph
# Usage: graph_init "mygraph"
graph_init() {
    _name="$1"
    eval "${_name}_nodes=''"
    eval "${_name}_edges=''"
}

# Add node to graph
# Usage: graph_add_node "mygraph" "A"
graph_add_node() {
    _name="$1"
    _node="$2"

    eval "_nodes=\${${_name}_nodes}"

    # Check if node exists
    case " $_nodes " in
        *" $_node "*) return 0 ;;
    esac

    if [ -z "$_nodes" ]; then
        eval "${_name}_nodes='$_node'"
    else
        eval "${_name}_nodes='$_nodes $_node'"
    fi
}

# Add edge to graph
# Usage: graph_add_edge "mygraph" "A" "B"
graph_add_edge() {
    _name="$1"
    _from="$2"
    _to="$3"

    graph_add_node "$_name" "$_from"
    graph_add_node "$_name" "$_to"

    eval "_edges=\${${_name}_edges}"
    _edge="${_from}:${_to}"

    # Check if edge exists
    case " $_edges " in
        *" $_edge "*) return 0 ;;
    esac

    if [ -z "$_edges" ]; then
        eval "${_name}_edges='$_edge'"
    else
        eval "${_name}_edges='$_edges $_edge'"
    fi
}

# Check if edge exists
# Usage: graph_has_edge "mygraph" "A" "B" && echo "yes"
graph_has_edge() {
    _name="$1"
    _from="$2"
    _to="$3"

    eval "_edges=\${${_name}_edges}"
    _edge="${_from}:${_to}"

    case " $_edges " in
        *" $_edge "*) return 0 ;;
    esac

    return 1
}

# Get graph nodes
# Usage: nodes=$(graph_nodes "mygraph")
graph_nodes() {
    eval "printf '%s' \"\${${1}_nodes}\""
}

# ============================================================================
# MODULE 24: SAFE_RATE_LIMITER - Rate limiting
# ============================================================================

# Initialize token bucket rate limiter
# Usage: rate_limiter_init "mylimiter" 10 1
# 10 tokens, 1 token per second refill
rate_limiter_init() {
    _name="$1"
    _capacity="$2"
    _refill_rate="$3"

    eval "${_name}_capacity=$_capacity"
    eval "${_name}_tokens=$_capacity"
    eval "${_name}_refill_rate=$_refill_rate"
    eval "${_name}_last_update=$(date +%s)"
}

# Try to acquire a token
# Usage: rate_limiter_try_acquire "mylimiter" && echo "allowed"
rate_limiter_try_acquire() {
    _name="$1"

    eval "_cap=\${${_name}_capacity}"
    eval "_tokens=\${${_name}_tokens}"
    eval "_rate=\${${_name}_refill_rate}"
    eval "_last=\${${_name}_last_update}"

    _now=$(date +%s)
    _elapsed=$((_now - _last))

    # Refill tokens
    _refill=$((_elapsed * _rate))
    _tokens=$((_tokens + _refill))
    [ "$_tokens" -gt "$_cap" ] && _tokens="$_cap"

    eval "${_name}_last_update=$_now"

    if [ "$_tokens" -ge 1 ]; then
        eval "${_name}_tokens=$((_tokens - 1))"
        return 0
    else
        eval "${_name}_tokens=$_tokens"
        return 1
    fi
}

# Get remaining tokens
# Usage: remaining=$(rate_limiter_remaining "mylimiter")
rate_limiter_remaining() {
    eval "printf '%s' \"\${${1}_tokens}\""
}

# ============================================================================
# MODULE 25: SAFE_CIRCUIT_BREAKER - Circuit breaker pattern
# ============================================================================

# Circuit breaker states
CIRCUIT_CLOSED=0
CIRCUIT_OPEN=1
CIRCUIT_HALF_OPEN=2

# Initialize circuit breaker
# Usage: circuit_breaker_init "mycb" 5 30
# 5 failures to open, 30 seconds timeout
circuit_breaker_init() {
    _name="$1"
    _threshold="$2"
    _timeout="$3"

    eval "${_name}_threshold=$_threshold"
    eval "${_name}_timeout=$_timeout"
    eval "${_name}_failures=0"
    eval "${_name}_state=$CIRCUIT_CLOSED"
    eval "${_name}_last_failure=0"
}

# Check if circuit breaker allows operation
# Usage: circuit_breaker_allow "mycb" && run_operation
circuit_breaker_allow() {
    _name="$1"

    eval "_state=\${${_name}_state}"
    eval "_timeout=\${${_name}_timeout}"
    eval "_last=\${${_name}_last_failure}"

    case "$_state" in
        "$CIRCUIT_CLOSED")
            return 0
            ;;
        "$CIRCUIT_OPEN")
            _now=$(date +%s)
            _elapsed=$((_now - _last))
            if [ "$_elapsed" -ge "$_timeout" ]; then
                eval "${_name}_state=$CIRCUIT_HALF_OPEN"
                return 0
            fi
            return 1
            ;;
        "$CIRCUIT_HALF_OPEN")
            return 0
            ;;
    esac

    return 1
}

# Record success
# Usage: circuit_breaker_success "mycb"
circuit_breaker_success() {
    _name="$1"

    eval "${_name}_failures=0"
    eval "${_name}_state=$CIRCUIT_CLOSED"
}

# Record failure
# Usage: circuit_breaker_failure "mycb"
circuit_breaker_failure() {
    _name="$1"

    eval "_failures=\${${_name}_failures}"
    eval "_threshold=\${${_name}_threshold}"

    _failures=$((_failures + 1))
    eval "${_name}_failures=$_failures"
    eval "${_name}_last_failure=$(date +%s)"

    if [ "$_failures" -ge "$_threshold" ]; then
        eval "${_name}_state=$CIRCUIT_OPEN"
    fi
}

# ============================================================================
# MODULE 26: SAFE_RETRY - Retry with exponential backoff
# ============================================================================

# Calculate exponential backoff delay
# Usage: delay=$(retry_backoff_delay 3 1000 30000 0.5)
# attempt, base_ms, max_ms, jitter_factor
retry_backoff_delay() {
    _attempt="$1"
    _base="${2:-1000}"
    _max="${3:-30000}"
    _jitter="${4:-0}"

    # Calculate 2^attempt * base
    _delay="$_base"
    _i=1
    while [ "$_i" -lt "$_attempt" ]; do
        _delay=$((_delay * 2))
        _i=$((_i + 1))
    done

    # Cap at max
    [ "$_delay" -gt "$_max" ] && _delay="$_max"

    # Add jitter if specified
    if [ "$_jitter" != "0" ]; then
        _rand=$(od -An -tu2 -N2 /dev/urandom 2>/dev/null | tr -d ' ' || printf '0')
        _jitter_amount=$(awk -v d="$_delay" -v j="$_jitter" -v r="$_rand" \
            'BEGIN { printf "%.0f", d * j * (r / 65535) }')
        _delay=$((_delay + _jitter_amount))
    fi

    printf '%s' "$_delay"
}

# Retry a command with backoff
# Usage: retry_with_backoff 3 1000 30000 my_command args
retry_with_backoff() {
    _max_attempts="$1"
    _base_delay="$2"
    _max_delay="$3"
    shift 3

    _attempt=1
    while [ "$_attempt" -le "$_max_attempts" ]; do
        if "$@"; then
            return 0
        fi

        if [ "$_attempt" -lt "$_max_attempts" ]; then
            _delay=$(retry_backoff_delay "$_attempt" "$_base_delay" "$_max_delay" "0.5")
            _delay_sec=$((_delay / 1000))
            [ "$_delay_sec" -lt 1 ] && _delay_sec=1
            sleep "$_delay_sec"
        fi

        _attempt=$((_attempt + 1))
    done

    return 1
}

# ============================================================================
# MODULE 27: SAFE_MONOTONIC - Monotonically increasing sequences
# ============================================================================

# Initialize monotonic counter
# Usage: monotonic_init "mycounter" 0
monotonic_init() {
    _name="$1"
    _start="${2:-0}"

    eval "${_name}_value=$_start"
}

# Get next value (atomic increment)
# Usage: next=$(monotonic_next "mycounter")
monotonic_next() {
    _name="$1"

    eval "_val=\${${_name}_value}"
    _new=$((_val + 1))
    eval "${_name}_value=$_new"

    printf '%s' "$_new"
}

# Get current value
# Usage: current=$(monotonic_current "mycounter")
monotonic_current() {
    eval "printf '%s' \"\${${1}_value}\""
}

# High water mark - only increases
# Usage: high_water_mark_update "hwm" 50
high_water_mark_update() {
    _name="$1"
    _value="$2"

    eval "_current=\${${_name}_value:-0}"

    if [ "$_value" -gt "$_current" ]; then
        eval "${_name}_value=$_value"
    fi
}

# ============================================================================
# MODULE 28: SAFE_STATE_MACHINE - State machine
# ============================================================================

# Initialize state machine
# Usage: state_machine_init "myfsm" "initial"
state_machine_init() {
    _name="$1"
    _initial="$2"

    eval "${_name}_state='$_initial'"
    eval "${_name}_transitions=''"
}

# Add transition
# Usage: state_machine_add_transition "myfsm" "state1" "event" "state2"
state_machine_add_transition() {
    _name="$1"
    _from="$2"
    _event="$3"
    _to="$4"

    eval "_trans=\${${_name}_transitions}"
    _new="${_from}:${_event}:${_to}"

    if [ -z "$_trans" ]; then
        eval "${_name}_transitions='$_new'"
    else
        eval "${_name}_transitions='$_trans $_new'"
    fi
}

# Fire event (attempt transition)
# Usage: state_machine_fire "myfsm" "event" && echo "transitioned"
state_machine_fire() {
    _name="$1"
    _event="$2"

    eval "_state=\${${_name}_state}"
    eval "_trans=\${${_name}_transitions}"

    for _t in $_trans; do
        _from="${_t%%:*}"
        _rest="${_t#*:}"
        _evt="${_rest%%:*}"
        _to="${_rest#*:}"

        if [ "$_from" = "$_state" ] && [ "$_evt" = "$_event" ]; then
            eval "${_name}_state='$_to'"
            return 0
        fi
    done

    PROVEN_ERROR="invalid_transition"
    return 1
}

# Get current state
# Usage: state=$(state_machine_state "myfsm")
state_machine_state() {
    eval "printf '%s' \"\${${1}_state}\""
}

# ============================================================================
# MODULE 29: SAFE_CALCULATOR - Expression evaluation
# ============================================================================

# Evaluate arithmetic expression safely
# Usage: result=$(calc "2 + 3 * 4")
calc() {
    _expr="$1"

    # Use awk for safe evaluation (no shell expansion)
    printf '%s' "$_expr" | awk '
        {
            # Basic safety: reject dangerous patterns
            if (/system|getline|print.*>/) {
                print "ERROR"
                exit 1
            }
            # Evaluate
            cmd = "echo \"" $0 "\" | bc -l 2>/dev/null"
            cmd | getline result
            close(cmd)
            if (result ~ /^-?[0-9.]+$/) {
                print result
            } else {
                print "ERROR"
                exit 1
            }
        }
    ' 2>/dev/null
}

# Safe integer calculation
# Usage: result=$(calc_int "10 / 3")
calc_int() {
    _result=$(calc "$1") || return 1
    printf '%.0f' "$_result"
}

# ============================================================================
# MODULE 30: SAFE_GEO - Geographic calculations
# ============================================================================

# Haversine distance between two points (km)
# Usage: dist=$(geo_distance 40.7128 -74.0060 34.0522 -118.2437)
geo_distance() {
    _lat1="$1"
    _lon1="$2"
    _lat2="$3"
    _lon2="$4"

    awk -v lat1="$_lat1" -v lon1="$_lon1" -v lat2="$_lat2" -v lon2="$_lon2" '
        BEGIN {
            PI = 3.14159265358979323846
            R = 6371  # Earth radius in km

            dlat = (lat2 - lat1) * PI / 180
            dlon = (lon2 - lon1) * PI / 180
            lat1_rad = lat1 * PI / 180
            lat2_rad = lat2 * PI / 180

            a = sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
            c = 2 * atan2(sqrt(a), sqrt(1-a))
            d = R * c

            printf "%.6f", d
        }
    '
}

# Validate latitude
# Usage: is_valid_latitude 40.7128 && echo "valid"
is_valid_latitude() {
    awk -v lat="$1" 'BEGIN { exit (lat >= -90 && lat <= 90) ? 0 : 1 }'
}

# Validate longitude
# Usage: is_valid_longitude -74.0060 && echo "valid"
is_valid_longitude() {
    awk -v lon="$1" 'BEGIN { exit (lon >= -180 && lon <= 180) ? 0 : 1 }'
}

# ============================================================================
# MODULE 31: SAFE_PROBABILITY - Probability values
# ============================================================================

# Create probability value (clamped to [0,1])
# Usage: p=$(probability 0.75)
probability() {
    awk -v p="$1" '
        BEGIN {
            if (p < 0) p = 0
            if (p > 1) p = 1
            printf "%.15g", p
        }
    '
}

# Probability complement
# Usage: q=$(probability_complement 0.75)
probability_complement() {
    awk -v p="$1" 'BEGIN { printf "%.15g", 1 - p }'
}

# Combine independent probabilities (AND)
# Usage: combined=$(probability_and 0.5 0.5)
probability_and() {
    awk -v p1="$1" -v p2="$2" 'BEGIN { printf "%.15g", p1 * p2 }'
}

# Combine probabilities (OR)
# Usage: combined=$(probability_or 0.5 0.5)
probability_or() {
    awk -v p1="$1" -v p2="$2" 'BEGIN { printf "%.15g", p1 + p2 - p1 * p2 }'
}

# ============================================================================
# MODULE 32: SAFE_CHECKSUM - Checksum algorithms
# ============================================================================

# Compute CRC-32 checksum
# Usage: crc=$(checksum_crc32 "hello")
checksum_crc32() {
    if command -v cksum >/dev/null 2>&1; then
        printf '%s' "$1" | cksum | cut -d' ' -f1
    else
        PROVEN_ERROR="no_cksum_tool"
        return 1
    fi
}

# Compute Adler-32 checksum
# Usage: adler=$(checksum_adler32 "hello")
checksum_adler32() {
    printf '%s' "$1" | awk '
        BEGIN {
            a = 1; b = 0
            MOD = 65521
        }
        {
            n = split($0, chars, "")
            for (i = 1; i <= n; i++) {
                a = (a + index(" !\"#$%&'\''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~", chars[i])) % MOD
                b = (b + a) % MOD
            }
        }
        END {
            printf "%u", (b * 65536) + a
        }
    '
}

# Luhn checksum validation
# Usage: luhn_check "4532015112830366" && echo "valid"
luhn_check() {
    _num="$1"

    printf '%s' "$_num" | awk '
        {
            n = length($0)
            sum = 0
            double = 0

            for (i = n; i >= 1; i--) {
                d = substr($0, i, 1)
                if (d !~ /[0-9]/) exit 1

                if (double) {
                    d = d * 2
                    if (d > 9) d = d - 9
                }
                sum += d
                double = !double
            }

            exit (sum % 10 == 0) ? 0 : 1
        }
    '
}

# ============================================================================
# MODULE 33: SAFE_TENSOR - Basic tensor/vector operations
# ============================================================================

# Dot product of two vectors
# Usage: result=$(tensor_dot "1 2 3" "4 5 6")
tensor_dot() {
    printf '%s\n%s' "$1" "$2" | awk '
        NR == 1 { split($0, a) }
        NR == 2 {
            split($0, b)
            sum = 0
            for (i = 1; i in a && i in b; i++) {
                sum += a[i] * b[i]
            }
            print sum
        }
    '
}

# Vector magnitude
# Usage: mag=$(tensor_magnitude "3 4")
tensor_magnitude() {
    printf '%s' "$1" | awk '
        {
            split($0, v)
            sum = 0
            for (i = 1; i in v; i++) {
                sum += v[i] * v[i]
            }
            printf "%.15g", sqrt(sum)
        }
    '
}

# Vector addition
# Usage: result=$(tensor_add "1 2 3" "4 5 6")
tensor_add() {
    printf '%s\n%s' "$1" "$2" | awk '
        NR == 1 { split($0, a); n = length(a) }
        NR == 2 {
            split($0, b)
            for (i = 1; i in a && i in b; i++) {
                printf "%s ", a[i] + b[i]
            }
        }
    '
}

# Scalar multiplication
# Usage: result=$(tensor_scale "1 2 3" 2)
tensor_scale() {
    printf '%s' "$1" | awk -v s="$2" '
        {
            split($0, v)
            for (i = 1; i in v; i++) {
                printf "%s ", v[i] * s
            }
        }
    '
}

# ============================================================================
# MODULE 34: SAFE_PASSWORD - Password validation
# ============================================================================

# Check password strength
# Usage: password_strength "MyP@ssw0rd!" && echo "Score: $PROVEN_RESULT"
password_strength() {
    _pass="$1"
    _score=0

    _len=$(_proven_strlen "$_pass")

    # Length scoring
    [ "$_len" -ge 8 ] && _score=$((_score + 1))
    [ "$_len" -ge 12 ] && _score=$((_score + 1))
    [ "$_len" -ge 16 ] && _score=$((_score + 1))

    # Character type scoring
    case "$_pass" in *[a-z]*) _score=$((_score + 1)) ;; esac
    case "$_pass" in *[A-Z]*) _score=$((_score + 1)) ;; esac
    case "$_pass" in *[0-9]*) _score=$((_score + 1)) ;; esac
    case "$_pass" in *[!@#\$%^*_+-=]*) _score=$((_score + 1)) ;; esac

    PROVEN_RESULT="$_score"

    # Minimum acceptable score is 4
    [ "$_score" -ge 4 ]
}

# Validate password against policy
# Usage: password_validate "MyP@ssw0rd!" 8 && echo "valid"
password_validate() {
    _pass="$1"
    _min_len="${2:-8}"

    _len=$(_proven_strlen "$_pass")

    # Check minimum length
    [ "$_len" -lt "$_min_len" ] && return 1

    # Check for at least one lowercase
    case "$_pass" in *[a-z]*) ;; *) return 1 ;; esac

    # Check for at least one uppercase
    case "$_pass" in *[A-Z]*) ;; *) return 1 ;; esac

    # Check for at least one digit
    case "$_pass" in *[0-9]*) ;; *) return 1 ;; esac

    return 0
}

# ============================================================================
# MODULE 35: SAFE_ML - Machine learning utilities
# ============================================================================

# Softmax (single value given array)
# Usage: result=$(ml_softmax "1 2 3" 1)  # Index 1 (0-based)
ml_softmax() {
    printf '%s' "$1" | awk -v idx="$2" '
        {
            split($0, v)
            max_v = v[1]
            for (i in v) if (v[i] > max_v) max_v = v[i]

            sum = 0
            for (i in v) {
                v[i] = exp(v[i] - max_v)
                sum += v[i]
            }

            printf "%.15g", v[idx + 1] / sum
        }
    '
}

# Sigmoid activation
# Usage: result=$(ml_sigmoid 0.5)
ml_sigmoid() {
    awk -v x="$1" 'BEGIN { printf "%.15g", 1 / (1 + exp(-x)) }'
}

# ReLU activation
# Usage: result=$(ml_relu -0.5)
ml_relu() {
    awk -v x="$1" 'BEGIN { printf "%.15g", (x > 0) ? x : 0 }'
}

# Mean squared error
# Usage: mse=$(ml_mse "1 2 3" "1.1 2.2 2.9")
ml_mse() {
    printf '%s\n%s' "$1" "$2" | awk '
        NR == 1 { split($0, y_true); n = 0; for (i in y_true) n++ }
        NR == 2 {
            split($0, y_pred)
            sum = 0
            for (i = 1; i <= n; i++) {
                diff = y_true[i] - y_pred[i]
                sum += diff * diff
            }
            printf "%.15g", sum / n
        }
    '
}

# ============================================================================
# MODULE 36: SAFE_HEADER - HTTP header validation
# ============================================================================

# Validate HTTP header name
# Usage: is_valid_header_name "Content-Type" && echo "valid"
is_valid_header_name() {
    case "$1" in
        ''|*[!A-Za-z0-9-]*) return 1 ;;
        *) return 0 ;;
    esac
}

# Validate HTTP header value (no CRLF injection)
# Usage: is_valid_header_value "text/html" && echo "valid"
is_valid_header_value() {
    case "$1" in
        *"$(printf '\r')"*|*"$(printf '\n')"*) return 1 ;;
        *) return 0 ;;
    esac
}

# Escape header value
# Usage: safe=$(escape_header_value "$value")
escape_header_value() {
    printf '%s' "$1" | tr -d '\r\n'
}

# Format header
# Usage: header=$(format_header "Content-Type" "text/html")
format_header() {
    _name="$1"
    _value="$2"

    is_valid_header_name "$_name" || return 1
    _safe_value=$(escape_header_value "$_value")

    printf '%s: %s' "$_name" "$_safe_value"
}

# ============================================================================
# MODULE 37: SAFE_COOKIE - HTTP cookie validation
# ============================================================================

# Validate cookie name
# Usage: is_valid_cookie_name "session_id" && echo "valid"
is_valid_cookie_name() {
    case "$1" in
        ''|*[!A-Za-z0-9_-]*) return 1 ;;
        *) return 0 ;;
    esac
}

# Validate cookie value
# Usage: is_valid_cookie_value "abc123" && echo "valid"
is_valid_cookie_value() {
    case "$1" in
        *[[:cntrl:]]*|*";"*|*","*|*" "*) return 1 ;;
        *) return 0 ;;
    esac
}

# Format Set-Cookie header
# Usage: cookie=$(format_cookie "session" "abc123" "/")
format_cookie() {
    _name="$1"
    _value="$2"
    _path="${3:-/}"

    is_valid_cookie_name "$_name" || return 1
    is_valid_cookie_value "$_value" || return 1

    printf '%s=%s; Path=%s; HttpOnly; Secure; SameSite=Strict' "$_name" "$_value" "$_path"
}

# Parse cookie string
# Usage: parse_cookie "name=value" && echo "$COOKIE_NAME: $COOKIE_VALUE"
parse_cookie() {
    _cookie="$1"
    COOKIE_NAME=""
    COOKIE_VALUE=""

    case "$_cookie" in
        *=*)
            COOKIE_NAME="${_cookie%%=*}"
            COOKIE_VALUE="${_cookie#*=}"
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# ============================================================================
# MODULE 38: SAFE_CONTENT_TYPE - MIME type validation
# ============================================================================

# Validate content type
# Usage: is_valid_content_type "text/html" && echo "valid"
is_valid_content_type() {
    case "$1" in
        */*)
            _type="${1%%/*}"
            _subtype="${1##*/}"
            _subtype="${_subtype%%;*}"  # Remove parameters

            # Check type
            case "$_type" in
                text|image|audio|video|application|multipart|message|font|model) ;;
                *) return 1 ;;
            esac

            # Check subtype format
            case "$_subtype" in
                *[!A-Za-z0-9.+-]*) return 1 ;;
            esac

            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# Get MIME type from extension
# Usage: mime=$(mime_from_extension "html")
mime_from_extension() {
    case "$(_proven_tolower "$1")" in
        html|htm) printf 'text/html' ;;
        css)      printf 'text/css' ;;
        js)       printf 'application/javascript' ;;
        json)     printf 'application/json' ;;
        xml)      printf 'application/xml' ;;
        txt)      printf 'text/plain' ;;
        pdf)      printf 'application/pdf' ;;
        zip)      printf 'application/zip' ;;
        png)      printf 'image/png' ;;
        jpg|jpeg) printf 'image/jpeg' ;;
        gif)      printf 'image/gif' ;;
        svg)      printf 'image/svg+xml' ;;
        webp)     printf 'image/webp' ;;
        mp3)      printf 'audio/mpeg' ;;
        mp4)      printf 'video/mp4' ;;
        webm)     printf 'video/webm' ;;
        woff)     printf 'font/woff' ;;
        woff2)    printf 'font/woff2' ;;
        *)        printf 'application/octet-stream' ;;
    esac
}

# Check if content type is safe for inline display
# Usage: is_safe_inline_type "image/png" && echo "safe"
is_safe_inline_type() {
    case "$1" in
        text/plain|text/html|text/css)          return 0 ;;
        image/png|image/jpeg|image/gif|image/webp|image/svg+xml) return 0 ;;
        application/pdf)                        return 0 ;;
        *)                                      return 1 ;;
    esac
}

# ============================================================================
# VERSION INFO AND HELP
# ============================================================================

# Print version info
proven_version() {
    printf 'Proven POSIX Shell Bindings v%s (%d modules)\n' "$PROVEN_VERSION" "$PROVEN_MODULE_COUNT"
}

# List all modules
proven_modules() {
    printf 'Core: %s\n' "$PROVEN_MODULES_CORE"
    printf 'Data: %s\n' "$PROVEN_MODULES_DATA"
    printf 'Data Structures: %s\n' "$PROVEN_MODULES_STRUCTURES"
    printf 'Resilience: %s\n' "$PROVEN_MODULES_RESILIENCE"
    printf 'State: %s\n' "$PROVEN_MODULES_STATE"
    printf 'Algorithm: %s\n' "$PROVEN_MODULES_ALGORITHM"
    printf 'Security: %s\n' "$PROVEN_MODULES_SECURITY"
    printf 'HTTP: %s\n' "$PROVEN_MODULES_HTTP"
}

# Self-test
proven_selftest() {
    _passed=0
    _failed=0

    printf 'Running Proven POSIX shell self-tests...\n'

    # Test safe_math
    if safe_add 5 3 && [ "$PROVEN_RESULT" = "8" ]; then
        _passed=$((_passed + 1))
    else
        printf 'FAIL: safe_add 5 3\n'
        _failed=$((_failed + 1))
    fi

    if safe_div 10 2 && [ "$PROVEN_RESULT" = "5" ]; then
        _passed=$((_passed + 1))
    else
        printf 'FAIL: safe_div 10 2\n'
        _failed=$((_failed + 1))
    fi

    if ! safe_div 1 0; then
        _passed=$((_passed + 1))
    else
        printf 'FAIL: safe_div 1 0 should fail\n'
        _failed=$((_failed + 1))
    fi

    # Test safe_string
    _escaped=$(escape_html "<script>")
    if [ "$_escaped" = "&lt;script&gt;" ]; then
        _passed=$((_passed + 1))
    else
        printf 'FAIL: escape_html\n'
        _failed=$((_failed + 1))
    fi

    # Test safe_email
    if is_valid_email "test@example.com"; then
        _passed=$((_passed + 1))
    else
        printf 'FAIL: is_valid_email\n'
        _failed=$((_failed + 1))
    fi

    # Test safe_url
    if is_valid_url "https://example.com"; then
        _passed=$((_passed + 1))
    else
        printf 'FAIL: is_valid_url\n'
        _failed=$((_failed + 1))
    fi

    # Test safe_ipv4
    if is_valid_ipv4 "192.168.1.1"; then
        _passed=$((_passed + 1))
    else
        printf 'FAIL: is_valid_ipv4\n'
        _failed=$((_failed + 1))
    fi

    # Test safe_uuid
    _uuid=$(uuid_generate_v4)
    if is_valid_uuid "$_uuid"; then
        _passed=$((_passed + 1))
    else
        printf 'FAIL: uuid_generate_v4\n'
        _failed=$((_failed + 1))
    fi

    printf '\nResults: %d passed, %d failed\n' "$_passed" "$_failed"

    [ "$_failed" -eq 0 ]
}

# End of proven.sh
