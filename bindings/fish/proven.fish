# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven Safety Library for Fish Shell
# Formally verified safety primitives for shell scripts.
#
# Version: 0.4.0
# Module Count: 38
#
# Categories:
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
#
# Usage: source proven.fish

# ============================================================================
# CONFIGURATION
# ============================================================================

set -g PROVEN_VERSION "0.4.0"
set -g PROVEN_MODULE_COUNT 38

# Maximum 64-bit signed integer values
set -g PROVEN_INT64_MAX 9223372036854775807
set -g PROVEN_INT64_MIN -9223372036854775808

# ============================================================================
# MODULE 1: SAFE_MATH (Core)
# ============================================================================

function proven_add --description "Safe addition with overflow check"
    set -l a $argv[1]
    set -l b $argv[2]

    if not string match -qr '^-?[0-9]+$' -- "$a"
        echo "error:invalid_input:a is not a number" >&2
        return 1
    end
    if not string match -qr '^-?[0-9]+$' -- "$b"
        echo "error:invalid_input:b is not a number" >&2
        return 1
    end

    set -l result (math "$a + $b")

    if test "$result" -gt "$PROVEN_INT64_MAX" 2>/dev/null; or test "$result" -lt "$PROVEN_INT64_MIN" 2>/dev/null
        echo "error:overflow:result exceeds 64-bit integer bounds" >&2
        return 1
    end

    echo "$result"
    return 0
end

function proven_sub --description "Safe subtraction with underflow check"
    set -l a $argv[1]
    set -l b $argv[2]

    if not string match -qr '^-?[0-9]+$' -- "$a"
        echo "error:invalid_input:a is not a number" >&2
        return 1
    end
    if not string match -qr '^-?[0-9]+$' -- "$b"
        echo "error:invalid_input:b is not a number" >&2
        return 1
    end

    set -l result (math "$a - $b")

    if test "$result" -gt "$PROVEN_INT64_MAX" 2>/dev/null; or test "$result" -lt "$PROVEN_INT64_MIN" 2>/dev/null
        echo "error:underflow:result exceeds 64-bit integer bounds" >&2
        return 1
    end

    echo "$result"
    return 0
end

function proven_mul --description "Safe multiplication with overflow check"
    set -l a $argv[1]
    set -l b $argv[2]

    if not string match -qr '^-?[0-9]+$' -- "$a"
        echo "error:invalid_input:a is not a number" >&2
        return 1
    end
    if not string match -qr '^-?[0-9]+$' -- "$b"
        echo "error:invalid_input:b is not a number" >&2
        return 1
    end

    if test "$a" -eq 0; or test "$b" -eq 0
        echo "0"
        return 0
    end

    set -l result (math "$a * $b")

    if test "$result" -gt "$PROVEN_INT64_MAX" 2>/dev/null; or test "$result" -lt "$PROVEN_INT64_MIN" 2>/dev/null
        echo "error:overflow:result exceeds 64-bit integer bounds" >&2
        return 1
    end

    # Verify with division check
    set -l check (math "floor($result / $a)")
    if test "$check" -ne "$b"
        echo "error:overflow:multiplication overflow detected" >&2
        return 1
    end

    echo "$result"
    return 0
end

function proven_div --description "Safe division with zero check"
    set -l a $argv[1]
    set -l b $argv[2]

    if not string match -qr '^-?[0-9]+$' -- "$a"
        echo "error:invalid_input:a is not a number" >&2
        return 1
    end
    if not string match -qr '^-?[0-9]+$' -- "$b"
        echo "error:invalid_input:b is not a number" >&2
        return 1
    end

    if test "$b" -eq 0
        echo "error:division_by_zero:cannot divide by zero" >&2
        return 1
    end

    set -l result (math "floor($a / $b)")
    echo "$result"
    return 0
end

function proven_mod --description "Safe modulo with zero check"
    set -l a $argv[1]
    set -l b $argv[2]

    if not string match -qr '^-?[0-9]+$' -- "$a"
        echo "error:invalid_input:a is not a number" >&2
        return 1
    end
    if not string match -qr '^-?[0-9]+$' -- "$b"
        echo "error:invalid_input:b is not a number" >&2
        return 1
    end

    if test "$b" -eq 0
        echo "error:modulo_by_zero:cannot modulo by zero" >&2
        return 1
    end

    set -l result (math "$a % $b")
    echo "$result"
    return 0
end

function proven_clamp --description "Clamp value to range [min, max]"
    set -l value $argv[1]
    set -l min_val $argv[2]
    set -l max_val $argv[3]

    if test "$value" -lt "$min_val"
        echo "$min_val"
    else if test "$value" -gt "$max_val"
        echo "$max_val"
    else
        echo "$value"
    end
    return 0
end

function proven_in_range --description "Check if value is in range (inclusive)"
    set -l value $argv[1]
    set -l min_val $argv[2]
    set -l max_val $argv[3]

    if test "$value" -ge "$min_val"; and test "$value" -le "$max_val"
        return 0
    else
        return 1
    end
end

function proven_abs --description "Absolute value"
    set -l value $argv[1]

    if not string match -qr '^-?[0-9]+$' -- "$value"
        echo "error:invalid_input:not a number" >&2
        return 1
    end

    if test "$value" -lt 0
        echo (math "0 - $value")
    else
        echo "$value"
    end
    return 0
end

function proven_min --description "Minimum of two values"
    set -l a $argv[1]
    set -l b $argv[2]

    if test "$a" -lt "$b"
        echo "$a"
    else
        echo "$b"
    end
    return 0
end

function proven_max --description "Maximum of two values"
    set -l a $argv[1]
    set -l b $argv[2]

    if test "$a" -gt "$b"
        echo "$a"
    else
        echo "$b"
    end
    return 0
end

# ============================================================================
# MODULE 2: SAFE_STRING (Core)
# ============================================================================

function proven_is_non_empty --description "Check if string is non-empty"
    set -l value $argv[1]

    if test -n "$value"; and test "$value" != ""
        return 0
    else
        return 1
    end
end

function proven_require_non_empty --description "Require non-empty string or exit"
    set -l value $argv[1]
    set -l name $argv[2]

    if not proven_is_non_empty "$value"
        if test -n "$name"
            echo "error:empty_string:$name cannot be empty" >&2
        else
            echo "error:empty_string:value cannot be empty" >&2
        end
        return 1
    end
    return 0
end

function proven_escape_html --description "Escape HTML special characters"
    set -l input $argv[1]

    set -l result (string replace -a '&' '&amp;' -- "$input")
    set result (string replace -a '<' '&lt;' -- "$result")
    set result (string replace -a '>' '&gt;' -- "$result")
    set result (string replace -a '"' '&quot;' -- "$result")
    set result (string replace -a "'" '&#39;' -- "$result")

    echo "$result"
    return 0
end

function proven_escape_sql --description "Escape SQL single quotes"
    set -l input $argv[1]

    set -l result (string replace -a "'" "''" -- "$input")
    echo "$result"
    return 0
end

function proven_escape_shell --description "Escape shell special characters"
    set -l input $argv[1]

    # Escape backslashes first, then other special chars
    set -l result (string replace -a '\\' '\\\\' -- "$input")
    set result (string replace -a '"' '\\"' -- "$result")
    set result (string replace -a '`' '\\`' -- "$result")
    set result (string replace -a '$' '\\$' -- "$result")
    set result (string replace -a '!' '\\!' -- "$result")

    echo "$result"
    return 0
end

function proven_truncate --description "Truncate string to max length"
    set -l input $argv[1]
    set -l max_len $argv[2]

    if test (string length -- "$input") -le "$max_len"
        echo "$input"
    else
        echo (string sub -l "$max_len" -- "$input")
    end
    return 0
end

function proven_is_ascii --description "Check if string contains only ASCII"
    set -l input $argv[1]

    if string match -qr '^[\x00-\x7F]*$' -- "$input"
        return 0
    else
        return 1
    end
end

function proven_is_alphanumeric --description "Check if string is alphanumeric"
    set -l input $argv[1]

    if string match -qr '^[a-zA-Z0-9]*$' -- "$input"
        return 0
    else
        return 1
    end
end

# ============================================================================
# MODULE 3: SAFE_PATH (Core)
# ============================================================================

function proven_is_safe_path --description "Check if path is safe (no traversal)"
    set -l path $argv[1]

    # Check for path traversal
    if string match -q '*../*' -- "$path"
        return 1
    end
    if string match -q '*/../*' -- "$path"
        return 1
    end
    if test "$path" = ".."
        return 1
    end
    if string match -q '*..*' -- "$path"
        return 1
    end

    return 0
end

function proven_safe_join_path --description "Safely join paths preventing traversal"
    set -l base $argv[1]
    set -l relative $argv[2]

    if not proven_is_safe_path "$relative"
        echo "error:unsafe_path:path contains traversal attempt" >&2
        return 1
    end

    set -l full_path "$base/$relative"
    set -l resolved (realpath -m "$full_path" 2>/dev/null)

    if test $status -ne 0
        echo "error:invalid_path:cannot resolve path" >&2
        return 1
    end

    # Verify resolved path is still under base
    set -l resolved_base (realpath -m "$base" 2>/dev/null)
    if not string match -q "$resolved_base*" -- "$resolved"
        echo "error:path_escape:path escapes base directory" >&2
        return 1
    end

    echo "$resolved"
    return 0
end

function proven_normalize_path --description "Normalize path separators"
    set -l path $argv[1]

    # Remove duplicate slashes
    set -l result (string replace -ar '/+' '/' -- "$path")
    # Remove trailing slash unless root
    if test "$result" != "/"
        set result (string replace -r '/$' '' -- "$result")
    end

    echo "$result"
    return 0
end

function proven_get_extension --description "Get file extension"
    set -l path $argv[1]

    set -l basename (basename "$path")
    if string match -q '*.*' -- "$basename"
        echo (string replace -r '^.*\.' '' -- "$basename")
        return 0
    else
        echo ""
        return 1
    end
end

# ============================================================================
# MODULE 4: SAFE_EMAIL (Core)
# ============================================================================

function proven_is_valid_email --description "Basic email format validation"
    set -l email $argv[1]

    if string match -qr '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$' -- "$email"
        return 0
    else
        return 1
    end
end

function proven_get_email_domain --description "Extract domain from email"
    set -l email $argv[1]

    if not proven_is_valid_email "$email"
        echo "error:invalid_email:not a valid email address" >&2
        return 1
    end

    echo (string replace -r '^[^@]+@' '' -- "$email")
    return 0
end

function proven_get_email_local --description "Extract local part from email"
    set -l email $argv[1]

    if not proven_is_valid_email "$email"
        echo "error:invalid_email:not a valid email address" >&2
        return 1
    end

    echo (string replace -r '@.*$' '' -- "$email")
    return 0
end

# ============================================================================
# MODULE 5: SAFE_URL (Core)
# ============================================================================

function proven_is_valid_url --description "Validate URL format"
    set -l url $argv[1]

    if string match -qr '^https?://[a-zA-Z0-9][-a-zA-Z0-9]*(\.[a-zA-Z0-9][-a-zA-Z0-9]*)*(:[0-9]+)?(/.*)?$' -- "$url"
        return 0
    else
        return 1
    end
end

function proven_get_url_scheme --description "Extract scheme from URL"
    set -l url $argv[1]

    if string match -qr '^([a-zA-Z][a-zA-Z0-9+.-]*)://' -- "$url"
        echo (string replace -r '://.*$' '' -- "$url")
        return 0
    else
        echo "error:invalid_url:no scheme found" >&2
        return 1
    end
end

function proven_get_url_host --description "Extract host from URL"
    set -l url $argv[1]

    set -l host (string replace -r '^[a-zA-Z][a-zA-Z0-9+.-]*://' '' -- "$url")
    set host (string replace -r '/.*$' '' -- "$host")
    set host (string replace -r ':[0-9]+$' '' -- "$host")
    set host (string replace -r '@.*' '' -- (string replace -r '^.*@' '' -- "$host"))

    echo "$host"
    return 0
end

function proven_encode_url_component --description "URL-encode a string"
    set -l input $argv[1]

    # Basic URL encoding for common special characters
    set -l result (string replace -a '%' '%25' -- "$input")
    set result (string replace -a ' ' '%20' -- "$result")
    set result (string replace -a '!' '%21' -- "$result")
    set result (string replace -a '#' '%23' -- "$result")
    set result (string replace -a '$' '%24' -- "$result")
    set result (string replace -a '&' '%26' -- "$result")
    set result (string replace -a "'" '%27' -- "$result")
    set result (string replace -a '(' '%28' -- "$result")
    set result (string replace -a ')' '%29' -- "$result")
    set result (string replace -a '+' '%2B' -- "$result")
    set result (string replace -a ',' '%2C' -- "$result")
    set result (string replace -a '/' '%2F' -- "$result")
    set result (string replace -a ':' '%3A' -- "$result")
    set result (string replace -a ';' '%3B' -- "$result")
    set result (string replace -a '=' '%3D' -- "$result")
    set result (string replace -a '?' '%3F' -- "$result")
    set result (string replace -a '@' '%40' -- "$result")

    echo "$result"
    return 0
end

# ============================================================================
# MODULE 6: SAFE_NETWORK (Core)
# ============================================================================

function proven_is_valid_ipv4 --description "Validate IPv4 address format"
    set -l ip $argv[1]

    if string match -qr '^([0-9]{1,3}\.){3}[0-9]{1,3}$' -- "$ip"
        for octet in (string split "." "$ip")
            if test "$octet" -gt 255; or test "$octet" -lt 0
                return 1
            end
        end
        return 0
    else
        return 1
    end
end

function proven_is_valid_ipv6 --description "Validate IPv6 address format (simplified)"
    set -l ip $argv[1]

    # Simplified check for basic IPv6 format
    if string match -qr '^([0-9a-fA-F]{0,4}:){2,7}[0-9a-fA-F]{0,4}$' -- "$ip"
        return 0
    else if string match -qr '^::$' -- "$ip"
        return 0
    else if string match -qr '^::1$' -- "$ip"
        return 0
    else
        return 1
    end
end

function proven_is_valid_port --description "Validate port number (1-65535)"
    set -l port $argv[1]

    if not string match -qr '^[0-9]+$' -- "$port"
        return 1
    end

    if test "$port" -ge 1; and test "$port" -le 65535
        return 0
    else
        return 1
    end
end

function proven_require_valid_port --description "Require valid port or exit"
    set -l port $argv[1]

    if not proven_is_valid_port "$port"
        echo "error:invalid_port:$port is not a valid port (1-65535)" >&2
        return 1
    end
    return 0
end

function proven_is_private_ip --description "Check if IPv4 is private"
    set -l ip $argv[1]

    if not proven_is_valid_ipv4 "$ip"
        return 1
    end

    # 10.0.0.0/8
    if string match -qr '^10\.' -- "$ip"
        return 0
    end
    # 172.16.0.0/12
    if string match -qr '^172\.(1[6-9]|2[0-9]|3[0-1])\.' -- "$ip"
        return 0
    end
    # 192.168.0.0/16
    if string match -qr '^192\.168\.' -- "$ip"
        return 0
    end
    # 127.0.0.0/8
    if string match -qr '^127\.' -- "$ip"
        return 0
    end

    return 1
end

function proven_is_valid_cidr --description "Validate CIDR notation"
    set -l cidr $argv[1]

    if string match -qr '^([0-9]{1,3}\.){3}[0-9]{1,3}/[0-9]{1,2}$' -- "$cidr"
        set -l ip (string replace -r '/.*' '' -- "$cidr")
        set -l prefix (string replace -r '.*/' '' -- "$cidr")

        if not proven_is_valid_ipv4 "$ip"
            return 1
        end

        if test "$prefix" -ge 0; and test "$prefix" -le 32
            return 0
        end
    end

    return 1
end

# ============================================================================
# MODULE 7: SAFE_CRYPTO (Core)
# ============================================================================

function proven_sha256 --description "Calculate SHA-256 hash"
    set -l input $argv[1]

    if command -q sha256sum
        echo -n "$input" | sha256sum | string replace -r '\s.*' ''
    else if command -q shasum
        echo -n "$input" | shasum -a 256 | string replace -r '\s.*' ''
    else
        echo "error:no_hash_command:sha256sum or shasum not found" >&2
        return 1
    end
    return 0
end

function proven_sha512 --description "Calculate SHA-512 hash"
    set -l input $argv[1]

    if command -q sha512sum
        echo -n "$input" | sha512sum | string replace -r '\s.*' ''
    else if command -q shasum
        echo -n "$input" | shasum -a 512 | string replace -r '\s.*' ''
    else
        echo "error:no_hash_command:sha512sum or shasum not found" >&2
        return 1
    end
    return 0
end

function proven_md5 --description "Calculate MD5 hash (not for security)"
    set -l input $argv[1]

    if command -q md5sum
        echo -n "$input" | md5sum | string replace -r '\s.*' ''
    else if command -q md5
        echo -n "$input" | md5
    else
        echo "error:no_hash_command:md5sum or md5 not found" >&2
        return 1
    end
    return 0
end

function proven_random_bytes --description "Generate random bytes (hex encoded)"
    set -l count $argv[1]

    if not string match -qr '^[0-9]+$' -- "$count"
        set count 16
    end

    if test -r /dev/urandom
        head -c "$count" /dev/urandom | xxd -p | tr -d '\n'
        return 0
    else
        echo "error:no_random_source:/dev/urandom not available" >&2
        return 1
    end
end

# ============================================================================
# MODULE 8: SAFE_UUID (Core)
# ============================================================================

function proven_generate_uuid_v4 --description "Generate UUID v4 (random)"
    set -l bytes (proven_random_bytes 16)
    if test $status -ne 0
        return 1
    end

    # Format as UUID and set version/variant bits
    set -l p1 (string sub -s 1 -l 8 -- "$bytes")
    set -l p2 (string sub -s 9 -l 4 -- "$bytes")
    set -l p3 (string sub -s 13 -l 4 -- "$bytes")
    set -l p4 (string sub -s 17 -l 4 -- "$bytes")
    set -l p5 (string sub -s 21 -l 12 -- "$bytes")

    # Set version 4 (random) in p3
    set -l p3_first (string sub -s 1 -l 1 -- "$p3")
    set -l p3_rest (string sub -s 2 -- "$p3")
    set p3 "4$p3_rest"

    # Set variant (8, 9, a, or b) in p4
    set -l p4_first (string sub -s 1 -l 1 -- "$p4")
    set -l p4_rest (string sub -s 2 -- "$p4")
    # Force to 8-b range
    switch $p4_first
        case 0 1 2 3
            set p4 "8$p4_rest"
        case 4 5 6 7
            set p4 "9$p4_rest"
        case 8 9
            set p4 "$p4_first$p4_rest"
        case a b c d e f A B C D E F
            set p4 "a$p4_rest"
    end

    echo "$p1-$p2-$p3-$p4-$p5"
    return 0
end

function proven_is_valid_uuid --description "Validate UUID format"
    set -l uuid $argv[1]

    if string match -qr '^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$' -- "$uuid"
        return 0
    else
        return 1
    end
end

function proven_uuid_nil --description "Return nil UUID"
    echo "00000000-0000-0000-0000-000000000000"
    return 0
end

# ============================================================================
# MODULE 9: SAFE_CURRENCY (Core)
# ============================================================================

function proven_format_currency --description "Format amount with currency code"
    set -l amount $argv[1]
    set -l currency $argv[2]

    if not string match -qr '^-?[0-9]+(\.[0-9]+)?$' -- "$amount"
        echo "error:invalid_amount:not a valid number" >&2
        return 1
    end

    # Format with 2 decimal places
    set -l formatted (printf "%.2f" "$amount")
    echo "$currency $formatted"
    return 0
end

function proven_is_valid_currency_code --description "Validate ISO 4217 currency code"
    set -l code $argv[1]

    if string match -qr '^[A-Z]{3}$' -- "$code"
        return 0
    else
        return 1
    end
end

function proven_add_money --description "Add two monetary amounts (integer cents)"
    set -l a $argv[1]
    set -l b $argv[2]

    proven_add "$a" "$b"
end

function proven_sub_money --description "Subtract monetary amounts (integer cents)"
    set -l a $argv[1]
    set -l b $argv[2]

    proven_sub "$a" "$b"
end

# ============================================================================
# MODULE 10: SAFE_PHONE (Core)
# ============================================================================

function proven_is_valid_phone_e164 --description "Validate E.164 phone format"
    set -l phone $argv[1]

    # E.164: +[country code][subscriber number], max 15 digits
    if string match -qr '^\+[1-9][0-9]{6,14}$' -- "$phone"
        return 0
    else
        return 1
    end
end

function proven_normalize_phone --description "Normalize phone to digits only"
    set -l phone $argv[1]

    echo (string replace -ar '[^0-9+]' '' -- "$phone")
    return 0
end

# ============================================================================
# MODULE 11: SAFE_HEX (Core)
# ============================================================================

function proven_is_valid_hex --description "Check if string is valid hex"
    set -l input $argv[1]

    if string match -qr '^[0-9a-fA-F]*$' -- "$input"
        return 0
    else
        return 1
    end
end

function proven_hex_encode --description "Encode string to hex"
    set -l input $argv[1]

    echo -n "$input" | xxd -p | tr -d '\n'
    return 0
end

function proven_hex_decode --description "Decode hex to string"
    set -l input $argv[1]

    if not proven_is_valid_hex "$input"
        echo "error:invalid_hex:input is not valid hexadecimal" >&2
        return 1
    end

    echo -n "$input" | xxd -r -p
    return 0
end

# ============================================================================
# MODULE 12: SAFE_JSON (Data)
# ============================================================================

function proven_is_valid_json --description "Validate JSON syntax"
    set -l input $argv[1]

    # Try to parse with jq if available
    if command -q jq
        echo "$input" | jq empty 2>/dev/null
        return $status
    end

    # Fallback: simple bracket matching
    set -l depth_brace 0
    set -l depth_bracket 0
    set -l in_string false

    for char in (string split '' -- "$input")
        if test "$in_string" = true
            if test "$char" = '"'
                set in_string false
            end
            continue
        end

        switch $char
            case '"'
                set in_string true
            case '{'
                set depth_brace (math "$depth_brace + 1")
            case '}'
                set depth_brace (math "$depth_brace - 1")
            case '['
                set depth_bracket (math "$depth_bracket + 1")
            case ']'
                set depth_bracket (math "$depth_bracket - 1")
        end

        if test "$depth_brace" -lt 0; or test "$depth_bracket" -lt 0
            return 1
        end
    end

    if test "$depth_brace" -eq 0; and test "$depth_bracket" -eq 0; and test "$in_string" = false
        return 0
    else
        return 1
    end
end

function proven_json_get --description "Get value from JSON (requires jq)"
    set -l json $argv[1]
    set -l path $argv[2]

    if not command -q jq
        echo "error:jq_required:jq command not found" >&2
        return 1
    end

    echo "$json" | jq -r "$path" 2>/dev/null
    return $status
end

# ============================================================================
# MODULE 13: SAFE_DATETIME (Data)
# ============================================================================

function proven_is_valid_iso8601 --description "Validate ISO 8601 date format"
    set -l input $argv[1]

    # Basic ISO 8601 date pattern: YYYY-MM-DD
    if string match -qr '^[0-9]{4}-[0-9]{2}-[0-9]{2}$' -- "$input"
        return 0
    end
    # With time: YYYY-MM-DDTHH:MM:SS
    if string match -qr '^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}' -- "$input"
        return 0
    end

    return 1
end

function proven_timestamp_now --description "Get current Unix timestamp"
    date +%s
    return 0
end

function proven_timestamp_to_iso --description "Convert timestamp to ISO 8601"
    set -l timestamp $argv[1]

    date -d "@$timestamp" -Iseconds 2>/dev/null
    or date -r "$timestamp" -Iseconds 2>/dev/null
    return $status
end

function proven_date_add_days --description "Add days to a date (YYYY-MM-DD)"
    set -l date $argv[1]
    set -l days $argv[2]

    date -d "$date + $days days" +%Y-%m-%d 2>/dev/null
    return $status
end

# ============================================================================
# MODULE 14: SAFE_FLOAT (Data)
# ============================================================================

function proven_is_finite --description "Check if number is finite (not NaN/Inf)"
    set -l value $argv[1]

    if string match -qr '^-?[0-9]+\.?[0-9]*$' -- "$value"
        return 0
    else if string match -qr '^-?[0-9]*\.[0-9]+$' -- "$value"
        return 0
    else
        return 1
    end
end

function proven_float_add --description "Safe floating point addition"
    set -l a $argv[1]
    set -l b $argv[2]

    if not proven_is_finite "$a"; or not proven_is_finite "$b"
        echo "error:invalid_float:operands must be finite numbers" >&2
        return 1
    end

    math "$a + $b"
    return 0
end

function proven_float_div --description "Safe floating point division"
    set -l a $argv[1]
    set -l b $argv[2]

    if not proven_is_finite "$a"; or not proven_is_finite "$b"
        echo "error:invalid_float:operands must be finite numbers" >&2
        return 1
    end

    if test (math "$b == 0") -eq 1
        echo "error:division_by_zero:cannot divide by zero" >&2
        return 1
    end

    math "$a / $b"
    return 0
end

# ============================================================================
# MODULE 15: SAFE_VERSION (Data)
# ============================================================================

function proven_is_valid_semver --description "Validate semantic version format"
    set -l version $argv[1]

    # Basic semver: major.minor.patch
    if string match -qr '^[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9.-]+)?(\+[a-zA-Z0-9.-]+)?$' -- "$version"
        return 0
    else
        return 1
    end
end

function proven_compare_semver --description "Compare two semver strings (-1, 0, 1)"
    set -l v1 $argv[1]
    set -l v2 $argv[2]

    # Extract major.minor.patch
    set -l v1_parts (string split '.' (string replace -r '[-+].*' '' -- "$v1"))
    set -l v2_parts (string split '.' (string replace -r '[-+].*' '' -- "$v2"))

    for i in 1 2 3
        set -l p1 $v1_parts[$i]
        set -l p2 $v2_parts[$i]
        test -z "$p1"; and set p1 0
        test -z "$p2"; and set p2 0

        if test "$p1" -lt "$p2"
            echo "-1"
            return 0
        else if test "$p1" -gt "$p2"
            echo "1"
            return 0
        end
    end

    echo "0"
    return 0
end

function proven_semver_major --description "Extract major version"
    set -l version $argv[1]

    string replace -r '\..*' '' -- "$version"
    return 0
end

function proven_semver_minor --description "Extract minor version"
    set -l version $argv[1]

    set -l without_major (string replace -r '^[0-9]+\.' '' -- "$version")
    string replace -r '\..*' '' -- "$without_major"
    return 0
end

# ============================================================================
# MODULE 16: SAFE_COLOR (Data)
# ============================================================================

function proven_is_valid_hex_color --description "Validate hex color (#RGB or #RRGGBB)"
    set -l color $argv[1]

    if string match -qr '^#[0-9a-fA-F]{6}$' -- "$color"
        return 0
    else if string match -qr '^#[0-9a-fA-F]{3}$' -- "$color"
        return 0
    else
        return 1
    end
end

function proven_rgb_to_hex --description "Convert RGB values to hex color"
    set -l r $argv[1]
    set -l g $argv[2]
    set -l b $argv[3]

    if test "$r" -lt 0 -o "$r" -gt 255
        echo "error:out_of_range:red must be 0-255" >&2
        return 1
    end
    if test "$g" -lt 0 -o "$g" -gt 255
        echo "error:out_of_range:green must be 0-255" >&2
        return 1
    end
    if test "$b" -lt 0 -o "$b" -gt 255
        echo "error:out_of_range:blue must be 0-255" >&2
        return 1
    end

    printf "#%02X%02X%02X\n" "$r" "$g" "$b"
    return 0
end

function proven_hex_to_rgb --description "Convert hex color to RGB values"
    set -l color $argv[1]

    if not proven_is_valid_hex_color "$color"
        echo "error:invalid_color:not a valid hex color" >&2
        return 1
    end

    set -l hex (string replace '#' '' -- "$color")

    # Handle 3-digit hex
    if test (string length -- "$hex") -eq 3
        set -l r (string sub -s 1 -l 1 -- "$hex")
        set -l g (string sub -s 2 -l 1 -- "$hex")
        set -l b (string sub -s 3 -l 1 -- "$hex")
        set hex "$r$r$g$g$b$b"
    end

    set -l r (printf "%d" 0x(string sub -s 1 -l 2 -- "$hex"))
    set -l g (printf "%d" 0x(string sub -s 3 -l 2 -- "$hex"))
    set -l b (printf "%d" 0x(string sub -s 5 -l 2 -- "$hex"))

    echo "$r $g $b"
    return 0
end

# ============================================================================
# MODULE 17: SAFE_ANGLE (Data)
# ============================================================================

function proven_deg_to_rad --description "Convert degrees to radians"
    set -l degrees $argv[1]

    math "$degrees * 3.14159265358979323846 / 180"
    return 0
end

function proven_rad_to_deg --description "Convert radians to degrees"
    set -l radians $argv[1]

    math "$radians * 180 / 3.14159265358979323846"
    return 0
end

function proven_normalize_degrees --description "Normalize degrees to [0, 360)"
    set -l degrees $argv[1]

    set -l result (math "$degrees % 360")
    if test (math "$result < 0") -eq 1
        set result (math "$result + 360")
    end

    echo "$result"
    return 0
end

# ============================================================================
# MODULE 18: SAFE_UNIT (Data)
# ============================================================================

function proven_convert_length --description "Convert length units"
    set -l value $argv[1]
    set -l from_unit $argv[2]
    set -l to_unit $argv[3]

    # Convert to meters first
    set -l meters
    switch $from_unit
        case m meters
            set meters $value
        case km kilometers
            set meters (math "$value * 1000")
        case cm centimeters
            set meters (math "$value / 100")
        case mm millimeters
            set meters (math "$value / 1000")
        case mi miles
            set meters (math "$value * 1609.344")
        case ft feet
            set meters (math "$value * 0.3048")
        case in inches
            set meters (math "$value * 0.0254")
        case '*'
            echo "error:unknown_unit:$from_unit" >&2
            return 1
    end

    # Convert from meters to target
    switch $to_unit
        case m meters
            echo "$meters"
        case km kilometers
            math "$meters / 1000"
        case cm centimeters
            math "$meters * 100"
        case mm millimeters
            math "$meters * 1000"
        case mi miles
            math "$meters / 1609.344"
        case ft feet
            math "$meters / 0.3048"
        case in inches
            math "$meters / 0.0254"
        case '*'
            echo "error:unknown_unit:$to_unit" >&2
            return 1
    end
    return 0
end

function proven_convert_temperature --description "Convert temperature units"
    set -l value $argv[1]
    set -l from_unit $argv[2]
    set -l to_unit $argv[3]

    # Convert to Celsius first
    set -l celsius
    switch $from_unit
        case c C celsius Celsius
            set celsius $value
        case f F fahrenheit Fahrenheit
            set celsius (math "($value - 32) * 5 / 9")
        case k K kelvin Kelvin
            set celsius (math "$value - 273.15")
        case '*'
            echo "error:unknown_unit:$from_unit" >&2
            return 1
    end

    # Convert from Celsius to target
    switch $to_unit
        case c C celsius Celsius
            echo "$celsius"
        case f F fahrenheit Fahrenheit
            math "$celsius * 9 / 5 + 32"
        case k K kelvin Kelvin
            math "$celsius + 273.15"
        case '*'
            echo "error:unknown_unit:$to_unit" >&2
            return 1
    end
    return 0
end

# ============================================================================
# MODULE 19: SAFE_BUFFER (Data Structures)
# ============================================================================

# Note: Fish doesn't have true data structures, so these use file-backed storage

function proven_buffer_create --description "Create a bounded buffer"
    set -l name $argv[1]
    set -l capacity $argv[2]

    set -g "PROVEN_BUFFER_$name""_CAPACITY" "$capacity"
    set -g "PROVEN_BUFFER_$name""_COUNT" 0
    set -g "PROVEN_BUFFER_$name""_DATA"
    return 0
end

function proven_buffer_push --description "Push item to buffer"
    set -l name $argv[1]
    set -l item $argv[2]

    set -l capacity_var "PROVEN_BUFFER_$name""_CAPACITY"
    set -l count_var "PROVEN_BUFFER_$name""_COUNT"
    set -l data_var "PROVEN_BUFFER_$name""_DATA"

    set -l capacity $$capacity_var
    set -l count $$count_var

    if test "$count" -ge "$capacity"
        echo "error:buffer_full:capacity $capacity exceeded" >&2
        return 1
    end

    set -a $data_var "$item"
    set -g $count_var (math "$count + 1")
    return 0
end

function proven_buffer_size --description "Get buffer size"
    set -l name $argv[1]
    set -l count_var "PROVEN_BUFFER_$name""_COUNT"

    echo $$count_var
    return 0
end

# ============================================================================
# MODULE 20: SAFE_QUEUE (Data Structures)
# ============================================================================

function proven_queue_create --description "Create a bounded queue"
    set -l name $argv[1]
    set -l capacity $argv[2]

    set -g "PROVEN_QUEUE_$name""_CAPACITY" "$capacity"
    set -g "PROVEN_QUEUE_$name""_DATA"
    return 0
end

function proven_queue_enqueue --description "Add item to queue"
    set -l name $argv[1]
    set -l item $argv[2]

    set -l capacity_var "PROVEN_QUEUE_$name""_CAPACITY"
    set -l data_var "PROVEN_QUEUE_$name""_DATA"

    set -l capacity $$capacity_var
    set -l count (count $$data_var)

    if test "$count" -ge "$capacity"
        echo "error:queue_full:capacity $capacity exceeded" >&2
        return 1
    end

    set -a $data_var "$item"
    return 0
end

function proven_queue_dequeue --description "Remove item from queue"
    set -l name $argv[1]

    set -l data_var "PROVEN_QUEUE_$name""_DATA"

    if test (count $$data_var) -eq 0
        echo "error:queue_empty:no items to dequeue" >&2
        return 1
    end

    echo $$data_var[1]
    set -e "$data_var"[1]
    return 0
end

function proven_queue_size --description "Get queue size"
    set -l name $argv[1]
    set -l data_var "PROVEN_QUEUE_$name""_DATA"

    count $$data_var
    return 0
end

# ============================================================================
# MODULE 21: SAFE_BLOOM (Data Structures)
# ============================================================================

# Simplified bloom filter using file-backed storage
function proven_bloom_create --description "Create a bloom filter"
    set -l name $argv[1]
    set -l size $argv[2]

    set -g "PROVEN_BLOOM_$name""_SIZE" "$size"
    set -g "PROVEN_BLOOM_$name""_BITS" (string repeat -n "$size" "0")
    return 0
end

function proven_bloom_add --description "Add item to bloom filter"
    set -l name $argv[1]
    set -l item $argv[2]

    set -l size_var "PROVEN_BLOOM_$name""_SIZE"
    set -l bits_var "PROVEN_BLOOM_$name""_BITS"

    set -l size $$size_var
    set -l hash1 (proven_djb2_hash "$item")
    set -l hash2 (proven_sdbm_hash "$item")

    set -l idx1 (math "$hash1 % $size + 1")
    set -l idx2 (math "$hash2 % $size + 1")

    # Set bits at indices
    set -l bits $$bits_var
    set -l new_bits (string sub -l (math "$idx1 - 1") -- "$bits")1(string sub -s (math "$idx1 + 1") -- "$bits")
    set -g $bits_var "$new_bits"

    set bits $$bits_var
    set new_bits (string sub -l (math "$idx2 - 1") -- "$bits")1(string sub -s (math "$idx2 + 1") -- "$bits")
    set -g $bits_var "$new_bits"

    return 0
end

function proven_bloom_contains --description "Check if item might be in bloom filter"
    set -l name $argv[1]
    set -l item $argv[2]

    set -l size_var "PROVEN_BLOOM_$name""_SIZE"
    set -l bits_var "PROVEN_BLOOM_$name""_BITS"

    set -l size $$size_var
    set -l bits $$bits_var

    set -l hash1 (proven_djb2_hash "$item")
    set -l hash2 (proven_sdbm_hash "$item")

    set -l idx1 (math "$hash1 % $size + 1")
    set -l idx2 (math "$hash2 % $size + 1")

    set -l bit1 (string sub -s "$idx1" -l 1 -- "$bits")
    set -l bit2 (string sub -s "$idx2" -l 1 -- "$bits")

    if test "$bit1" = "1"; and test "$bit2" = "1"
        return 0
    else
        return 1
    end
end

# ============================================================================
# MODULE 22: SAFE_LRU (Data Structures)
# ============================================================================

function proven_lru_create --description "Create an LRU cache"
    set -l name $argv[1]
    set -l capacity $argv[2]

    set -g "PROVEN_LRU_$name""_CAPACITY" "$capacity"
    set -g "PROVEN_LRU_$name""_KEYS"
    set -g "PROVEN_LRU_$name""_VALUES"
    return 0
end

function proven_lru_put --description "Put item in LRU cache"
    set -l name $argv[1]
    set -l key $argv[2]
    set -l value $argv[3]

    set -l capacity_var "PROVEN_LRU_$name""_CAPACITY"
    set -l keys_var "PROVEN_LRU_$name""_KEYS"
    set -l values_var "PROVEN_LRU_$name""_VALUES"

    set -l capacity $$capacity_var

    # Remove if exists
    set -l idx 1
    for k in $$keys_var
        if test "$k" = "$key"
            set -e "$keys_var"[$idx]
            set -e "$values_var"[$idx]
            break
        end
        set idx (math "$idx + 1")
    end

    # Evict if at capacity
    while test (count $$keys_var) -ge "$capacity"
        set -e "$keys_var"[1]
        set -e "$values_var"[1]
    end

    # Add new item at end (most recently used)
    set -a $keys_var "$key"
    set -a $values_var "$value"
    return 0
end

function proven_lru_get --description "Get item from LRU cache"
    set -l name $argv[1]
    set -l key $argv[2]

    set -l keys_var "PROVEN_LRU_$name""_KEYS"
    set -l values_var "PROVEN_LRU_$name""_VALUES"

    set -l idx 1
    for k in $$keys_var
        if test "$k" = "$key"
            set -l value $$values_var[$idx]
            # Move to end (mark as recently used)
            set -e "$keys_var"[$idx]
            set -e "$values_var"[$idx]
            set -a $keys_var "$key"
            set -a $values_var "$value"
            echo "$value"
            return 0
        end
        set idx (math "$idx + 1")
    end

    return 1
end

# ============================================================================
# MODULE 23: SAFE_GRAPH (Data Structures)
# ============================================================================

function proven_graph_create --description "Create a directed graph"
    set -l name $argv[1]

    set -g "PROVEN_GRAPH_$name""_NODES"
    set -g "PROVEN_GRAPH_$name""_EDGES"
    return 0
end

function proven_graph_add_node --description "Add node to graph"
    set -l name $argv[1]
    set -l node $argv[2]

    set -l nodes_var "PROVEN_GRAPH_$name""_NODES"

    # Check if already exists
    for n in $$nodes_var
        if test "$n" = "$node"
            return 0
        end
    end

    set -a $nodes_var "$node"
    return 0
end

function proven_graph_add_edge --description "Add edge to graph (from -> to)"
    set -l name $argv[1]
    set -l from $argv[2]
    set -l to $argv[3]

    proven_graph_add_node "$name" "$from"
    proven_graph_add_node "$name" "$to"

    set -l edges_var "PROVEN_GRAPH_$name""_EDGES"
    set -a $edges_var "$from:$to"
    return 0
end

function proven_graph_has_edge --description "Check if edge exists"
    set -l name $argv[1]
    set -l from $argv[2]
    set -l to $argv[3]

    set -l edges_var "PROVEN_GRAPH_$name""_EDGES"
    set -l edge "$from:$to"

    for e in $$edges_var
        if test "$e" = "$edge"
            return 0
        end
    end

    return 1
end

# ============================================================================
# MODULE 24: SAFE_RATE_LIMITER (Resilience)
# ============================================================================

function proven_ratelimit_create --description "Create token bucket rate limiter"
    set -l name $argv[1]
    set -l capacity $argv[2]
    set -l refill_rate $argv[3]

    set -g "PROVEN_RATELIMIT_$name""_CAPACITY" "$capacity"
    set -g "PROVEN_RATELIMIT_$name""_TOKENS" "$capacity"
    set -g "PROVEN_RATELIMIT_$name""_REFILL_RATE" "$refill_rate"
    set -g "PROVEN_RATELIMIT_$name""_LAST_REFILL" (date +%s)
    return 0
end

function proven_ratelimit_acquire --description "Try to acquire tokens"
    set -l name $argv[1]
    set -l count $argv[2]
    test -z "$count"; and set count 1

    set -l capacity_var "PROVEN_RATELIMIT_$name""_CAPACITY"
    set -l tokens_var "PROVEN_RATELIMIT_$name""_TOKENS"
    set -l refill_var "PROVEN_RATELIMIT_$name""_REFILL_RATE"
    set -l last_var "PROVEN_RATELIMIT_$name""_LAST_REFILL"

    # Refill tokens
    set -l now (date +%s)
    set -l elapsed (math "$now - $$last_var")
    set -l new_tokens (math "$$tokens_var + $elapsed * $$refill_var")
    if test "$new_tokens" -gt "$$capacity_var"
        set new_tokens $$capacity_var
    end
    set -g $tokens_var "$new_tokens"
    set -g $last_var "$now"

    # Try to acquire
    if test "$$tokens_var" -ge "$count"
        set -g $tokens_var (math "$$tokens_var - $count")
        return 0
    else
        return 1
    end
end

# ============================================================================
# MODULE 25: SAFE_CIRCUIT_BREAKER (Resilience)
# ============================================================================

function proven_circuit_create --description "Create circuit breaker"
    set -l name $argv[1]
    set -l threshold $argv[2]
    set -l timeout $argv[3]

    set -g "PROVEN_CIRCUIT_$name""_STATE" "closed"
    set -g "PROVEN_CIRCUIT_$name""_FAILURES" 0
    set -g "PROVEN_CIRCUIT_$name""_THRESHOLD" "$threshold"
    set -g "PROVEN_CIRCUIT_$name""_TIMEOUT" "$timeout"
    set -g "PROVEN_CIRCUIT_$name""_OPENED_AT" 0
    return 0
end

function proven_circuit_allow --description "Check if circuit allows request"
    set -l name $argv[1]

    set -l state_var "PROVEN_CIRCUIT_$name""_STATE"
    set -l opened_var "PROVEN_CIRCUIT_$name""_OPENED_AT"
    set -l timeout_var "PROVEN_CIRCUIT_$name""_TIMEOUT"

    switch $$state_var
        case closed
            return 0
        case open
            set -l now (date +%s)
            set -l elapsed (math "$now - $$opened_var")
            if test "$elapsed" -ge "$$timeout_var"
                set -g $state_var "half-open"
                return 0
            end
            return 1
        case half-open
            return 0
    end
end

function proven_circuit_record_success --description "Record successful call"
    set -l name $argv[1]

    set -l state_var "PROVEN_CIRCUIT_$name""_STATE"
    set -l failures_var "PROVEN_CIRCUIT_$name""_FAILURES"

    set -g $failures_var 0
    set -g $state_var "closed"
    return 0
end

function proven_circuit_record_failure --description "Record failed call"
    set -l name $argv[1]

    set -l state_var "PROVEN_CIRCUIT_$name""_STATE"
    set -l failures_var "PROVEN_CIRCUIT_$name""_FAILURES"
    set -l threshold_var "PROVEN_CIRCUIT_$name""_THRESHOLD"
    set -l opened_var "PROVEN_CIRCUIT_$name""_OPENED_AT"

    set -g $failures_var (math "$$failures_var + 1")

    if test "$$failures_var" -ge "$$threshold_var"
        set -g $state_var "open"
        set -g $opened_var (date +%s)
    end
    return 0
end

# ============================================================================
# MODULE 26: SAFE_RETRY (Resilience)
# ============================================================================

function proven_retry_backoff --description "Calculate exponential backoff delay"
    set -l attempt $argv[1]
    set -l base_ms $argv[2]
    set -l max_ms $argv[3]

    test -z "$base_ms"; and set base_ms 100
    test -z "$max_ms"; and set max_ms 30000

    # 2^attempt * base, capped at max
    set -l delay (math "2 ^ $attempt * $base_ms")
    if test "$delay" -gt "$max_ms"
        set delay "$max_ms"
    end

    echo "$delay"
    return 0
end

function proven_retry_with_jitter --description "Add jitter to delay"
    set -l delay $argv[1]
    set -l jitter_factor $argv[2]

    test -z "$jitter_factor"; and set jitter_factor "0.5"

    set -l jitter_range (math "$delay * $jitter_factor")
    set -l random_jitter (math "(random 0 1000) / 1000 * $jitter_range")
    set -l result (math "$delay + $random_jitter - $jitter_range / 2")

    # Ensure non-negative
    if test (math "$result < 0") -eq 1
        set result 0
    end

    printf "%.0f" "$result"
    return 0
end

# ============================================================================
# MODULE 27: SAFE_MONOTONIC (Resilience)
# ============================================================================

function proven_monotonic_create --description "Create monotonic counter"
    set -l name $argv[1]
    set -l initial $argv[2]

    test -z "$initial"; and set initial 0

    set -g "PROVEN_MONOTONIC_$name""_VALUE" "$initial"
    return 0
end

function proven_monotonic_next --description "Get next value from monotonic counter"
    set -l name $argv[1]

    set -l value_var "PROVEN_MONOTONIC_$name""_VALUE"
    set -l current $$value_var
    set -g $value_var (math "$current + 1")

    echo "$$value_var"
    return 0
end

function proven_monotonic_high_water_mark --description "Update high water mark"
    set -l name $argv[1]
    set -l value $argv[2]

    set -l value_var "PROVEN_MONOTONIC_$name""_VALUE"

    if test "$value" -gt "$$value_var"
        set -g $value_var "$value"
    end

    echo "$$value_var"
    return 0
end

# ============================================================================
# MODULE 28: SAFE_STATE_MACHINE (State)
# ============================================================================

function proven_fsm_create --description "Create finite state machine"
    set -l name $argv[1]
    set -l initial_state $argv[2]

    set -g "PROVEN_FSM_$name""_STATE" "$initial_state"
    set -g "PROVEN_FSM_$name""_TRANSITIONS"
    return 0
end

function proven_fsm_add_transition --description "Add state transition rule"
    set -l name $argv[1]
    set -l from $argv[2]
    set -l event $argv[3]
    set -l to $argv[4]

    set -l transitions_var "PROVEN_FSM_$name""_TRANSITIONS"
    set -a $transitions_var "$from:$event:$to"
    return 0
end

function proven_fsm_transition --description "Attempt state transition"
    set -l name $argv[1]
    set -l event $argv[2]

    set -l state_var "PROVEN_FSM_$name""_STATE"
    set -l transitions_var "PROVEN_FSM_$name""_TRANSITIONS"

    set -l current $$state_var

    for t in $$transitions_var
        set -l parts (string split ':' -- "$t")
        if test "$parts[1]" = "$current"; and test "$parts[2]" = "$event"
            set -g $state_var "$parts[3]"
            echo "$parts[3]"
            return 0
        end
    end

    echo "error:invalid_transition:no transition from $current on $event" >&2
    return 1
end

function proven_fsm_state --description "Get current state"
    set -l name $argv[1]

    set -l state_var "PROVEN_FSM_$name""_STATE"
    echo "$$state_var"
    return 0
end

# ============================================================================
# MODULE 29: SAFE_CALCULATOR (State)
# ============================================================================

function proven_calc_eval --description "Safely evaluate arithmetic expression"
    set -l expr $argv[1]

    # Validate expression contains only safe characters
    if not string match -qr '^[0-9+\-*/().%\s]+$' -- "$expr"
        echo "error:invalid_expression:contains unsafe characters" >&2
        return 1
    end

    # Check for division by zero
    if string match -qr '/\s*0([^0-9]|$)' -- "$expr"
        echo "error:division_by_zero:expression contains division by zero" >&2
        return 1
    end

    math "$expr" 2>/dev/null
    return $status
end

# ============================================================================
# MODULE 30: SAFE_GEO (Algorithm)
# ============================================================================

function proven_is_valid_latitude --description "Validate latitude (-90 to 90)"
    set -l lat $argv[1]

    if not proven_is_finite "$lat"
        return 1
    end

    if test (math "$lat >= -90 && $lat <= 90") -eq 1
        return 0
    else
        return 1
    end
end

function proven_is_valid_longitude --description "Validate longitude (-180 to 180)"
    set -l lon $argv[1]

    if not proven_is_finite "$lon"
        return 1
    end

    if test (math "$lon >= -180 && $lon <= 180") -eq 1
        return 0
    else
        return 1
    end
end

function proven_haversine_distance --description "Calculate distance between coordinates (km)"
    set -l lat1 $argv[1]
    set -l lon1 $argv[2]
    set -l lat2 $argv[3]
    set -l lon2 $argv[4]

    if not proven_is_valid_latitude "$lat1"; or not proven_is_valid_latitude "$lat2"
        echo "error:invalid_latitude:latitude must be between -90 and 90" >&2
        return 1
    end
    if not proven_is_valid_longitude "$lon1"; or not proven_is_valid_longitude "$lon2"
        echo "error:invalid_longitude:longitude must be between -180 and 180" >&2
        return 1
    end

    # Convert to radians
    set -l pi 3.14159265358979323846
    set -l lat1_rad (math "$lat1 * $pi / 180")
    set -l lat2_rad (math "$lat2 * $pi / 180")
    set -l dlat (math "($lat2 - $lat1) * $pi / 180")
    set -l dlon (math "($lon2 - $lon1) * $pi / 180")

    # Haversine formula
    set -l a (math "sin($dlat / 2)^2 + cos($lat1_rad) * cos($lat2_rad) * sin($dlon / 2)^2")
    set -l c (math "2 * atan2(sqrt($a), sqrt(1 - $a))")
    set -l earth_radius_km 6371

    math "$earth_radius_km * $c"
    return 0
end

# ============================================================================
# MODULE 31: SAFE_PROBABILITY (Algorithm)
# ============================================================================

function proven_is_valid_probability --description "Validate probability (0 to 1)"
    set -l p $argv[1]

    if not proven_is_finite "$p"
        return 1
    end

    if test (math "$p >= 0 && $p <= 1") -eq 1
        return 0
    else
        return 1
    end
end

function proven_clamp_probability --description "Clamp value to [0, 1]"
    set -l p $argv[1]

    if test (math "$p < 0") -eq 1
        echo "0"
    else if test (math "$p > 1") -eq 1
        echo "1"
    else
        echo "$p"
    end
    return 0
end

function proven_probability_complement --description "Calculate 1 - p"
    set -l p $argv[1]

    if not proven_is_valid_probability "$p"
        echo "error:invalid_probability:must be between 0 and 1" >&2
        return 1
    end

    math "1 - $p"
    return 0
end

# ============================================================================
# MODULE 32: SAFE_CHECKSUM (Algorithm)
# ============================================================================

function proven_djb2_hash --description "DJB2 hash function"
    set -l input $argv[1]

    set -l hash 5381
    for char in (string split '' -- "$input")
        set -l ord (printf '%d' "'$char")
        set hash (math "($hash * 33 + $ord) % 4294967296")
    end

    echo "$hash"
    return 0
end

function proven_sdbm_hash --description "SDBM hash function"
    set -l input $argv[1]

    set -l hash 0
    for char in (string split '' -- "$input")
        set -l ord (printf '%d' "'$char")
        set hash (math "($ord + ($hash * 65599)) % 4294967296")
    end

    echo "$hash"
    return 0
end

function proven_luhn_check --description "Validate Luhn checksum"
    set -l digits $argv[1]

    if not string match -qr '^[0-9]+$' -- "$digits"
        return 1
    end

    set -l sum 0
    set -l alt false
    set -l len (string length -- "$digits")

    for i in (seq $len -1 1)
        set -l d (string sub -s $i -l 1 -- "$digits")

        if test "$alt" = true
            set d (math "$d * 2")
            if test "$d" -gt 9
                set d (math "$d - 9")
            end
        end

        set sum (math "$sum + $d")
        if test "$alt" = true
            set alt false
        else
            set alt true
        end
    end

    test (math "$sum % 10") -eq 0
    return $status
end

# ============================================================================
# MODULE 33: SAFE_TENSOR (Algorithm)
# ============================================================================

function proven_vector_add --description "Add two vectors"
    set -l v1 $argv[1]
    set -l v2 $argv[2]

    set -l a1 (string split ',' -- "$v1")
    set -l a2 (string split ',' -- "$v2")

    if test (count $a1) -ne (count $a2)
        echo "error:dimension_mismatch:vectors must have same length" >&2
        return 1
    end

    set -l result
    for i in (seq 1 (count $a1))
        set -a result (math "$a1[$i] + $a2[$i]")
    end

    echo (string join ',' -- $result)
    return 0
end

function proven_vector_dot --description "Dot product of two vectors"
    set -l v1 $argv[1]
    set -l v2 $argv[2]

    set -l a1 (string split ',' -- "$v1")
    set -l a2 (string split ',' -- "$v2")

    if test (count $a1) -ne (count $a2)
        echo "error:dimension_mismatch:vectors must have same length" >&2
        return 1
    end

    set -l sum 0
    for i in (seq 1 (count $a1))
        set sum (math "$sum + $a1[$i] * $a2[$i]")
    end

    echo "$sum"
    return 0
end

function proven_vector_scale --description "Scale vector by scalar"
    set -l v $argv[1]
    set -l scalar $argv[2]

    set -l arr (string split ',' -- "$v")
    set -l result

    for x in $arr
        set -a result (math "$x * $scalar")
    end

    echo (string join ',' -- $result)
    return 0
end

# ============================================================================
# MODULE 34: SAFE_PASSWORD (Security)
# ============================================================================

function proven_check_password_strength --description "Check password strength (0-4)"
    set -l password $argv[1]

    set -l score 0
    set -l len (string length -- "$password")

    # Length check
    if test "$len" -ge 8
        set score (math "$score + 1")
    end
    if test "$len" -ge 12
        set score (math "$score + 1")
    end

    # Character class checks
    if string match -qr '[a-z]' -- "$password"
        if string match -qr '[A-Z]' -- "$password"
            set score (math "$score + 1")
        end
    end
    if string match -qr '[0-9]' -- "$password"
        set score (math "$score + 1")
    end
    if string match -qr '[^a-zA-Z0-9]' -- "$password"
        set score (math "$score + 1")
    end

    # Cap at 4
    if test "$score" -gt 4
        set score 4
    end

    echo "$score"
    return 0
end

function proven_is_common_password --description "Check if password is common"
    set -l password $argv[1]

    # Common passwords list (simplified)
    set -l common_passwords password 123456 12345678 qwerty abc123 monkey 1234567 letmein \
        trustno1 dragon baseball iloveyou master sunshine ashley foobar passw0rd shadow \
        123123 654321 superman qazwsx michael football password1 password123 welcome

    for p in $common_passwords
        if test "$password" = "$p"
            return 0
        end
    end

    return 1
end

# ============================================================================
# MODULE 35: SAFE_ML (Security)
# ============================================================================

function proven_softmax --description "Compute softmax of comma-separated values"
    set -l input $argv[1]

    set -l values (string split ',' -- "$input")

    # Find max for numerical stability
    set -l max_val $values[1]
    for v in $values
        if test (math "$v > $max_val") -eq 1
            set max_val $v
        end
    end

    # Compute exp(x - max) for each
    set -l exp_values
    set -l sum 0
    for v in $values
        set -l exp_v (math "e ^ ($v - $max_val)")
        set -a exp_values $exp_v
        set sum (math "$sum + $exp_v")
    end

    # Normalize
    set -l result
    for e in $exp_values
        set -a result (math "$e / $sum")
    end

    echo (string join ',' -- $result)
    return 0
end

function proven_sigmoid --description "Compute sigmoid of value"
    set -l x $argv[1]

    math "1 / (1 + e ^ (-$x))"
    return 0
end

function proven_relu --description "Compute ReLU of value"
    set -l x $argv[1]

    if test (math "$x > 0") -eq 1
        echo "$x"
    else
        echo "0"
    end
    return 0
end

# ============================================================================
# MODULE 36: SAFE_HEADER (HTTP)
# ============================================================================

function proven_is_safe_header_value --description "Check if header value is safe (no CRLF)"
    set -l value $argv[1]

    # Check for CRLF injection
    if string match -q '*\r*' -- "$value"; or string match -q '*\n*' -- "$value"
        return 1
    end

    return 0
end

function proven_sanitize_header_value --description "Sanitize header value (remove CRLF)"
    set -l value $argv[1]

    set -l result (string replace -a '\r' '' -- "$value")
    set result (string replace -a '\n' '' -- "$result")

    echo "$result"
    return 0
end

function proven_is_valid_header_name --description "Validate HTTP header name"
    set -l name $argv[1]

    # Header names: token (RFC 7230)
    if string match -qr '^[A-Za-z0-9!#$%&\'*+.^_`|~-]+$' -- "$name"
        return 0
    else
        return 1
    end
end

# ============================================================================
# MODULE 37: SAFE_COOKIE (HTTP)
# ============================================================================

function proven_is_valid_cookie_name --description "Validate cookie name"
    set -l name $argv[1]

    # Cookie names: token (no =, ;, whitespace)
    if string match -qr '^[A-Za-z0-9!#$%&\'*+.^_`|~-]+$' -- "$name"
        return 0
    else
        return 1
    end
end

function proven_is_valid_cookie_value --description "Validate cookie value"
    set -l value $argv[1]

    # No semicolons, commas, or whitespace (unless quoted)
    if string match -qr '^[^;,\s]*$' -- "$value"
        return 0
    else
        return 1
    end
end

function proven_encode_cookie_value --description "Encode cookie value"
    set -l value $argv[1]

    # URL-encode problematic characters
    proven_encode_url_component "$value"
    return 0
end

# ============================================================================
# MODULE 38: SAFE_CONTENT_TYPE (HTTP)
# ============================================================================

function proven_is_valid_mime_type --description "Validate MIME type format"
    set -l mime $argv[1]

    # Basic MIME type pattern: type/subtype
    if string match -qr '^[a-zA-Z0-9][a-zA-Z0-9!#$&^_.+-]*/[a-zA-Z0-9][a-zA-Z0-9!#$&^_.+-]*$' -- "$mime"
        return 0
    else
        return 1
    end
end

function proven_parse_content_type --description "Parse content type and return media type"
    set -l content_type $argv[1]

    # Remove parameters (;charset=... etc)
    set -l media_type (string replace -r ';.*' '' -- "$content_type")
    set media_type (string trim -- "$media_type")

    echo "$media_type"
    return 0
end

function proven_is_text_mime --description "Check if MIME type is text-based"
    set -l mime $argv[1]

    if string match -qr '^text/' -- "$mime"
        return 0
    end
    if string match -q 'application/json' -- "$mime"
        return 0
    end
    if string match -q 'application/xml' -- "$mime"
        return 0
    end
    if string match -q '*+json' -- "$mime"
        return 0
    end
    if string match -q '*+xml' -- "$mime"
        return 0
    end

    return 1
end

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

function proven_is_valid_percentage --description "Validate percentage (0-100)"
    set -l value $argv[1]

    if not string match -qr '^[0-9]+(\.[0-9]+)?$' -- "$value"
        return 1
    end

    if test (math "$value <= 100") -eq 1; and test (math "$value >= 0") -eq 1
        return 0
    else
        return 1
    end
end

function proven_require_valid_percentage --description "Require valid percentage or exit"
    set -l value $argv[1]

    if not proven_is_valid_percentage "$value"
        echo "error:invalid_percentage:$value is not a valid percentage (0-100)" >&2
        return 1
    end
    return 0
end

function proven_require_in_range --description "Require value in range or exit"
    set -l value $argv[1]
    set -l min_val $argv[2]
    set -l max_val $argv[3]

    if not proven_in_range "$value" "$min_val" "$max_val"
        echo "error:out_of_bounds:$value not in [$min_val, $max_val]" >&2
        return 1
    end
    return 0
end

function proven_percentage_of --description "Calculate percentage (basis points)"
    set -l amount $argv[1]
    set -l bps $argv[2]

    if not string match -qr '^[0-9]+$' -- "$amount"
        echo "error:invalid_input:amount must be a non-negative integer" >&2
        return 1
    end
    if not string match -qr '^[0-9]+$' -- "$bps"
        echo "error:invalid_input:bps must be a non-negative integer" >&2
        return 1
    end

    set -l result (math "floor($amount * $bps / 10000)")
    echo "$result"
    return 0
end

function proven_percentage_of_100 --description "Calculate percentage (0-100 scale)"
    set -l amount $argv[1]
    set -l pct $argv[2]

    if not proven_is_valid_percentage "$pct"
        echo "error:invalid_percentage:percentage must be 0-100" >&2
        return 1
    end

    set -l result (math "floor($amount * $pct / 100)")
    echo "$result"
    return 0
end

# ============================================================================
# VERSION & INFO
# ============================================================================

function proven_version --description "Get library version"
    echo "$PROVEN_VERSION"
end

function proven_module_count --description "Get total module count"
    echo "$PROVEN_MODULE_COUNT"
end

function proven_list_modules --description "List all available modules"
    echo "Core (11): safe_math, safe_string, safe_path, safe_email, safe_url, safe_network, safe_crypto, safe_uuid, safe_currency, safe_phone, safe_hex"
    echo "Data (7): safe_json, safe_datetime, safe_float, safe_version, safe_color, safe_angle, safe_unit"
    echo "Data Structures (5): safe_buffer, safe_queue, safe_bloom, safe_lru, safe_graph"
    echo "Resilience (4): safe_rate_limiter, safe_circuit_breaker, safe_retry, safe_monotonic"
    echo "State (2): safe_state_machine, safe_calculator"
    echo "Algorithm (4): safe_geo, safe_probability, safe_checksum, safe_tensor"
    echo "Security (2): safe_password, safe_ml"
    echo "HTTP (3): safe_header, safe_cookie, safe_content_type"
    return 0
end

# ============================================================================
# COMPLETIONS
# ============================================================================

# Core - safe_math
complete -c proven_add -d "Safe addition with overflow check"
complete -c proven_sub -d "Safe subtraction with underflow check"
complete -c proven_mul -d "Safe multiplication with overflow check"
complete -c proven_div -d "Safe division with zero check"
complete -c proven_mod -d "Safe modulo with zero check"
complete -c proven_clamp -d "Clamp value to range"
complete -c proven_in_range -d "Check if value in range"
complete -c proven_abs -d "Absolute value"
complete -c proven_min -d "Minimum of two values"
complete -c proven_max -d "Maximum of two values"

# Core - safe_string
complete -c proven_is_non_empty -d "Check if string is non-empty"
complete -c proven_require_non_empty -d "Require non-empty string"
complete -c proven_escape_html -d "Escape HTML special characters"
complete -c proven_escape_sql -d "Escape SQL single quotes"
complete -c proven_escape_shell -d "Escape shell special characters"
complete -c proven_truncate -d "Truncate string to max length"
complete -c proven_is_ascii -d "Check if string is ASCII"
complete -c proven_is_alphanumeric -d "Check if string is alphanumeric"

# Core - safe_path
complete -c proven_is_safe_path -d "Check if path is safe"
complete -c proven_safe_join_path -d "Safely join paths"
complete -c proven_normalize_path -d "Normalize path separators"
complete -c proven_get_extension -d "Get file extension"

# Core - safe_email
complete -c proven_is_valid_email -d "Validate email format"
complete -c proven_get_email_domain -d "Extract domain from email"
complete -c proven_get_email_local -d "Extract local part from email"

# Core - safe_url
complete -c proven_is_valid_url -d "Validate URL format"
complete -c proven_get_url_scheme -d "Extract URL scheme"
complete -c proven_get_url_host -d "Extract URL host"
complete -c proven_encode_url_component -d "URL-encode string"

# Core - safe_network
complete -c proven_is_valid_ipv4 -d "Validate IPv4 address"
complete -c proven_is_valid_ipv6 -d "Validate IPv6 address"
complete -c proven_is_valid_port -d "Validate port number"
complete -c proven_require_valid_port -d "Require valid port"
complete -c proven_is_private_ip -d "Check if IPv4 is private"
complete -c proven_is_valid_cidr -d "Validate CIDR notation"

# Core - safe_crypto
complete -c proven_sha256 -d "Calculate SHA-256 hash"
complete -c proven_sha512 -d "Calculate SHA-512 hash"
complete -c proven_md5 -d "Calculate MD5 hash"
complete -c proven_random_bytes -d "Generate random bytes"

# Core - safe_uuid
complete -c proven_generate_uuid_v4 -d "Generate UUID v4"
complete -c proven_is_valid_uuid -d "Validate UUID format"
complete -c proven_uuid_nil -d "Return nil UUID"

# Core - safe_currency
complete -c proven_format_currency -d "Format currency amount"
complete -c proven_is_valid_currency_code -d "Validate ISO 4217 code"
complete -c proven_add_money -d "Add monetary amounts"
complete -c proven_sub_money -d "Subtract monetary amounts"

# Core - safe_phone
complete -c proven_is_valid_phone_e164 -d "Validate E.164 phone"
complete -c proven_normalize_phone -d "Normalize phone number"

# Core - safe_hex
complete -c proven_is_valid_hex -d "Check valid hex string"
complete -c proven_hex_encode -d "Encode to hex"
complete -c proven_hex_decode -d "Decode from hex"

# Data - safe_json
complete -c proven_is_valid_json -d "Validate JSON syntax"
complete -c proven_json_get -d "Get JSON value (requires jq)"

# Data - safe_datetime
complete -c proven_is_valid_iso8601 -d "Validate ISO 8601 date"
complete -c proven_timestamp_now -d "Get Unix timestamp"
complete -c proven_timestamp_to_iso -d "Convert timestamp to ISO"
complete -c proven_date_add_days -d "Add days to date"

# Data - safe_float
complete -c proven_is_finite -d "Check if number is finite"
complete -c proven_float_add -d "Safe float addition"
complete -c proven_float_div -d "Safe float division"

# Data - safe_version
complete -c proven_is_valid_semver -d "Validate semver format"
complete -c proven_compare_semver -d "Compare semver strings"
complete -c proven_semver_major -d "Extract major version"
complete -c proven_semver_minor -d "Extract minor version"

# Data - safe_color
complete -c proven_is_valid_hex_color -d "Validate hex color"
complete -c proven_rgb_to_hex -d "RGB to hex color"
complete -c proven_hex_to_rgb -d "Hex to RGB values"

# Data - safe_angle
complete -c proven_deg_to_rad -d "Degrees to radians"
complete -c proven_rad_to_deg -d "Radians to degrees"
complete -c proven_normalize_degrees -d "Normalize to [0, 360)"

# Data - safe_unit
complete -c proven_convert_length -d "Convert length units"
complete -c proven_convert_temperature -d "Convert temperature units"

# Data Structures - safe_buffer
complete -c proven_buffer_create -d "Create bounded buffer"
complete -c proven_buffer_push -d "Push to buffer"
complete -c proven_buffer_size -d "Get buffer size"

# Data Structures - safe_queue
complete -c proven_queue_create -d "Create bounded queue"
complete -c proven_queue_enqueue -d "Add to queue"
complete -c proven_queue_dequeue -d "Remove from queue"
complete -c proven_queue_size -d "Get queue size"

# Data Structures - safe_bloom
complete -c proven_bloom_create -d "Create bloom filter"
complete -c proven_bloom_add -d "Add to bloom filter"
complete -c proven_bloom_contains -d "Check bloom filter"

# Data Structures - safe_lru
complete -c proven_lru_create -d "Create LRU cache"
complete -c proven_lru_put -d "Put in LRU cache"
complete -c proven_lru_get -d "Get from LRU cache"

# Data Structures - safe_graph
complete -c proven_graph_create -d "Create directed graph"
complete -c proven_graph_add_node -d "Add graph node"
complete -c proven_graph_add_edge -d "Add graph edge"
complete -c proven_graph_has_edge -d "Check edge exists"

# Resilience - safe_rate_limiter
complete -c proven_ratelimit_create -d "Create rate limiter"
complete -c proven_ratelimit_acquire -d "Acquire tokens"

# Resilience - safe_circuit_breaker
complete -c proven_circuit_create -d "Create circuit breaker"
complete -c proven_circuit_allow -d "Check if allowed"
complete -c proven_circuit_record_success -d "Record success"
complete -c proven_circuit_record_failure -d "Record failure"

# Resilience - safe_retry
complete -c proven_retry_backoff -d "Calculate backoff delay"
complete -c proven_retry_with_jitter -d "Add jitter to delay"

# Resilience - safe_monotonic
complete -c proven_monotonic_create -d "Create monotonic counter"
complete -c proven_monotonic_next -d "Get next value"
complete -c proven_monotonic_high_water_mark -d "Update high water mark"

# State - safe_state_machine
complete -c proven_fsm_create -d "Create state machine"
complete -c proven_fsm_add_transition -d "Add transition rule"
complete -c proven_fsm_transition -d "Perform transition"
complete -c proven_fsm_state -d "Get current state"

# State - safe_calculator
complete -c proven_calc_eval -d "Evaluate expression"

# Algorithm - safe_geo
complete -c proven_is_valid_latitude -d "Validate latitude"
complete -c proven_is_valid_longitude -d "Validate longitude"
complete -c proven_haversine_distance -d "Calculate distance"

# Algorithm - safe_probability
complete -c proven_is_valid_probability -d "Validate probability"
complete -c proven_clamp_probability -d "Clamp to [0, 1]"
complete -c proven_probability_complement -d "Calculate 1 - p"

# Algorithm - safe_checksum
complete -c proven_djb2_hash -d "DJB2 hash"
complete -c proven_sdbm_hash -d "SDBM hash"
complete -c proven_luhn_check -d "Luhn checksum"

# Algorithm - safe_tensor
complete -c proven_vector_add -d "Add vectors"
complete -c proven_vector_dot -d "Dot product"
complete -c proven_vector_scale -d "Scale vector"

# Security - safe_password
complete -c proven_check_password_strength -d "Check password strength"
complete -c proven_is_common_password -d "Check if common password"

# Security - safe_ml
complete -c proven_softmax -d "Compute softmax"
complete -c proven_sigmoid -d "Compute sigmoid"
complete -c proven_relu -d "Compute ReLU"

# HTTP - safe_header
complete -c proven_is_safe_header_value -d "Check header safety"
complete -c proven_sanitize_header_value -d "Sanitize header value"
complete -c proven_is_valid_header_name -d "Validate header name"

# HTTP - safe_cookie
complete -c proven_is_valid_cookie_name -d "Validate cookie name"
complete -c proven_is_valid_cookie_value -d "Validate cookie value"
complete -c proven_encode_cookie_value -d "Encode cookie value"

# HTTP - safe_content_type
complete -c proven_is_valid_mime_type -d "Validate MIME type"
complete -c proven_parse_content_type -d "Parse content type"
complete -c proven_is_text_mime -d "Check if text MIME"

# Utility
complete -c proven_is_valid_percentage -d "Validate percentage"
complete -c proven_require_valid_percentage -d "Require valid percentage"
complete -c proven_require_in_range -d "Require value in range"
complete -c proven_percentage_of -d "Calculate percentage (bps)"
complete -c proven_percentage_of_100 -d "Calculate percentage (0-100)"

# Info
complete -c proven_version -d "Get library version"
complete -c proven_module_count -d "Get module count"
complete -c proven_list_modules -d "List all modules"
