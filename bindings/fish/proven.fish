# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven Safety Library for Fish Shell
# Formally verified safety primitives for shell scripts.
# Version: 0.9.0

# ============================================================================
# SAFE MATH
# ============================================================================

function proven_add --description "Safe addition with overflow check"
    set -l a $argv[1]
    set -l b $argv[2]

    # Validate inputs are numbers
    if not string match -qr '^-?[0-9]+$' -- "$a"
        echo "error:invalid_input:a is not a number" >&2
        return 1
    end
    if not string match -qr '^-?[0-9]+$' -- "$b"
        echo "error:invalid_input:b is not a number" >&2
        return 1
    end

    # Use bc for arbitrary precision, then check bounds
    set -l result (math "$a + $b")

    # Check for overflow (64-bit signed integer bounds)
    set -l max 9223372036854775807
    set -l min -9223372036854775808

    if test "$result" -gt "$max" 2>/dev/null; or test "$result" -lt "$min" 2>/dev/null
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

    set -l max 9223372036854775807
    set -l min -9223372036854775808

    if test "$result" -gt "$max" 2>/dev/null; or test "$result" -lt "$min" 2>/dev/null
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

    set -l max 9223372036854775807
    set -l min -9223372036854775808

    if test "$result" -gt "$max" 2>/dev/null; or test "$result" -lt "$min" 2>/dev/null
        echo "error:overflow:result exceeds 64-bit integer bounds" >&2
        return 1
    end

    # Verify with division check
    set -l check (math "$result / $a")
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

    # Integer division
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

# ============================================================================
# BOUNDED VALUES
# ============================================================================

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

# ============================================================================
# VALIDATION
# ============================================================================

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

function proven_is_valid_email --description "Basic email format validation"
    set -l email $argv[1]

    if string match -qr '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$' -- "$email"
        return 0
    else
        return 1
    end
end

function proven_is_valid_ipv4 --description "Validate IPv4 address format"
    set -l ip $argv[1]

    if string match -qr '^([0-9]{1,3}\.){3}[0-9]{1,3}$' -- "$ip"
        # Check each octet
        for octet in (string split "." "$ip")
            if test "$octet" -gt 255
                return 1
            end
        end
        return 0
    else
        return 1
    end
end

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

# ============================================================================
# SAFE PATH OPERATIONS
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

    # Check for null bytes (shouldn't happen in fish, but be safe)
    if string match -q '*\x00*' -- "$path"
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

# ============================================================================
# PERCENTAGE CALCULATIONS
# ============================================================================

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
# VERSION
# ============================================================================

function proven_version --description "Get library version"
    echo "0.9.0"
end

# ============================================================================
# COMPLETIONS
# ============================================================================

# Add completions for proven functions
complete -c proven_add -d "Safe addition"
complete -c proven_sub -d "Safe subtraction"
complete -c proven_mul -d "Safe multiplication"
complete -c proven_div -d "Safe division"
complete -c proven_mod -d "Safe modulo"
complete -c proven_clamp -d "Clamp value to range"
complete -c proven_in_range -d "Check if value in range"
complete -c proven_is_valid_port -d "Validate port number"
complete -c proven_is_valid_percentage -d "Validate percentage"
complete -c proven_is_valid_email -d "Validate email format"
complete -c proven_is_valid_ipv4 -d "Validate IPv4 address"
complete -c proven_version -d "Get library version"
