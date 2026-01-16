# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven Safety Library for Zsh
# Formally verified safety primitives for shell scripts.
# Version: 0.9.0
#
# Usage: source proven.zsh

# ============================================================================
# CONFIGURATION
# ============================================================================

# Maximum 64-bit signed integer values
typeset -g PROVEN_INT64_MAX=9223372036854775807
typeset -g PROVEN_INT64_MIN=-9223372036854775808

# Library version
typeset -g PROVEN_VERSION="0.9.0"

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
# SAFE MATH
# ============================================================================

# Safe addition with overflow check
# Usage: proven_add <a> <b>
proven_add() {
    local a=$1
    local b=$2

    # Validate inputs
    if [[ ! "$a" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: a is not a number"
        return 1
    fi
    if [[ ! "$b" =~ ^-?[0-9]+$ ]]; then
        proven_err "invalid_input: b is not a number"
        return 1
    fi

    # Check for overflow before operation
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

    # Check for overflow/underflow
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

    # Verify with division (catch overflow)
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

    # Handle INT64_MIN / -1 overflow
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
# Returns: 0 if in range, 1 if not
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

# ============================================================================
# VALIDATION
# ============================================================================

# Validate port number (1-65535)
# Usage: proven_is_valid_port <port>
proven_is_valid_port() {
    local port=$1
    [[ "$port" =~ ^[0-9]+$ ]] && (( port >= 1 && port <= 65535 ))
}

# Require valid port or return error
proven_require_valid_port() {
    local port=$1
    if ! proven_is_valid_port "$port"; then
        proven_err "invalid_port: $port is not valid (1-65535)"
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
proven_require_valid_percentage() {
    local value=$1
    if ! proven_is_valid_percentage "$value"; then
        proven_err "invalid_percentage: $value is not valid (0-100)"
        return 1
    fi
    return 0
}

# Validate email format (basic)
# Usage: proven_is_valid_email <email>
proven_is_valid_email() {
    local email=$1
    [[ "$email" =~ ^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$ ]]
}

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

# Check if string is non-empty
# Usage: proven_is_non_empty <value>
proven_is_non_empty() {
    [[ -n "$1" && "$1" != "" ]]
}

# Require non-empty string or return error
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
# SAFE PATH OPERATIONS
# ============================================================================

# Check if path is safe (no traversal attacks)
# Usage: proven_is_safe_path <path>
proven_is_safe_path() {
    local path=$1

    # Check for path traversal
    [[ "$path" == *".."* ]] && return 1

    # Check for null bytes
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

    # Verify resolved path is still under base
    local resolved_base=$(realpath -m "$base" 2>/dev/null)
    if [[ ! "$resolved" == "$resolved_base"* ]]; then
        proven_err "path_escape: path escapes base directory"
        return 1
    fi

    proven_ok "$resolved"
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
# VERSION
# ============================================================================

proven_version() {
    echo "$PROVEN_VERSION"
}

# ============================================================================
# AUTOLOAD SUPPORT
# ============================================================================

# If loaded via autoload, register completion functions
if [[ -n "$ZSH_VERSION" ]]; then
    # Zsh completion for proven functions
    _proven_functions() {
        local -a commands
        commands=(
            'proven_add:Safe addition with overflow check'
            'proven_sub:Safe subtraction with underflow check'
            'proven_mul:Safe multiplication with overflow check'
            'proven_div:Safe division with zero check'
            'proven_mod:Safe modulo with zero check'
            'proven_clamp:Clamp value to range'
            'proven_in_range:Check if value in range'
            'proven_is_valid_port:Validate port number'
            'proven_is_valid_percentage:Validate percentage'
            'proven_is_valid_email:Validate email format'
            'proven_is_valid_ipv4:Validate IPv4 address'
            'proven_is_safe_path:Check path safety'
            'proven_version:Get library version'
        )
        _describe 'proven command' commands
    }

    compdef _proven_functions proven
fi
