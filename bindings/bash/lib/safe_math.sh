#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_math.sh - Safe arithmetic operations for Bash
# Source this file: source /path/to/safe_math.sh

# 64-bit integer limits (Bash uses native integers)
declare -r PROVEN_MAX_INT=9223372036854775807
declare -r PROVEN_MIN_INT=-9223372036854775808

# Safe division - returns 1 on error, result in PROVEN_RESULT
# Usage: safe_div 10 2 && echo "$PROVEN_RESULT"
safe_div() {
    local numerator="$1"
    local denominator="$2"
    PROVEN_RESULT=""
    PROVEN_ERROR=""

    if [[ -z "$numerator" || -z "$denominator" ]]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if [[ "$denominator" -eq 0 ]]; then
        PROVEN_ERROR="division_by_zero"
        return 1
    fi

    # Handle MIN_INT / -1 overflow
    if [[ "$numerator" -eq "$PROVEN_MIN_INT" && "$denominator" -eq -1 ]]; then
        PROVEN_ERROR="overflow"
        return 1
    fi

    PROVEN_RESULT=$((numerator / denominator))
    return 0
}

# Safe division with default - always returns 0
# Usage: result=$(safe_div_or 42 10 0)
safe_div_or() {
    local default="$1"
    local numerator="$2"
    local denominator="$3"

    if safe_div "$numerator" "$denominator"; then
        echo "$PROVEN_RESULT"
    else
        echo "$default"
    fi
}

# Safe modulo - returns 1 on error
# Usage: safe_mod 10 3 && echo "$PROVEN_RESULT"
safe_mod() {
    local numerator="$1"
    local denominator="$2"
    PROVEN_RESULT=""
    PROVEN_ERROR=""

    if [[ -z "$numerator" || -z "$denominator" ]]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if [[ "$denominator" -eq 0 ]]; then
        PROVEN_ERROR="division_by_zero"
        return 1
    fi

    PROVEN_RESULT=$((numerator % denominator))
    return 0
}

# Checked addition - returns 1 on overflow
# Usage: safe_add 5 3 && echo "$PROVEN_RESULT"
safe_add() {
    local a="$1"
    local b="$2"
    PROVEN_RESULT=""
    PROVEN_ERROR=""

    if [[ -z "$a" || -z "$b" ]]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    # Check for overflow before operation
    if [[ "$b" -gt 0 && "$a" -gt $((PROVEN_MAX_INT - b)) ]]; then
        PROVEN_ERROR="overflow"
        return 1
    fi
    if [[ "$b" -lt 0 && "$a" -lt $((PROVEN_MIN_INT - b)) ]]; then
        PROVEN_ERROR="overflow"
        return 1
    fi

    PROVEN_RESULT=$((a + b))
    return 0
}

# Checked subtraction - returns 1 on underflow
# Usage: safe_sub 10 3 && echo "$PROVEN_RESULT"
safe_sub() {
    local a="$1"
    local b="$2"
    PROVEN_RESULT=""
    PROVEN_ERROR=""

    if [[ -z "$a" || -z "$b" ]]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    # Check for underflow before operation
    if [[ "$b" -gt 0 && "$a" -lt $((PROVEN_MIN_INT + b)) ]]; then
        PROVEN_ERROR="underflow"
        return 1
    fi
    if [[ "$b" -lt 0 && "$a" -gt $((PROVEN_MAX_INT + b)) ]]; then
        PROVEN_ERROR="underflow"
        return 1
    fi

    PROVEN_RESULT=$((a - b))
    return 0
}

# Checked multiplication - returns 1 on overflow
# Note: Bash doesn't have native overflow detection, this is best-effort
# Usage: safe_mul 5 3 && echo "$PROVEN_RESULT"
safe_mul() {
    local a="$1"
    local b="$2"
    PROVEN_RESULT=""
    PROVEN_ERROR=""

    if [[ -z "$a" || -z "$b" ]]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if [[ "$a" -eq 0 || "$b" -eq 0 ]]; then
        PROVEN_RESULT=0
        return 0
    fi

    # Simplified overflow check
    local result=$((a * b))

    # Verify by division (if it doesn't round-trip, overflow occurred)
    if [[ "$a" -ne 0 && $((result / a)) -ne "$b" ]]; then
        PROVEN_ERROR="overflow"
        return 1
    fi

    PROVEN_RESULT="$result"
    return 0
}

# Safe absolute value - returns 1 for MIN_INT
# Usage: safe_abs -5 && echo "$PROVEN_RESULT"
safe_abs() {
    local n="$1"
    PROVEN_RESULT=""
    PROVEN_ERROR=""

    if [[ -z "$n" ]]; then
        PROVEN_ERROR="invalid_argument"
        return 1
    fi

    if [[ "$n" -eq "$PROVEN_MIN_INT" ]]; then
        PROVEN_ERROR="overflow"
        return 1
    fi

    if [[ "$n" -lt 0 ]]; then
        PROVEN_RESULT=$((-n))
    else
        PROVEN_RESULT="$n"
    fi
    return 0
}

# Clamp value to range
# Usage: result=$(safe_clamp 0 100 150)  # returns 100
safe_clamp() {
    local lo="$1"
    local hi="$2"
    local value="$3"

    if [[ "$value" -lt "$lo" ]]; then
        echo "$lo"
    elif [[ "$value" -gt "$hi" ]]; then
        echo "$hi"
    else
        echo "$value"
    fi
}

# Check if value is a valid integer
# Usage: is_integer "123" && echo "yes"
is_integer() {
    local value="$1"
    [[ "$value" =~ ^-?[0-9]+$ ]]
}

# Safe percentage calculation
# Usage: safe_percent_of 50 200 && echo "$PROVEN_RESULT"  # 100
safe_percent_of() {
    local percent="$1"
    local total="$2"

    if ! safe_mul "$percent" "$total"; then
        return 1
    fi
    local product="$PROVEN_RESULT"

    if ! safe_div "$product" 100; then
        return 1
    fi
    return 0
}
