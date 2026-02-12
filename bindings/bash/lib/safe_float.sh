#!/bin/sh
# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_float.sh - Safe floating-point operations for POSIX shells
# Source this file: . /path/to/safe_float.sh
# Note: Uses awk for floating-point arithmetic (POSIX sh has no native float support)

# Global result variable
FLOAT_RESULT=""
FLOAT_ERROR=""

# Check if a string is a valid float
# Usage: float_is_valid "3.14" && echo "valid"
# Returns: 0 if valid, 1 otherwise
float_is_valid() {
    input="$1"

    [ -z "$input" ] && return 1

    case "$input" in
        # Match integers, floats, scientific notation
        [-+]?[0-9]*|[-+]?[0-9]*.[0-9]*|[-+]?[0-9]*[eE][-+]?[0-9]*|[-+]?[0-9]*.[0-9]*[eE][-+]?[0-9]*)
            # Additional validation with awk
            printf '%s' "$input" | awk '{ if ($0+0 == $0 || $0 == "0") exit 0; else exit 1 }' 2>/dev/null
            return $?
            ;;
        *)
            return 1
            ;;
    esac
}

# Parse and normalize a float
# Usage: normalized=$(float_parse "3.14")
# Returns: Normalized float string
float_parse() {
    input="$1"

    if ! float_is_valid "$input"; then
        FLOAT_ERROR="invalid_float"
        return 1
    fi

    printf '%s' "$input" | awk '{ printf "%.15g", $1 }'
}

# Add two floats
# Usage: result=$(float_add 1.5 2.5)
# Returns: Sum
float_add() {
    a="$1"
    b="$2"

    awk -v x="$a" -v y="$b" 'BEGIN { printf "%.15g", x + y }'
}

# Subtract two floats
# Usage: result=$(float_sub 5.5 2.5)
# Returns: Difference
float_sub() {
    a="$1"
    b="$2"

    awk -v x="$a" -v y="$b" 'BEGIN { printf "%.15g", x - y }'
}

# Multiply two floats
# Usage: result=$(float_mul 2.5 4.0)
# Returns: Product
float_mul() {
    a="$1"
    b="$2"

    awk -v x="$a" -v y="$b" 'BEGIN { printf "%.15g", x * y }'
}

# Divide two floats
# Usage: result=$(float_div 10.0 3.0)
# Returns: Quotient, or error on division by zero
float_div() {
    a="$1"
    b="$2"

    # Check for division by zero
    is_zero=$(awk -v x="$b" 'BEGIN { print (x == 0) ? 1 : 0 }')
    if [ "$is_zero" -eq 1 ]; then
        FLOAT_ERROR="division_by_zero"
        return 1
    fi

    awk -v x="$a" -v y="$b" 'BEGIN { printf "%.15g", x / y }'
}

# Calculate modulo
# Usage: result=$(float_mod 10.5 3.0)
# Returns: Remainder
float_mod() {
    a="$1"
    b="$2"

    is_zero=$(awk -v x="$b" 'BEGIN { print (x == 0) ? 1 : 0 }')
    if [ "$is_zero" -eq 1 ]; then
        FLOAT_ERROR="division_by_zero"
        return 1
    fi

    awk -v x="$a" -v y="$b" 'BEGIN { printf "%.15g", x - int(x/y) * y }'
}

# Calculate power
# Usage: result=$(float_pow 2.0 3.0)
# Returns: a^b
float_pow() {
    a="$1"
    b="$2"

    awk -v x="$a" -v y="$b" 'BEGIN { printf "%.15g", x ^ y }'
}

# Calculate square root
# Usage: result=$(float_sqrt 16.0)
# Returns: Square root
float_sqrt() {
    a="$1"

    is_neg=$(awk -v x="$a" 'BEGIN { print (x < 0) ? 1 : 0 }')
    if [ "$is_neg" -eq 1 ]; then
        FLOAT_ERROR="negative_sqrt"
        return 1
    fi

    awk -v x="$a" 'BEGIN { printf "%.15g", sqrt(x) }'
}

# Calculate absolute value
# Usage: result=$(float_abs -3.14)
# Returns: Absolute value
float_abs() {
    a="$1"

    awk -v x="$a" 'BEGIN { printf "%.15g", (x < 0) ? -x : x }'
}

# Round to nearest integer
# Usage: result=$(float_round 3.7)
# Returns: Rounded integer
float_round() {
    a="$1"

    awk -v x="$a" 'BEGIN { printf "%.0f", x }'
}

# Floor (round down)
# Usage: result=$(float_floor 3.7)
# Returns: 3
float_floor() {
    a="$1"

    awk -v x="$a" 'BEGIN { printf "%d", int(x) + (x < 0 && x != int(x) ? -1 : 0) }'
}

# Ceiling (round up)
# Usage: result=$(float_ceil 3.1)
# Returns: 4
float_ceil() {
    a="$1"

    awk -v x="$a" 'BEGIN { printf "%d", int(x) + (x > int(x) ? 1 : 0) }'
}

# Truncate towards zero
# Usage: result=$(float_trunc -3.7)
# Returns: -3
float_trunc() {
    a="$1"

    awk -v x="$a" 'BEGIN { printf "%d", int(x) }'
}

# Round to specified decimal places
# Usage: result=$(float_round_to 3.14159 2)
# Returns: 3.14
float_round_to() {
    a="$1"
    places="$2"

    awk -v x="$a" -v p="$places" 'BEGIN { printf "%.*f", p, x }'
}

# Compare two floats
# Usage: result=$(float_compare 3.14 3.15)
# Returns: -1, 0, or 1
float_compare() {
    a="$1"
    b="$2"

    awk -v x="$a" -v y="$b" 'BEGIN {
        if (x < y) print -1
        else if (x > y) print 1
        else print 0
    }'
}

# Check if float is less than
# Usage: float_lt 3.14 4.0 && echo "less"
# Returns: 0 if a < b, 1 otherwise
float_lt() {
    result=$(float_compare "$1" "$2")
    [ "$result" -eq -1 ]
}

# Check if float is greater than
# Usage: float_gt 4.0 3.14 && echo "greater"
# Returns: 0 if a > b, 1 otherwise
float_gt() {
    result=$(float_compare "$1" "$2")
    [ "$result" -eq 1 ]
}

# Check if floats are equal (with epsilon)
# Usage: float_eq 3.14 3.14 && echo "equal"
# Returns: 0 if equal within epsilon, 1 otherwise
float_eq() {
    a="$1"
    b="$2"
    epsilon="${3:-0.0000001}"

    awk -v x="$a" -v y="$b" -v e="$epsilon" 'BEGIN {
        diff = x - y
        if (diff < 0) diff = -diff
        exit (diff <= e) ? 0 : 1
    }'
}

# Get minimum of two floats
# Usage: result=$(float_min 3.14 2.71)
# Returns: Smaller value
float_min() {
    a="$1"
    b="$2"

    awk -v x="$a" -v y="$b" 'BEGIN { printf "%.15g", (x < y) ? x : y }'
}

# Get maximum of two floats
# Usage: result=$(float_max 3.14 2.71)
# Returns: Larger value
float_max() {
    a="$1"
    b="$2"

    awk -v x="$a" -v y="$b" 'BEGIN { printf "%.15g", (x > y) ? x : y }'
}

# Clamp value to range
# Usage: result=$(float_clamp 5.0 0.0 10.0)
# Returns: Value clamped to [min, max]
float_clamp() {
    value="$1"
    lo="$2"
    hi="$3"

    awk -v v="$value" -v l="$lo" -v h="$hi" 'BEGIN {
        if (v < l) printf "%.15g", l
        else if (v > h) printf "%.15g", h
        else printf "%.15g", v
    }'
}

# Linear interpolation
# Usage: result=$(float_lerp 0.0 10.0 0.5)
# Returns: Interpolated value
float_lerp() {
    a="$1"
    b="$2"
    t="$3"

    awk -v x="$a" -v y="$b" -v t="$t" 'BEGIN { printf "%.15g", x + (y - x) * t }'
}

# Check if float is NaN-like (in shell context, check for invalid)
# Usage: float_is_nan "NaN" && echo "is NaN"
# Returns: 0 if NaN-like string, 1 otherwise
float_is_nan() {
    input="$1"

    case "$input" in
        [Nn][Aa][Nn]|nan|NAN|NaN)
            return 0
            ;;
        *)
            ! float_is_valid "$input"
            return $?
            ;;
    esac
}

# Check if float is infinite
# Usage: float_is_inf "Inf" && echo "is infinite"
# Returns: 0 if infinite, 1 otherwise
float_is_inf() {
    input="$1"

    case "$input" in
        [Ii][Nn][Ff]|inf|INF|Inf|+[Ii][Nn][Ff]|-[Ii][Nn][Ff])
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# Check if float is zero (or very close)
# Usage: float_is_zero 0.0 && echo "zero"
# Returns: 0 if zero, 1 otherwise
float_is_zero() {
    input="$1"
    epsilon="${2:-0.0000001}"

    awk -v x="$input" -v e="$epsilon" 'BEGIN {
        if (x < 0) x = -x
        exit (x <= e) ? 0 : 1
    }'
}

# Check if float is positive
# Usage: float_is_positive 3.14 && echo "positive"
# Returns: 0 if > 0, 1 otherwise
float_is_positive() {
    input="$1"

    awk -v x="$input" 'BEGIN { exit (x > 0) ? 0 : 1 }'
}

# Check if float is negative
# Usage: float_is_negative -3.14 && echo "negative"
# Returns: 0 if < 0, 1 otherwise
float_is_negative() {
    input="$1"

    awk -v x="$input" 'BEGIN { exit (x < 0) ? 0 : 1 }'
}

# Sign function
# Usage: result=$(float_sign -3.14)
# Returns: -1, 0, or 1
float_sign() {
    input="$1"

    awk -v x="$input" 'BEGIN {
        if (x > 0) print 1
        else if (x < 0) print -1
        else print 0
    }'
}

# Natural logarithm
# Usage: result=$(float_log 2.718281828)
# Returns: ln(x)
float_log() {
    input="$1"

    awk -v x="$input" 'BEGIN { printf "%.15g", log(x) }'
}

# Exponential (e^x)
# Usage: result=$(float_exp 1.0)
# Returns: e^x
float_exp() {
    input="$1"

    awk -v x="$input" 'BEGIN { printf "%.15g", exp(x) }'
}

# Sine (radians)
# Usage: result=$(float_sin 3.14159)
# Returns: sin(x)
float_sin() {
    input="$1"

    awk -v x="$input" 'BEGIN { printf "%.15g", sin(x) }'
}

# Cosine (radians)
# Usage: result=$(float_cos 0.0)
# Returns: cos(x)
float_cos() {
    input="$1"

    awk -v x="$input" 'BEGIN { printf "%.15g", cos(x) }'
}

# Arc tangent
# Usage: result=$(float_atan 1.0)
# Returns: atan(x)
float_atan() {
    input="$1"

    awk -v x="$input" 'BEGIN { printf "%.15g", atan2(x, 1) }'
}

# Arc tangent of y/x
# Usage: result=$(float_atan2 1.0 1.0)
# Returns: atan2(y, x)
float_atan2() {
    y="$1"
    x="$2"

    awk -v y="$y" -v x="$x" 'BEGIN { printf "%.15g", atan2(y, x) }'
}

# Constants
FLOAT_PI="3.14159265358979323846"
FLOAT_E="2.71828182845904523536"
FLOAT_TAU="6.28318530717958647692"
