// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:math"

// Default epsilon for float comparisons.
DEFAULT_EPSILON :: 1e-9

// Check if two floats are approximately equal.
float_approx_equal :: proc(a, b: f64, epsilon: f64 = DEFAULT_EPSILON) -> bool {
    diff := abs(a - b)
    if diff <= epsilon {
        return true
    }

    // Relative comparison for larger values
    larger := max(abs(a), abs(b))
    return diff <= larger * epsilon
}

// Check if a float is effectively zero.
float_is_zero :: proc(value: f64, epsilon: f64 = DEFAULT_EPSILON) -> bool {
    return abs(value) <= epsilon
}

// Check if a float is finite (not NaN or infinity).
float_is_finite :: proc(value: f64) -> bool {
    return !math.is_nan(value) && !math.is_inf(value)
}

// Safe division that returns nil on division by zero or invalid result.
safe_divide :: proc(a, b: f64) -> (result: f64, ok: bool) {
    if float_is_zero(b) {
        return 0, false
    }

    result = a / b

    if !float_is_finite(result) {
        return 0, false
    }

    return result, true
}

// Safe square root that returns nil for negative numbers.
safe_sqrt :: proc(value: f64) -> (result: f64, ok: bool) {
    if value < 0 || math.is_nan(value) {
        return 0, false
    }
    return math.sqrt(value), true
}

// Safe logarithm that returns nil for non-positive numbers.
safe_log :: proc(value: f64) -> (result: f64, ok: bool) {
    if value <= 0 || math.is_nan(value) {
        return 0, false
    }
    return math.ln(value), true
}

// Safe logarithm base 10.
safe_log10 :: proc(value: f64) -> (result: f64, ok: bool) {
    if value <= 0 || math.is_nan(value) {
        return 0, false
    }
    return math.log10(value), true
}

// Safe power function.
safe_pow :: proc(base, exp: f64) -> (result: f64, ok: bool) {
    if math.is_nan(base) || math.is_nan(exp) {
        return 0, false
    }

    // Handle special cases
    if base < 0 && exp != math.floor(exp) {
        // Negative base with non-integer exponent
        return 0, false
    }

    result = math.pow(base, exp)

    if !float_is_finite(result) {
        return 0, false
    }

    return result, true
}

// Clamp a float to a range.
float_clamp :: proc(value, min_val, max_val: f64) -> f64 {
    if math.is_nan(value) {
        return min_val
    }
    if value < min_val {
        return min_val
    }
    if value > max_val {
        return max_val
    }
    return value
}

// Clamp to unit range [0, 1].
float_clamp_unit :: proc(value: f64) -> f64 {
    return float_clamp(value, 0.0, 1.0)
}

// Linear interpolation.
float_lerp :: proc(a, b, t: f64) -> f64 {
    return a + (b - a) * float_clamp_unit(t)
}

// Inverse linear interpolation - find t given a, b, and value.
float_inverse_lerp :: proc(a, b, value: f64) -> (t: f64, ok: bool) {
    denom := b - a
    if float_is_zero(denom) {
        return 0, false
    }
    return (value - a) / denom, true
}

// Remap a value from one range to another.
float_remap :: proc(value, from_min, from_max, to_min, to_max: f64) -> (result: f64, ok: bool) {
    t, ok := float_inverse_lerp(from_min, from_max, value)
    if !ok {
        return 0, false
    }
    return float_lerp(to_min, to_max, t), true
}

// Round to a specific number of decimal places.
round_to_places :: proc(value: f64, places: int) -> f64 {
    if places < 0 {
        return value
    }

    multiplier := math.pow(10.0, f64(places))
    return math.round(value * multiplier) / multiplier
}

// Truncate to a specific number of decimal places.
truncate_to_places :: proc(value: f64, places: int) -> f64 {
    if places < 0 {
        return value
    }

    multiplier := math.pow(10.0, f64(places))
    return math.trunc(value * multiplier) / multiplier
}

// Check if a float is a valid percentage (0-100).
is_valid_percentage :: proc(value: f64) -> bool {
    return float_is_finite(value) && value >= 0.0 && value <= 100.0
}

// Convert percentage to ratio (0-1).
percentage_to_ratio :: proc(percentage: f64) -> f64 {
    return float_clamp(percentage / 100.0, 0.0, 1.0)
}

// Convert ratio to percentage.
ratio_to_percentage :: proc(ratio: f64) -> f64 {
    return float_clamp(ratio * 100.0, 0.0, 100.0)
}

// Sign of a number (-1, 0, 1).
float_sign :: proc(value: f64) -> int {
    if value < 0 {
        return -1
    }
    if value > 0 {
        return 1
    }
    return 0
}

// Check if two floats have the same sign.
same_sign :: proc(a, b: f64) -> bool {
    return (a >= 0 && b >= 0) || (a < 0 && b < 0)
}

// Normalize a value to unit range if valid range is provided.
normalize :: proc(value, min_val, max_val: f64) -> (result: f64, ok: bool) {
    range_size := max_val - min_val
    if float_is_zero(range_size) {
        return 0, false
    }
    return (value - min_val) / range_size, true
}

// Wrap a value to a range (useful for angles, etc.).
float_wrap :: proc(value, min_val, max_val: f64) -> f64 {
    range_size := max_val - min_val
    if float_is_zero(range_size) {
        return min_val
    }

    result := value
    for result < min_val {
        result += range_size
    }
    for result >= max_val {
        result -= range_size
    }
    return result
}

// Safe average of a slice.
float_average :: proc(values: []f64) -> (result: f64, ok: bool) {
    if len(values) == 0 {
        return 0, false
    }

    sum: f64 = 0
    for v in values {
        if !float_is_finite(v) {
            return 0, false
        }
        sum += v
    }

    return sum / f64(len(values)), true
}

// Safe weighted average.
float_weighted_average :: proc(values: []f64, weights: []f64) -> (result: f64, ok: bool) {
    if len(values) != len(weights) || len(values) == 0 {
        return 0, false
    }

    sum: f64 = 0
    weight_sum: f64 = 0

    for i := 0; i < len(values); i += 1 {
        if !float_is_finite(values[i]) || !float_is_finite(weights[i]) {
            return 0, false
        }
        sum += values[i] * weights[i]
        weight_sum += weights[i]
    }

    if float_is_zero(weight_sum) {
        return 0, false
    }

    return sum / weight_sum, true
}
