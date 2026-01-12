// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:math"

// Safe arithmetic operations with overflow checking.

// Add two i64 values with overflow checking.
safe_add :: proc(a, b: i64) -> (result: i64, ok: bool) {
    // Check for potential overflow
    if b > 0 && a > max(i64) - b {
        return 0, false
    }
    if b < 0 && a < min(i64) - b {
        return 0, false
    }
    return a + b, true
}

// Subtract two i64 values with overflow checking.
safe_sub :: proc(a, b: i64) -> (result: i64, ok: bool) {
    if b < 0 && a > max(i64) + b {
        return 0, false
    }
    if b > 0 && a < min(i64) + b {
        return 0, false
    }
    return a - b, true
}

// Multiply two i64 values with overflow checking.
safe_mul :: proc(a, b: i64) -> (result: i64, ok: bool) {
    if a == 0 || b == 0 {
        return 0, true
    }

    r := a * b
    if a != 0 && r / a != b {
        return 0, false
    }
    return r, true
}

// Divide two i64 values safely.
safe_div :: proc(a, b: i64) -> (result: i64, ok: bool) {
    if b == 0 {
        return 0, false
    }
    // Check for overflow (MIN_VALUE / -1)
    if a == min(i64) && b == -1 {
        return 0, false
    }
    return a / b, true
}

// Modulo operation with safety checks.
safe_mod :: proc(a, b: i64) -> (result: i64, ok: bool) {
    if b == 0 {
        return 0, false
    }
    return a % b, true
}

// Absolute value with overflow checking.
safe_abs :: proc(a: i64) -> (result: i64, ok: bool) {
    if a == min(i64) {
        return 0, false
    }
    return abs(a), true
}

// Negate with overflow checking.
safe_neg :: proc(a: i64) -> (result: i64, ok: bool) {
    if a == min(i64) {
        return 0, false
    }
    return -a, true
}

// Power operation with overflow checking.
safe_pow :: proc(base, exp: i64) -> (result: i64, ok: bool) {
    if exp < 0 {
        return 0, false
    }
    if exp == 0 {
        return 1, true
    }
    if exp == 1 {
        return base, true
    }

    r: i64 = 1
    b := base
    e := exp

    for e > 0 {
        if e & 1 == 1 {
            new_r, ok := safe_mul(r, b)
            if !ok {
                return 0, false
            }
            r = new_r
        }
        e >>= 1
        if e > 0 {
            new_b, ok := safe_mul(b, b)
            if !ok {
                return 0, false
            }
            b = new_b
        }
    }

    return r, true
}

// Safe sum of a slice.
safe_sum :: proc(values: []i64) -> (result: i64, ok: bool) {
    r: i64 = 0
    for v in values {
        new_r, success := safe_add(r, v)
        if !success {
            return 0, false
        }
        r = new_r
    }
    return r, true
}

// Safe product of a slice.
safe_product :: proc(values: []i64) -> (result: i64, ok: bool) {
    r: i64 = 1
    for v in values {
        new_r, success := safe_mul(r, v)
        if !success {
            return 0, false
        }
        r = new_r
    }
    return r, true
}

// Clamp value to range.
clamp_value :: proc(value, min_val, max_val: i64) -> i64 {
    if value < min_val {
        return min_val
    }
    if value > max_val {
        return max_val
    }
    return value
}

// Check if value is in range.
in_range :: proc(value, min_val, max_val: i64) -> bool {
    return value >= min_val && value <= max_val
}
