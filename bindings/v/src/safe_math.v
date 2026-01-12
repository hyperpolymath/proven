// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

// Safe arithmetic operations with overflow checking.

// Add two i64 values with overflow checking.
pub fn safe_add(a i64, b i64) ?i64 {
	// Check for overflow before operation
	if b > 0 && a > max_i64 - b {
		return none
	}
	if b < 0 && a < min_i64 - b {
		return none
	}
	return a + b
}

// Subtract two i64 values with overflow checking.
pub fn safe_sub(a i64, b i64) ?i64 {
	// Check for overflow before operation
	if b < 0 && a > max_i64 + b {
		return none
	}
	if b > 0 && a < min_i64 + b {
		return none
	}
	return a - b
}

// Multiply two i64 values with overflow checking.
pub fn safe_mul(a i64, b i64) ?i64 {
	if a == 0 || b == 0 {
		return i64(0)
	}

	result := a * b
	// Check if overflow occurred
	if a != 0 && result / a != b {
		return none
	}
	return result
}

// Divide two i64 values safely.
pub fn safe_div(a i64, b i64) ?i64 {
	if b == 0 {
		return none
	}
	// Check for overflow (MIN_VALUE / -1)
	if a == min_i64 && b == -1 {
		return none
	}
	return a / b
}

// Modulo operation with safety checks.
pub fn safe_mod(a i64, b i64) ?i64 {
	if b == 0 {
		return none
	}
	return a % b
}

// Absolute value with overflow checking.
pub fn safe_abs(a i64) ?i64 {
	if a == min_i64 {
		return none
	}
	if a < 0 {
		return -a
	}
	return a
}

// Negate with overflow checking.
pub fn safe_neg(a i64) ?i64 {
	if a == min_i64 {
		return none
	}
	return -a
}

// Power operation with overflow checking.
pub fn safe_pow(base i64, exp i64) ?i64 {
	if exp < 0 {
		return none
	}
	if exp == 0 {
		return i64(1)
	}
	if exp == 1 {
		return base
	}

	mut result := i64(1)
	mut b := base
	mut e := exp

	for e > 0 {
		if e & 1 == 1 {
			new_result := safe_mul(result, b) or { return none }
			result = new_result
		}
		e >>= 1
		if e > 0 {
			new_b := safe_mul(b, b) or { return none }
			b = new_b
		}
	}

	return result
}

// Safe sum of an array.
pub fn safe_sum(values []i64) ?i64 {
	mut result := i64(0)
	for v in values {
		new_result := safe_add(result, v) or { return none }
		result = new_result
	}
	return result
}

// Safe product of an array.
pub fn safe_product(values []i64) ?i64 {
	mut result := i64(1)
	for v in values {
		new_result := safe_mul(result, v) or { return none }
		result = new_result
	}
	return result
}

// Clamp value to range.
pub fn clamp(value i64, min_val i64, max_val i64) i64 {
	if value < min_val {
		return min_val
	}
	if value > max_val {
		return max_val
	}
	return value
}

// Check if value is in range.
pub fn in_range(value i64, min_val i64, max_val i64) bool {
	return value >= min_val && value <= max_val
}

// Constants
const max_i64 = i64(9223372036854775807)
const min_i64 = i64(-9223372036854775808)
