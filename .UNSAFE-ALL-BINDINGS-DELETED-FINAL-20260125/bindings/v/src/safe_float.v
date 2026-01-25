// SPDX-License-Identifier: PMPL-1.0
// Safe floating-point operations for V.
//
// Provides safe floating-point handling with NaN/Infinity protection,
// comparison tolerance, and clamping functions.

module proven

import math

// Check if float is finite (not NaN or Infinity)
pub fn is_finite(x f64) bool {
	return !math.is_nan(x) && !math.is_inf(x, 0)
}

// Check if float is NaN
pub fn is_nan(x f64) bool {
	return math.is_nan(x)
}

// Check if float is positive infinity
pub fn is_pos_inf(x f64) bool {
	return math.is_inf(x, 1)
}

// Check if float is negative infinity
pub fn is_neg_inf(x f64) bool {
	return math.is_inf(x, -1)
}

// Check if float is any infinity
pub fn is_inf(x f64) bool {
	return math.is_inf(x, 0)
}

// Safe addition with overflow check
pub fn safe_add_f64(a f64, b f64) ?f64 {
	result := a + b
	if !is_finite(result) {
		return none
	}
	return result
}

// Safe subtraction with underflow check
pub fn safe_sub_f64(a f64, b f64) ?f64 {
	result := a - b
	if !is_finite(result) {
		return none
	}
	return result
}

// Safe multiplication with overflow check
pub fn safe_mul_f64(a f64, b f64) ?f64 {
	result := a * b
	if !is_finite(result) {
		return none
	}
	return result
}

// Safe division with zero check
pub fn safe_div_f64(a f64, b f64) ?f64 {
	if b == 0.0 || !is_finite(b) {
		return none
	}
	result := a / b
	if !is_finite(result) {
		return none
	}
	return result
}

// Clamp float to range
pub fn clamp_f64(x f64, min f64, max f64) f64 {
	if x < min {
		return min
	}
	if x > max {
		return max
	}
	return x
}

// Clamp to unit interval [0, 1]
pub fn clamp_unit(x f64) f64 {
	return clamp_f64(x, 0.0, 1.0)
}

// Compare floats with epsilon tolerance
pub fn approx_eq(a f64, b f64, epsilon f64) bool {
	if is_nan(a) || is_nan(b) {
		return false
	}
	diff := math.abs(a - b)
	return diff <= epsilon
}

// Compare floats with relative tolerance
pub fn relative_eq(a f64, b f64, tolerance f64) bool {
	if is_nan(a) || is_nan(b) {
		return false
	}

	if a == b {
		return true
	}

	diff := math.abs(a - b)
	max_val := math.max(math.abs(a), math.abs(b))

	if max_val == 0.0 {
		return diff <= tolerance
	}

	return diff / max_val <= tolerance
}

// Default epsilon for comparisons
pub const float_epsilon = 1e-10

// Compare floats with default epsilon
pub fn nearly_eq(a f64, b f64) bool {
	return approx_eq(a, b, float_epsilon)
}

// Check if float is approximately zero
pub fn is_zero(x f64, epsilon f64) bool {
	return math.abs(x) <= epsilon
}

// Check if float is positive
pub fn is_positive(x f64) bool {
	return x > 0.0 && is_finite(x)
}

// Check if float is negative
pub fn is_negative(x f64) bool {
	return x < 0.0 && is_finite(x)
}

// Check if float is non-negative
pub fn is_non_negative(x f64) bool {
	return x >= 0.0 && is_finite(x)
}

// Check if float is non-positive
pub fn is_non_positive(x f64) bool {
	return x <= 0.0 && is_finite(x)
}

// Round to decimal places
pub fn round_to(x f64, decimals int) f64 {
	if decimals < 0 {
		return x
	}

	multiplier := math.pow(10.0, f64(decimals))
	return math.round(x * multiplier) / multiplier
}

// Floor to decimal places
pub fn floor_to(x f64, decimals int) f64 {
	if decimals < 0 {
		return x
	}

	multiplier := math.pow(10.0, f64(decimals))
	return math.floor(x * multiplier) / multiplier
}

// Ceiling to decimal places
pub fn ceil_to(x f64, decimals int) f64 {
	if decimals < 0 {
		return x
	}

	multiplier := math.pow(10.0, f64(decimals))
	return math.ceil(x * multiplier) / multiplier
}

// Truncate to decimal places
pub fn trunc_to(x f64, decimals int) f64 {
	if decimals < 0 {
		return x
	}

	multiplier := math.pow(10.0, f64(decimals))
	return math.trunc(x * multiplier) / multiplier
}

// Safe square root (returns none for negative)
pub fn safe_sqrt(x f64) ?f64 {
	if x < 0.0 || !is_finite(x) {
		return none
	}
	return math.sqrt(x)
}

// Safe natural log (returns none for non-positive)
pub fn safe_ln(x f64) ?f64 {
	if x <= 0.0 || !is_finite(x) {
		return none
	}
	return math.log(x)
}

// Safe log10 (returns none for non-positive)
pub fn safe_log10(x f64) ?f64 {
	if x <= 0.0 || !is_finite(x) {
		return none
	}
	return math.log10(x)
}

// Safe power (checks for overflow)
pub fn safe_pow(base f64, exp f64) ?f64 {
	if !is_finite(base) || !is_finite(exp) {
		return none
	}

	result := math.pow(base, exp)
	if !is_finite(result) {
		return none
	}
	return result
}

// Linear interpolation
pub fn lerp(a f64, b f64, t f64) f64 {
	return a + (b - a) * clamp_unit(t)
}

// Inverse linear interpolation
pub fn inverse_lerp(a f64, b f64, value f64) ?f64 {
	if a == b {
		return none
	}
	return (value - a) / (b - a)
}

// Map value from one range to another
pub fn map_range(value f64, in_min f64, in_max f64, out_min f64, out_max f64) ?f64 {
	if in_min == in_max {
		return none
	}

	t := (value - in_min) / (in_max - in_min)
	return out_min + (out_max - out_min) * t
}

// Smooth step interpolation
pub fn smooth_step(edge0 f64, edge1 f64, x f64) f64 {
	t := clamp_f64((x - edge0) / (edge1 - edge0), 0.0, 1.0)
	return t * t * (3.0 - 2.0 * t)
}

// Smoother step interpolation (Ken Perlin's version)
pub fn smoother_step(edge0 f64, edge1 f64, x f64) f64 {
	t := clamp_f64((x - edge0) / (edge1 - edge0), 0.0, 1.0)
	return t * t * t * (t * (t * 6.0 - 15.0) + 10.0)
}

// Sign function (-1, 0, or 1)
pub fn sign(x f64) f64 {
	if x > 0.0 {
		return 1.0
	}
	if x < 0.0 {
		return -1.0
	}
	return 0.0
}

// Step function (0 if x < edge, 1 otherwise)
pub fn step(edge f64, x f64) f64 {
	if x < edge {
		return 0.0
	}
	return 1.0
}

// Wrap value to range [min, max)
pub fn wrap(x f64, min f64, max f64) f64 {
	if min >= max {
		return min
	}

	range_size := max - min
	mut result := x - min

	result = math.fmod(result, range_size)
	if result < 0.0 {
		result += range_size
	}

	return result + min
}

// Compute mean of array
pub fn mean(values []f64) ?f64 {
	if values.len == 0 {
		return none
	}

	mut sum := 0.0
	for v in values {
		sum += v
	}

	return sum / f64(values.len)
}

// Compute variance of array
pub fn variance(values []f64) ?f64 {
	m := mean(values)?

	mut sum_sq := 0.0
	for v in values {
		diff := v - m
		sum_sq += diff * diff
	}

	return sum_sq / f64(values.len)
}

// Compute standard deviation of array
pub fn std_dev(values []f64) ?f64 {
	v := variance(values)?
	return math.sqrt(v)
}
