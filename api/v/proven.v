// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Proven V-lang API — Type-safe alternatives to dangerous standard library functions.
// Wraps the Zig FFI which implements the Idris2 ABI (80+ safe modules).
module proven

// ═══════════════════════════════════════════════════════════════════════
// Safe Math (wraps SafeMath.idr → safe_math.zig)
// ═══════════════════════════════════════════════════════════════════════

fn C.proven_safe_add(a i64, b i64, out &i64) int
fn C.proven_safe_mul(a i64, b i64, out &i64) int
fn C.proven_safe_div(a i64, b i64, out &i64) int

// safe_add performs checked addition, returns false on overflow.
pub fn safe_add(a i64, b i64) ?i64 {
	mut result := i64(0)
	if C.proven_safe_add(a, b, &result) == 0 {
		return result
	}
	return none
}

// safe_mul performs checked multiplication, returns false on overflow.
pub fn safe_mul(a i64, b i64) ?i64 {
	mut result := i64(0)
	if C.proven_safe_mul(a, b, &result) == 0 {
		return result
	}
	return none
}

// safe_div performs checked division (no division by zero).
pub fn safe_div(a i64, b i64) ?i64 {
	mut result := i64(0)
	if C.proven_safe_div(a, b, &result) == 0 {
		return result
	}
	return none
}

// ═══════════════════════════════════════════════════════════════════════
// Safe String (wraps SafeString.idr → safe_string.zig)
// ═══════════════════════════════════════════════════════════════════════

fn C.proven_safe_substring(s_ptr &u8, s_len int, start int, end int, out_ptr &&u8, out_len &int) int

// safe_substring extracts a substring with bounds checking.
pub fn safe_substring(s string, start int, end int) ?string {
	mut out_ptr := &u8(0)
	mut out_len := 0
	if C.proven_safe_substring(s.str, s.len, start, end, &out_ptr, &out_len) == 0 {
		unsafe {
			return out_ptr.vstring_with_len(out_len)
		}
	}
	return none
}

// ═══════════════════════════════════════════════════════════════════════
// Safe JSON (wraps SafeJson.idr → safe_json.zig)
// ═══════════════════════════════════════════════════════════════════════

fn C.proven_json_validate(json_ptr &u8, json_len int) int

// json_is_valid checks if a string is valid JSON without parsing.
pub fn json_is_valid(json string) bool {
	return C.proven_json_validate(json.str, json.len) == 0
}
