// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe-math.kk -- Safe arithmetic operations via libproven FFI.
//
// All computation is performed by the Idris 2 verified implementation.
// No arithmetic logic is reimplemented in Koka.
//
// Functions use the `<ndet>` effect since they call into the C FFI layer.
// Results are returned as `maybe<int64>` (Nothing on error).
module proven/safe-math

import std/num/int64
import proven/lib-proven

// ============================================================================
// Raw FFI declarations (extern C calls to libproven)
// ============================================================================

extern raw-add-checked(a: int64, b: int64): int-result
  c inline "{ int32_t s; int64_t v; ProvenIntResult r = proven_math_add_checked(#1, #2); s = r.status; v = r.value; return kk_proven_int_result(s, v, kk_context()); }"

extern raw-sub-checked(a: int64, b: int64): int-result
  c inline "{ int32_t s; int64_t v; ProvenIntResult r = proven_math_sub_checked(#1, #2); s = r.status; v = r.value; return kk_proven_int_result(s, v, kk_context()); }"

extern raw-mul-checked(a: int64, b: int64): int-result
  c inline "{ int32_t s; int64_t v; ProvenIntResult r = proven_math_mul_checked(#1, #2); s = r.status; v = r.value; return kk_proven_int_result(s, v, kk_context()); }"

extern raw-div(a: int64, b: int64): int-result
  c inline "{ int32_t s; int64_t v; ProvenIntResult r = proven_math_div(#1, #2); s = r.status; v = r.value; return kk_proven_int_result(s, v, kk_context()); }"

extern raw-mod(a: int64, b: int64): int-result
  c inline "{ int32_t s; int64_t v; ProvenIntResult r = proven_math_mod(#1, #2); s = r.status; v = r.value; return kk_proven_int_result(s, v, kk_context()); }"

extern raw-abs-safe(n: int64): int-result
  c inline "{ int32_t s; int64_t v; ProvenIntResult r = proven_math_abs_safe(#1); s = r.status; v = r.value; return kk_proven_int_result(s, v, kk_context()); }"

extern raw-clamp(lo: int64, hi: int64, value: int64): int64
  c inline "proven_math_clamp(#1, #2, #3)"

extern raw-pow-checked(base: int64, exp: int32): int-result
  c inline "{ int32_t s; int64_t v; ProvenIntResult r = proven_math_pow_checked(#1, (uint32_t)#2); s = r.status; v = r.value; return kk_proven_int_result(s, v, kk_context()); }"

// ============================================================================
// Safe public API
// ============================================================================

// Checked addition with overflow detection.
// Returns Nothing if the result would overflow int64.
pub fun safe-add(a: int64, b: int64): <ndet> maybe<int64>
  int-result-to-maybe(raw-add-checked(a, b))

// Checked subtraction with underflow detection.
// Returns Nothing if the result would underflow int64.
pub fun safe-sub(a: int64, b: int64): <ndet> maybe<int64>
  int-result-to-maybe(raw-sub-checked(a, b))

// Checked multiplication with overflow detection.
// Returns Nothing if the result would overflow int64.
pub fun safe-mul(a: int64, b: int64): <ndet> maybe<int64>
  int-result-to-maybe(raw-mul-checked(a, b))

// Safe division with zero-check.
// Returns Nothing if the denominator is zero or on overflow (INT64_MIN / -1).
pub fun safe-div(numerator: int64, denominator: int64): <ndet> maybe<int64>
  int-result-to-maybe(raw-div(numerator, denominator))

// Safe modulo with zero-check.
// Returns Nothing if the denominator is zero.
pub fun safe-mod(numerator: int64, denominator: int64): <ndet> maybe<int64>
  int-result-to-maybe(raw-mod(numerator, denominator))

// Safe absolute value.
// Returns Nothing for INT64_MIN (whose absolute value overflows int64).
pub fun safe-abs(n: int64): <ndet> maybe<int64>
  int-result-to-maybe(raw-abs-safe(n))

// Clamp a value to the range [lo, hi].
// Always succeeds. Returns lo if value < lo, hi if value > hi.
pub fun clamp(lo: int64, hi: int64, value: int64): <ndet> int64
  raw-clamp(lo, hi, value)

// Integer exponentiation with overflow checking.
// Returns Nothing if the result would overflow int64.
pub fun safe-pow(base: int64, exp: int32): <ndet> maybe<int64>
  int-result-to-maybe(raw-pow-checked(base, exp))
