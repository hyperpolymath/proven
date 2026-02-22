// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeMath - Overflow-safe integer arithmetic via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This class is a thin wrapper; it does NOT reimplement any logic.

primitive SafeMath
  """
  Formally verified arithmetic that cannot crash.

  Every method returns `(I64 | None)` -- the I64 value on success,
  or `None` on error (overflow, underflow, division by zero).

  Example:
  ```pony
  match SafeMath.add(2, 3)
  | let v: I64 => env.out.print("Sum: " + v.string())
  | None => env.out.print("Overflow!")
  end
  ```
  """

  fun add(a: I64, b: I64): (I64 | None) =>
    """Checked addition with overflow detection."""
    let r = _LibProven.math_add_checked(a, b)
    if r.status == ProvenOk() then r.value else None end

  fun sub(a: I64, b: I64): (I64 | None) =>
    """Checked subtraction with underflow detection."""
    let r = _LibProven.math_sub_checked(a, b)
    if r.status == ProvenOk() then r.value else None end

  fun mul(a: I64, b: I64): (I64 | None) =>
    """Checked multiplication with overflow detection."""
    let r = _LibProven.math_mul_checked(a, b)
    if r.status == ProvenOk() then r.value else None end

  fun div(a: I64, b: I64): (I64 | None) =>
    """Safe integer division. Returns None on division by zero or overflow."""
    let r = _LibProven.math_div(a, b)
    if r.status == ProvenOk() then r.value else None end

  fun mod'(a: I64, b: I64): (I64 | None) =>
    """Safe modulo operation. Returns None on division by zero."""
    let r = _LibProven.math_mod(a, b)
    if r.status == ProvenOk() then r.value else None end

  fun abs'(n: I64): (I64 | None) =>
    """Safe absolute value. Returns None for I64.min_value()."""
    let r = _LibProven.math_abs_safe(n)
    if r.status == ProvenOk() then r.value else None end

  fun clamp(lo: I64, hi: I64, value: I64): I64 =>
    """Clamp value to [lo, hi] range. Always succeeds."""
    _LibProven.math_clamp(lo, hi, value)

  fun pow'(base: I64, exp: U32): (I64 | None) =>
    """Integer exponentiation with overflow checking."""
    let r = _LibProven.math_pow_checked(base, exp)
    if r.status == ProvenOk() then r.value else None end

  fun negate(n: I64): (I64 | None) =>
    """Safe negation. Returns None for I64.min_value()."""
    sub(0, n)
