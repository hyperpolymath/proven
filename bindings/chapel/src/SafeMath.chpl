// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeMath - Overflow-safe integer arithmetic via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.

module SafeMath {

  public use LibProven;

  /**
   * Checked addition with overflow detection.
   *
   * :arg a: First operand.
   * :arg b: Second operand.
   * :returns: ``none`` on overflow, otherwise the sum.
   */
  proc safeAdd(a: int(64), b: int(64)): int(64)? {
    var r = provenMathAddChecked(a, b);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Checked subtraction with underflow detection.
   *
   * :arg a: First operand.
   * :arg b: Second operand.
   * :returns: ``none`` on underflow, otherwise the difference.
   */
  proc safeSub(a: int(64), b: int(64)): int(64)? {
    var r = provenMathSubChecked(a, b);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Checked multiplication with overflow detection.
   *
   * :arg a: First operand.
   * :arg b: Second operand.
   * :returns: ``none`` on overflow, otherwise the product.
   */
  proc safeMul(a: int(64), b: int(64)): int(64)? {
    var r = provenMathMulChecked(a, b);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Safe integer division.
   *
   * :arg a: Dividend.
   * :arg b: Divisor.
   * :returns: ``none`` on division by zero or overflow, otherwise the quotient.
   */
  proc safeDiv(a: int(64), b: int(64)): int(64)? {
    var r = provenMathDiv(a, b);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Safe modulo operation.
   *
   * :arg a: Dividend.
   * :arg b: Divisor.
   * :returns: ``none`` on division by zero, otherwise the remainder.
   */
  proc safeMod(a: int(64), b: int(64)): int(64)? {
    var r = provenMathMod(a, b);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Safe absolute value.
   *
   * :arg n: Input value.
   * :returns: ``none`` for INT64_MIN, otherwise the absolute value.
   */
  proc safeAbs(n: int(64)): int(64)? {
    var r = provenMathAbsSafe(n);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Clamp value to [lo, hi] range.
   *
   * :arg lo: Lower bound (inclusive).
   * :arg hi: Upper bound (inclusive).
   * :arg value: Value to clamp.
   * :returns: Clamped value (always succeeds).
   */
  proc safeClamp(lo: int(64), hi: int(64), value: int(64)): int(64) {
    return provenMathClamp(lo, hi, value);
  }

  /**
   * Integer exponentiation with overflow checking.
   *
   * :arg base: Base value.
   * :arg exp: Exponent (non-negative).
   * :returns: ``none`` on overflow, otherwise the result.
   */
  proc safePow(base: int(64), exp: uint(32)): int(64)? {
    var r = provenMathPowChecked(base, exp);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Safe negation (delegates to checked subtraction of 0 - n).
   *
   * :arg n: Value to negate.
   * :returns: ``none`` on overflow (INT64_MIN), otherwise the negation.
   */
  proc safeNegate(n: int(64)): int(64)? {
    return safeSub(0: int(64), n);
  }

}
