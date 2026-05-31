// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// SafeMath - Overflow-safe integer arithmetic via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.
//
// Status: GATED on proven#88 — the proven_math_* Zig exports are not yet
// present in libproven 0.9.0.  Calling any proc below produces a linker
// error.  Declarations are kept here as the documented C ABI contract.

module SafeMath {

  public use LibProven;

  /** Checked addition with overflow detection. */
  proc safeAdd(a: int(64), b: int(64)): Maybe(int(64)) {
    var r = provenMathAddChecked(a, b);
    if isOk(r.status) then return some(r.value);
    return absent(int(64));
  }

  /** Checked subtraction with underflow detection. */
  proc safeSub(a: int(64), b: int(64)): Maybe(int(64)) {
    var r = provenMathSubChecked(a, b);
    if isOk(r.status) then return some(r.value);
    return absent(int(64));
  }

  /** Checked multiplication with overflow detection. */
  proc safeMul(a: int(64), b: int(64)): Maybe(int(64)) {
    var r = provenMathMulChecked(a, b);
    if isOk(r.status) then return some(r.value);
    return absent(int(64));
  }

  /** Safe integer division. */
  proc safeDiv(a: int(64), b: int(64)): Maybe(int(64)) {
    var r = provenMathDiv(a, b);
    if isOk(r.status) then return some(r.value);
    return absent(int(64));
  }

  /** Safe modulo operation. */
  proc safeMod(a: int(64), b: int(64)): Maybe(int(64)) {
    var r = provenMathMod(a, b);
    if isOk(r.status) then return some(r.value);
    return absent(int(64));
  }

  /** Safe absolute value (``absent(int(64))`` for INT64_MIN). */
  proc safeAbs(n: int(64)): Maybe(int(64)) {
    var r = provenMathAbsSafe(n);
    if isOk(r.status) then return some(r.value);
    return absent(int(64));
  }

  /** Clamp value to ``[lo, hi]``.  Always succeeds. */
  proc safeClamp(lo: int(64), hi: int(64), value: int(64)): int(64) {
    return provenMathClamp(lo, hi, value);
  }

  /** Integer exponentiation with overflow checking. */
  proc safePow(base: int(64), exp: uint(32)): Maybe(int(64)) {
    var r = provenMathPowChecked(base, exp);
    if isOk(r.status) then return some(r.value);
    return absent(int(64));
  }

  /** Safe negation (delegates to checked subtraction of 0 - n). */
  proc safeNegate(n: int(64)): Maybe(int(64)) {
    return safeSub(0: int(64), n);
  }

}
