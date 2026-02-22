;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven/safe-math.fnl - Safe arithmetic wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via LuaJIT FFI.
;; Do NOT reimplement any arithmetic logic in Fennel.

(local {: lib : int-result : float-result} (require :proven.ffi))

;; ============================================================================
;; Safe integer arithmetic
;; ============================================================================

(fn div [a b]
  "Safe integer division. Returns nil on division by zero or overflow."
  (int-result (lib.proven_math_div a b)))

(fn mod* [a b]
  "Safe modulo. Returns nil on division by zero."
  (int-result (lib.proven_math_mod a b)))

(fn add-checked [a b]
  "Checked addition. Returns nil on overflow."
  (int-result (lib.proven_math_add_checked a b)))

(fn sub-checked [a b]
  "Checked subtraction. Returns nil on underflow."
  (int-result (lib.proven_math_sub_checked a b)))

(fn mul-checked [a b]
  "Checked multiplication. Returns nil on overflow."
  (int-result (lib.proven_math_mul_checked a b)))

(fn abs-safe [n]
  "Safe absolute value. Returns nil for INT64_MIN."
  (int-result (lib.proven_math_abs_safe n)))

(fn clamp [lo hi value]
  "Clamp value to [lo, hi] range."
  (tonumber (lib.proven_math_clamp lo hi value)))

(fn pow-checked [base exp]
  "Checked integer power. Returns nil on overflow."
  (int-result (lib.proven_math_pow_checked base exp)))

;; ============================================================================
;; Safe floating-point arithmetic
;; ============================================================================

(fn float-div [a b]
  "Safe floating-point division. Returns nil on division by zero or NaN."
  (float-result (lib.proven_float_div a b)))

(fn float-is-finite [x]
  "Return true if x is finite (not NaN or Inf)."
  (lib.proven_float_is_finite x))

(fn float-is-nan [x]
  "Return true if x is NaN."
  (lib.proven_float_is_nan x))

(fn float-sqrt [x]
  "Safe square root. Returns nil for negative or NaN input."
  (float-result (lib.proven_float_sqrt x)))

(fn float-ln [x]
  "Safe natural logarithm. Returns nil for non-positive or NaN input."
  (float-result (lib.proven_float_ln x)))

;; ============================================================================
;; Export
;; ============================================================================

{: div
 :mod mod*
 :add_checked add-checked
 :sub_checked sub-checked
 :mul_checked mul-checked
 :abs_safe    abs-safe
 : clamp
 :pow_checked pow-checked
 :float_div       float-div
 :float_is_finite float-is-finite
 :float_is_nan    float-is-nan
 :float_sqrt      float-sqrt
 :float_ln        float-ln}
