;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven-safe-math.el - Safe arithmetic wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via FFI.
;; Do NOT reimplement any arithmetic logic in Emacs Lisp.

;;; Commentary:
;;
;; Provides overflow/underflow/division-by-zero safe integer arithmetic
;; and safe floating-point operations.  Every function returns nil on
;; error instead of crashing or producing undefined behavior.

;;; Code:

(require 'proven-ffi)

;;; ============================================================================
;;; Safe integer arithmetic
;;; ============================================================================

(defun proven-safe-math-add (a b)
  "Safe integer addition with overflow detection.
Returns the sum of A and B, or nil if overflow would occur.
Calls proven_math_add_checked via FFI."
  (condition-case nil
      (proven-ffi--extract-int-result (proven--ffi-math-add-checked a b))
    (error nil)))

(defun proven-safe-math-sub (a b)
  "Safe integer subtraction with underflow detection.
Returns A - B, or nil if underflow would occur.
Calls proven_math_sub_checked via FFI."
  (condition-case nil
      (proven-ffi--extract-int-result (proven--ffi-math-sub-checked a b))
    (error nil)))

(defun proven-safe-math-mul (a b)
  "Safe integer multiplication with overflow detection.
Returns A * B, or nil if overflow would occur.
Calls proven_math_mul_checked via FFI."
  (condition-case nil
      (proven-ffi--extract-int-result (proven--ffi-math-mul-checked a b))
    (error nil)))

(defun proven-safe-math-div (a b)
  "Safe integer division.
Returns the quotient of A / B, or nil if B is zero or if
A is INT64_MIN and B is -1.
Calls proven_math_div via FFI."
  (condition-case nil
      (proven-ffi--extract-int-result (proven--ffi-math-div a b))
    (error nil)))

(defun proven-safe-math-mod (a b)
  "Safe modulo operation.
Returns A mod B, or nil if B is zero.
Calls proven_math_mod via FFI."
  (condition-case nil
      (proven-ffi--extract-int-result (proven--ffi-math-mod a b))
    (error nil)))

(defun proven-safe-math-abs (n)
  "Safe absolute value.
Returns |N|, or nil if N is INT64_MIN (cannot be represented as positive).
Calls proven_math_abs_safe via FFI."
  (condition-case nil
      (proven-ffi--extract-int-result (proven--ffi-math-abs-safe n))
    (error nil)))

(defun proven-safe-math-clamp (lo hi value)
  "Clamp VALUE to the range [LO, HI].
Always succeeds; returns the clamped value.
Calls proven_math_clamp via FFI."
  (condition-case nil
      (proven--ffi-math-clamp lo hi value)
    (error nil)))

(defun proven-safe-math-pow (base exponent)
  "Safe integer exponentiation with overflow checking.
Returns BASE raised to EXPONENT, or nil if overflow would occur.
EXPONENT must be non-negative.
Calls proven_math_pow_checked via FFI."
  (condition-case nil
      (proven-ffi--extract-int-result (proven--ffi-math-pow-checked base exponent))
    (error nil)))

;;; ============================================================================
;;; Safe floating-point arithmetic
;;; ============================================================================

(defun proven-safe-float-div (a b)
  "Safe floating-point division.
Returns A / B as a float, or nil if B is zero or result is NaN.
Calls proven_float_div via FFI."
  (condition-case nil
      (proven-ffi--extract-float-result (proven--ffi-float-div a b))
    (error nil)))

(defun proven-safe-float-sqrt (x)
  "Safe square root.
Returns sqrt(X), or nil if X is negative or NaN.
Calls proven_float_sqrt via FFI."
  (condition-case nil
      (proven-ffi--extract-float-result (proven--ffi-float-sqrt x))
    (error nil)))

(defun proven-safe-float-ln (x)
  "Safe natural logarithm.
Returns ln(X), or nil if X <= 0 or NaN.
Calls proven_float_ln via FFI."
  (condition-case nil
      (proven-ffi--extract-float-result (proven--ffi-float-ln x))
    (error nil)))

(defun proven-safe-float-finite-p (x)
  "Return non-nil if X is a finite float (not NaN or Inf).
Calls proven_float_is_finite via FFI."
  (condition-case nil
      (proven--ffi-float-is-finite x)
    (error nil)))

(defun proven-safe-float-nan-p (x)
  "Return non-nil if X is NaN.
Calls proven_float_is_nan via FFI."
  (condition-case nil
      (proven--ffi-float-is-nan x)
    (error nil)))

(provide 'proven-safe-math)

;;; proven-safe-math.el ends here
