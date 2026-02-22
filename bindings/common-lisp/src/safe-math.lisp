;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeMath - Thin CFFI wrapper for libproven arithmetic operations.
;;;; All computation delegates to Idris 2 via the Zig FFI layer.

(in-package #:proven)

;;; FFI declarations
(defcfun ("proven_math_div" %math-div) (:struct int-result) (a :int64) (b :int64))
(defcfun ("proven_math_mod" %math-mod) (:struct int-result) (a :int64) (b :int64))
(defcfun ("proven_math_add_checked" %math-add-checked) (:struct int-result) (a :int64) (b :int64))
(defcfun ("proven_math_sub_checked" %math-sub-checked) (:struct int-result) (a :int64) (b :int64))
(defcfun ("proven_math_mul_checked" %math-mul-checked) (:struct int-result) (a :int64) (b :int64))
(defcfun ("proven_math_abs_safe" %math-abs-safe) (:struct int-result) (n :int64))
(defcfun ("proven_math_clamp" %math-clamp) :int64 (lo :int64) (hi :int64) (value :int64))
(defcfun ("proven_math_pow_checked" %math-pow-checked) (:struct int-result) (base :int64) (exp :uint32))

;;; Public API

(defun math-div (a b)
  "Safe integer division. Returns (values quotient t) or (values nil nil) on error."
  (extract-int-result (%math-div a b)))

(defun math-mod (a b)
  "Safe modulo. Returns (values remainder t) or (values nil nil) on error."
  (extract-int-result (%math-mod a b)))

(defun math-add-checked (a b)
  "Checked addition. Returns (values sum t) or (values nil nil) on overflow."
  (extract-int-result (%math-add-checked a b)))

(defun math-sub-checked (a b)
  "Checked subtraction. Returns (values difference t) or (values nil nil) on underflow."
  (extract-int-result (%math-sub-checked a b)))

(defun math-mul-checked (a b)
  "Checked multiplication. Returns (values product t) or (values nil nil) on overflow."
  (extract-int-result (%math-mul-checked a b)))

(defun math-abs-safe (n)
  "Safe absolute value. Returns (values abs t) or (values nil nil) for MIN_INT."
  (extract-int-result (%math-abs-safe n)))

(defun math-clamp (lo hi value)
  "Clamp VALUE to the range [LO, HI]."
  (%math-clamp lo hi value))

(defun math-pow-checked (base exponent)
  "Checked integer power. Returns (values result t) or (values nil nil) on overflow."
  (extract-int-result (%math-pow-checked base exponent)))
