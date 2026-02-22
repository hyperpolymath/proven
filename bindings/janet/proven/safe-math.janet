# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# proven/safe-math.janet - Safe arithmetic wrappers for libproven.
# All computation delegates to the Idris 2 verified core via Janet FFI.
# Do NOT reimplement any arithmetic logic in Janet.

(import ./ffi :prefix "ffi/")

## ============================================================================
## Safe integer arithmetic
## ============================================================================

(defn add
  "Safe integer addition with overflow detection.
  Returns the sum, or nil if overflow would occur."
  [a b]
  (ffi/extract-int-result (ffi/proven-math-add-checked a b)))

(defn sub
  "Safe integer subtraction with underflow detection.
  Returns a - b, or nil if underflow would occur."
  [a b]
  (ffi/extract-int-result (ffi/proven-math-sub-checked a b)))

(defn mul
  "Safe integer multiplication with overflow detection.
  Returns a * b, or nil if overflow would occur."
  [a b]
  (ffi/extract-int-result (ffi/proven-math-mul-checked a b)))

(defn div
  "Safe integer division.
  Returns the quotient, or nil on division by zero or overflow."
  [a b]
  (ffi/extract-int-result (ffi/proven-math-div a b)))

(defn mod*
  "Safe modulo operation.
  Returns a mod b, or nil on division by zero."
  [a b]
  (ffi/extract-int-result (ffi/proven-math-mod a b)))

(defn abs*
  "Safe absolute value.
  Returns |n|, or nil if n is INT64_MIN."
  [n]
  (ffi/extract-int-result (ffi/proven-math-abs-safe n)))

(defn clamp
  "Clamp value to [lo, hi] range. Always succeeds."
  [lo hi value]
  (ffi/proven-math-clamp lo hi value))

(defn pow
  "Safe integer exponentiation with overflow checking.
  Returns base^exp, or nil on overflow."
  [base exp]
  (ffi/extract-int-result (ffi/proven-math-pow-checked base exp)))

## ============================================================================
## Safe floating-point arithmetic
## ============================================================================

(defn float-div
  "Safe floating-point division.
  Returns a / b, or nil on division by zero or NaN."
  [a b]
  (ffi/extract-float-result (ffi/proven-float-div a b)))

(defn float-finite?
  "Return true if x is finite (not NaN or Inf)."
  [x]
  (ffi/proven-float-is-finite x))

(defn float-nan?
  "Return true if x is NaN."
  [x]
  (ffi/proven-float-is-nan x))

(defn float-sqrt
  "Safe square root. Returns nil for negative or NaN input."
  [x]
  (ffi/extract-float-result (ffi/proven-float-sqrt x)))

(defn float-ln
  "Safe natural logarithm. Returns nil for non-positive or NaN input."
  [x]
  (ffi/extract-float-result (ffi/proven-float-ln x)))
