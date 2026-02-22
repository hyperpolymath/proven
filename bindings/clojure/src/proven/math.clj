;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeMath - JNA wrapper for proven_math_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.math
  "Safe arithmetic operations via libproven JNA FFI.

  Every function delegates to the Idris 2 verified implementation through
  the Zig C ABI. No arithmetic logic is reimplemented in Clojure.

  All fallible operations return nil on error (overflow, division by zero)."
  (:require [proven.native :as n]))

(defn add
  "Checked addition. Returns nil on overflow."
  [^long a ^long b]
  (n/call-int-result "proven_math_add_checked" (long a) (long b)))

(defn sub
  "Checked subtraction. Returns nil on underflow."
  [^long a ^long b]
  (n/call-int-result "proven_math_sub_checked" (long a) (long b)))

(defn mul
  "Checked multiplication. Returns nil on overflow."
  [^long a ^long b]
  (n/call-int-result "proven_math_mul_checked" (long a) (long b)))

(defn div
  "Safe division. Returns nil on division by zero."
  [^long a ^long b]
  (n/call-int-result "proven_math_div" (long a) (long b)))

(defn mod'
  "Safe modulo. Returns nil on division by zero.
  Named mod' to avoid collision with clojure.core/mod."
  [^long a ^long b]
  (n/call-int-result "proven_math_mod" (long a) (long b)))

(defn abs'
  "Safe absolute value. Returns nil for Long/MIN_VALUE.
  Named abs' to avoid collision with clojure.core/abs."
  [^long n]
  (n/call-int-result "proven_math_abs_safe" (long n)))

(defn clamp
  "Clamp value to range [lo, hi]. Cannot fail."
  [^long lo ^long hi ^long value]
  (n/call-long "proven_math_clamp" (long lo) (long hi) (long value)))

(defn pow'
  "Checked exponentiation. Returns nil on overflow."
  [^long base ^int exponent]
  (n/call-int-result "proven_math_pow_checked" (long base) (int exponent)))
