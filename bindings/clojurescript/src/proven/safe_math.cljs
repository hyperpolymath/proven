;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeMath - Arithmetic operations that cannot crash.
;;
;; All functions delegate to libproven via JS interop (transitive FFI).
;; Returns nil on error (overflow, underflow, division by zero).
;; NEVER reimplements logic.

(ns proven.safe-math
  "Safe arithmetic operations with proven correctness guarantees.
   All computation delegates to the Idris 2 core via FFI."
  (:require [proven.ffi :as ffi]))


(defn safe-add
  "Checked addition. Returns nil on overflow.

  Example:
    (safe-add 5 3)  ;=> 8
    (safe-add js/Number.MAX_SAFE_INTEGER 1)  ;=> nil"
  [a b]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_math_add_checked lib a b)]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn safe-sub
  "Checked subtraction. Returns nil on underflow.

  Example:
    (safe-sub 10 3)  ;=> 7"
  [a b]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_math_sub_checked lib a b)]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn safe-mul
  "Checked multiplication. Returns nil on overflow.

  Example:
    (safe-mul 6 7)  ;=> 42"
  [a b]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_math_mul_checked lib a b)]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn safe-div
  "Safe integer division. Returns nil on division by zero or overflow.

  Example:
    (safe-div 10 2)  ;=> 5
    (safe-div 10 0)  ;=> nil"
  [numerator denominator]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_math_div lib numerator denominator)]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn safe-mod
  "Safe modulo. Returns nil on division by zero.

  Example:
    (safe-mod 10 3)  ;=> 1
    (safe-mod 10 0)  ;=> nil"
  [numerator denominator]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_math_mod lib numerator denominator)]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn safe-abs
  "Safe absolute value. Returns nil for INT64_MIN.

  Example:
    (safe-abs -42)  ;=> 42"
  [n]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_math_abs_safe lib n)]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn safe-negate
  "Safe negation via checked subtraction from zero. Returns nil on overflow.

  Example:
    (safe-negate 5)  ;=> -5"
  [n]
  (safe-sub 0 n))


(defn clamp
  "Clamp value to [lo, hi] range.

  Example:
    (clamp 0 100 50)   ;=> 50
    (clamp 0 100 150)  ;=> 100"
  [lo hi value]
  (when-let [lib (ffi/get-lib)]
    (.proven_math_clamp lib lo hi value)))


(defn safe-pow
  "Checked integer exponentiation. Returns nil on overflow.

  Example:
    (safe-pow 2 10)  ;=> 1024"
  [base exp]
  (when (neg? exp)
    (return nil))
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_math_pow_checked lib base exp)]
      (when (ffi/ok? (.-status result))
        (.-value result)))))
