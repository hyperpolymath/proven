;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeFloat - JNA wrapper for proven_float_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.float
  "Safe floating-point operations via libproven JNA FFI."
  (:require [proven.native :as n]))

(defn div
  "Safe division. Returns nil on division by zero or non-finite result."
  [^double a ^double b]
  (n/call-float-result "proven_float_div" (double a) (double b)))

(defn finite?
  "Check if a value is finite (not NaN, not Inf)."
  [^double x]
  (n/call-bool "proven_float_is_finite" (double x)))

(defn nan?
  "Check if a value is NaN."
  [^double x]
  (n/call-bool "proven_float_is_nan" (double x)))

(defn sqrt
  "Safe square root. Returns nil for negative inputs."
  [^double x]
  (n/call-float-result "proven_float_sqrt" (double x)))

(defn ln
  "Safe natural logarithm. Returns nil for non-positive inputs."
  [^double x]
  (n/call-float-result "proven_float_ln" (double x)))
