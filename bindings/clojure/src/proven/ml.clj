;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeML - JNA wrapper for proven_ml_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.ml
  "Safe ML activation functions via libproven JNA FFI."
  (:require [proven.native :as n]))

(defn sigmoid
  "Sigmoid activation: 1 / (1 + exp(-x))."
  [^double x]
  (n/call-double "proven_ml_sigmoid" (double x)))

(defn relu
  "ReLU activation: max(0, x)."
  [^double x]
  (n/call-double "proven_ml_relu" (double x)))

(defn leaky-relu
  "Leaky ReLU activation: x if x > 0, alpha * x otherwise."
  [^double x ^double alpha]
  (n/call-double "proven_ml_leaky_relu" (double x) (double alpha)))

(defn clamp
  "Clamp value to range [min-val, max-val]."
  [^double x ^double min-val ^double max-val]
  (n/call-double "proven_ml_clamp" (double x) (double min-val) (double max-val)))
