;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeCalculator - JNA wrapper for proven_calculator_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.calculator
  "Safe expression evaluator via libproven JNA FFI."
  (:require [proven.native :as n])
  (:import [java.nio.charset StandardCharsets]))

(defn eval-expr
  "Evaluate a mathematical expression string. Returns the result or nil."
  [^String expression]
  (let [mem (n/to-native-string expression)]
    (when mem
      (let [bytes (.getBytes expression StandardCharsets/UTF_8)]
        (n/call-float-result "proven_calculator_eval" mem (long (alength bytes)))))))
