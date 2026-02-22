;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeProbability - JNA wrapper for proven_probability_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.probability
  "Safe probability operations via libproven JNA FFI.

  All values are clamped to [0.0, 1.0] by the verified implementation."
  (:require [proven.native :as n]))

(defn create
  "Create a valid probability (clamped to [0, 1])."
  [^double value]
  (n/call-double "proven_probability_create" (double value)))

(defn and'
  "Probability of A AND B (independent events).
  Named and' to avoid collision with clojure.core/and."
  [^double a ^double b]
  (n/call-double "proven_probability_and" (double a) (double b)))

(defn or-exclusive
  "Probability of exclusive or."
  [^double a ^double b]
  (n/call-double "proven_probability_or_exclusive" (double a) (double b)))

(defn not'
  "Complement probability: 1 - P.
  Named not' to avoid collision with clojure.core/not."
  [^double p]
  (n/call-double "proven_probability_not" (double p)))
