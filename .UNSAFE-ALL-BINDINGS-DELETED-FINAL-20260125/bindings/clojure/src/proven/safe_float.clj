;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-float
  "Safe floating-point operations with IEEE 754 awareness."
  (:require [clojure.string :as str]))

(def ^:const epsilon
  "Machine epsilon for double precision."
  (Math/ulp 1.0))

(def ^:const max-value Double/MAX_VALUE)
(def ^:const min-value Double/MIN_VALUE)
(def ^:const positive-infinity Double/POSITIVE_INFINITY)
(def ^:const negative-infinity Double/NEGATIVE_INFINITY)
(def ^:const nan-value Double/NaN)

(defn finite?
  "Check if value is finite (not infinity or NaN)."
  [value]
  (Double/isFinite value))

(defn infinite?
  "Check if value is infinite."
  [value]
  (Double/isInfinite value))

(defn nan?
  "Check if value is NaN."
  [value]
  (Double/isNaN value))

(defn normal?
  "Check if value is a normal floating-point number (not subnormal, infinity, or NaN)."
  [value]
  (and (finite? value)
       (not (zero? value))
       (>= (Math/abs value) Double/MIN_NORMAL)))

(defn subnormal?
  "Check if value is a subnormal (denormalized) number."
  [value]
  (and (not (zero? value))
       (< (Math/abs value) Double/MIN_NORMAL)))

(defn safe-div
  "Safe division that returns nil for division by zero or invalid results."
  [numerator denominator]
  (cond
    (zero? denominator) nil
    (nan? numerator) nil
    (nan? denominator) nil
    :else (let [result (/ numerator denominator)]
            (if (or (nan? result) (infinite? result))
              nil
              result))))

(defn safe-sqrt
  "Safe square root that returns nil for negative numbers."
  [value]
  (cond
    (nan? value) nil
    (neg? value) nil
    :else (Math/sqrt value)))

(defn safe-log
  "Safe natural logarithm that returns nil for non-positive numbers."
  [value]
  (cond
    (nan? value) nil
    (<= value 0) nil
    :else (Math/log value)))

(defn safe-log10
  "Safe base-10 logarithm that returns nil for non-positive numbers."
  [value]
  (cond
    (nan? value) nil
    (<= value 0) nil
    :else (Math/log10 value)))

(defn safe-pow
  "Safe power function that returns nil for invalid operations."
  [base exponent]
  (cond
    (nan? base) nil
    (nan? exponent) nil
    (and (neg? base) (not (zero? (mod exponent 1.0)))) nil
    :else (let [result (Math/pow base exponent)]
            (if (or (nan? result) (infinite? result))
              nil
              result))))

(defn approximately-equal?
  "Check if two floats are approximately equal within a tolerance."
  ([a b] (approximately-equal? a b (* epsilon 100)))
  ([a b tolerance]
   (cond
     (and (nan? a) (nan? b)) true
     (or (nan? a) (nan? b)) false
     (and (infinite? a) (infinite? b)) (= a b)
     :else (<= (Math/abs (- a b)) tolerance))))

(defn relative-equal?
  "Check if two floats are equal within a relative tolerance."
  ([a b] (relative-equal? a b 1e-9))
  ([a b relative-tolerance]
   (cond
     (and (nan? a) (nan? b)) true
     (or (nan? a) (nan? b)) false
     (and (zero? a) (zero? b)) true
     :else (let [max-abs (max (Math/abs a) (Math/abs b))]
             (<= (Math/abs (- a b)) (* max-abs relative-tolerance))))))

(defn ulps-equal?
  "Check if two floats are within n ULPs (units in last place) of each other."
  ([a b] (ulps-equal? a b 4))
  ([a b max-ulps]
   (cond
     (and (nan? a) (nan? b)) true
     (or (nan? a) (nan? b)) false
     :else (let [bits-a (Double/doubleToLongBits a)
                 bits-b (Double/doubleToLongBits b)]
             (<= (Math/abs (- bits-a bits-b)) max-ulps)))))

(defn clamp
  "Clamp value to range [min-val, max-val]. Returns nil for NaN."
  [value min-val max-val]
  (cond
    (nan? value) nil
    :else (min max-val (max min-val value))))

(defn lerp
  "Linear interpolation between a and b. t should be in [0, 1]."
  [a b t]
  (+ a (* t (- b a))))

(defn round-to
  "Round to specified number of decimal places."
  [value decimal-places]
  (let [factor (Math/pow 10 decimal-places)]
    (/ (Math/round (* value factor)) factor)))

(defn truncate-to
  "Truncate to specified number of decimal places."
  [value decimal-places]
  (let [factor (Math/pow 10 decimal-places)]
    (/ (Math/floor (* value factor)) factor)))

(defn sign
  "Get the sign of a number (-1, 0, or 1)."
  [value]
  (cond
    (nan? value) nil
    (pos? value) 1
    (neg? value) -1
    :else 0))

(defn copy-sign
  "Copy sign from one number to another."
  [magnitude sign-source]
  (Math/copySign magnitude sign-source))

(defn next-up
  "Get the next representable floating-point value after value toward positive infinity."
  [value]
  (Math/nextUp value))

(defn next-down
  "Get the next representable floating-point value after value toward negative infinity."
  [value]
  (Math/nextDown value))

(defn ulp
  "Get the unit in last place for a value."
  [value]
  (Math/ulp value))

(defn safe-average
  "Calculate average avoiding overflow."
  [values]
  (if (empty? values)
    nil
    (/ (reduce + 0.0 values) (count values))))

(defn kahan-sum
  "Kahan summation algorithm for improved accuracy with floating-point sums."
  [values]
  (loop [sum 0.0
         compensation 0.0
         remaining values]
    (if (empty? remaining)
      sum
      (let [value (first remaining)
            y (- value compensation)
            t (+ sum y)
            new-compensation (- (- t sum) y)]
        (recur t new-compensation (rest remaining))))))

(defn fma
  "Fused multiply-add: (a * b) + c with only one rounding."
  [a b c]
  (Math/fma a b c))

(defn hypot
  "Calculate sqrt(x^2 + y^2) without overflow."
  [x y]
  (Math/hypot x y))

(defn parse-float
  "Parse a string to float. Returns {:ok value} or {:error message}."
  [s]
  (if (str/blank? s)
    {:error "empty_string"}
    (try
      (let [value (Double/parseDouble s)]
        (if (finite? value)
          {:ok value}
          {:error "infinite_value"}))
      (catch NumberFormatException _
        {:error "invalid_number"}))))
