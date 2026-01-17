;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-math
  "Safe mathematical operations with overflow protection.")

(def ^:const max-long Long/MAX_VALUE)
(def ^:const min-long Long/MIN_VALUE)

(defn add
  "Checked addition that detects overflow.
   Returns result or nil on overflow."
  [a b]
  (try
    (Math/addExact (long a) (long b))
    (catch ArithmeticException _ nil)))

(defn sub
  "Checked subtraction that detects overflow.
   Returns result or nil on overflow."
  [a b]
  (try
    (Math/subtractExact (long a) (long b))
    (catch ArithmeticException _ nil)))

(defn mul
  "Checked multiplication that detects overflow.
   Returns result or nil on overflow."
  [a b]
  (try
    (Math/multiplyExact (long a) (long b))
    (catch ArithmeticException _ nil)))

(defn div
  "Checked division that handles division by zero.
   Returns result or nil on error."
  [numerator denominator]
  (cond
    (zero? denominator) nil
    (and (= (long numerator) min-long) (= (long denominator) -1)) nil
    :else (quot (long numerator) (long denominator))))

(defn mod-safe
  "Checked modulo operation.
   Returns result or nil if divisor is zero."
  [a b]
  (if (zero? b)
    nil
    (mod a b)))

(defn neg
  "Checked negation.
   Returns negated value or nil if a == min-long."
  [a]
  (try
    (Math/negateExact (long a))
    (catch ArithmeticException _ nil)))

(defn abs-safe
  "Checked absolute value.
   Returns absolute value or nil if a == min-long."
  [a]
  (if (= (long a) min-long)
    nil
    (Math/abs (long a))))

(defn pow
  "Checked power operation.
   Returns result or nil on overflow or negative exponent."
  [base exponent]
  (cond
    (neg? exponent) nil
    (zero? exponent) 1
    (zero? base) 0
    (= base 1) 1
    (= base -1) (if (even? exponent) 1 -1)
    :else
    (let [result (.pow (biginteger base) exponent)]
      (if (or (> result (biginteger max-long))
              (< result (biginteger min-long)))
        nil
        (long result)))))

(defn clamp
  "Clamp value to range [min-val, max-val]."
  [value min-val max-val]
  (min max-val (max min-val value)))

(defn would-add-overflow?
  "Check if addition would overflow without performing it."
  [a b]
  (nil? (add a b)))

(defn would-mul-overflow?
  "Check if multiplication would overflow without performing it."
  [a b]
  (nil? (mul a b)))

(defn checked-inc
  "Checked increment. Returns result or nil on overflow."
  [n]
  (add n 1))

(defn checked-dec
  "Checked decrement. Returns result or nil on overflow."
  [n]
  (sub n 1))

(defn safe-sum
  "Sum a collection with overflow checking.
   Returns nil if any addition overflows."
  [coll]
  (reduce (fn [acc x]
            (if (nil? acc)
              nil
              (add acc x)))
          0
          coll))

(defn safe-product
  "Multiply a collection with overflow checking.
   Returns nil if any multiplication overflows."
  [coll]
  (reduce (fn [acc x]
            (if (nil? acc)
              nil
              (mul acc x)))
          1
          coll))
