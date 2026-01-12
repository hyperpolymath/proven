;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeMath - Overflow-checked arithmetic for Common Lisp

(in-package #:proven)

;;; Constants for 64-bit integers
(defconstant +max-int64+ 9223372036854775807)
(defconstant +min-int64+ -9223372036854775808)

;;; Result struct for safe operations
(defstruct safe-result
  (value 0 :type integer)
  (ok-p nil :type boolean))

;;; Safe addition with overflow checking
(defun safe-add (a b)
  "Add two integers with overflow checking.
   Returns a SAFE-RESULT with VALUE and OK-P."
  (cond
    ;; Check for positive overflow: b > 0 and a > MAX - b
    ((and (> b 0) (> a (- +max-int64+ b)))
     (make-safe-result :value 0 :ok-p nil))
    ;; Check for negative overflow: b < 0 and a < MIN - b
    ((and (< b 0) (< a (- +min-int64+ b)))
     (make-safe-result :value 0 :ok-p nil))
    ;; Safe to add
    (t (make-safe-result :value (+ a b) :ok-p t))))

;;; Safe subtraction with overflow checking
(defun safe-sub (a b)
  "Subtract two integers with overflow checking.
   Returns a SAFE-RESULT with VALUE and OK-P."
  (cond
    ;; Check for positive overflow: b < 0 and a > MAX + b
    ((and (< b 0) (> a (+ +max-int64+ b)))
     (make-safe-result :value 0 :ok-p nil))
    ;; Check for negative overflow: b > 0 and a < MIN + b
    ((and (> b 0) (< a (+ +min-int64+ b)))
     (make-safe-result :value 0 :ok-p nil))
    ;; Safe to subtract
    (t (make-safe-result :value (- a b) :ok-p t))))

;;; Safe multiplication with overflow checking
(defun safe-mul (a b)
  "Multiply two integers with overflow checking.
   Returns a SAFE-RESULT with VALUE and OK-P."
  (cond
    ;; Handle zero cases
    ((or (zerop a) (zerop b))
     (make-safe-result :value 0 :ok-p t))
    ;; Compute and verify
    (t
     (let ((result (* a b)))
       (if (and (not (zerop a))
                (not (= (truncate result a) b)))
           (make-safe-result :value 0 :ok-p nil)
           (make-safe-result :value result :ok-p t))))))

;;; Safe division with zero check
(defun safe-div (a b)
  "Divide two integers with zero check.
   Returns a SAFE-RESULT with VALUE and OK-P."
  (cond
    ;; Division by zero
    ((zerop b)
     (make-safe-result :value 0 :ok-p nil))
    ;; MIN / -1 overflow
    ((and (= a +min-int64+) (= b -1))
     (make-safe-result :value 0 :ok-p nil))
    ;; Safe to divide
    (t (make-safe-result :value (truncate a b) :ok-p t))))

;;; Safe modulo with zero check
(defun safe-mod (a b)
  "Compute modulo with zero check.
   Returns a SAFE-RESULT with VALUE and OK-P."
  (if (zerop b)
      (make-safe-result :value 0 :ok-p nil)
      (make-safe-result :value (mod a b) :ok-p t)))

;;; Safe absolute value
(defun safe-abs (a)
  "Compute absolute value with overflow check.
   Returns a SAFE-RESULT with VALUE and OK-P."
  (if (= a +min-int64+)
      (make-safe-result :value 0 :ok-p nil)
      (make-safe-result :value (abs a) :ok-p t)))

;;; Safe negate
(defun safe-negate (a)
  "Negate an integer with overflow check.
   Returns a SAFE-RESULT with VALUE and OK-P."
  (if (= a +min-int64+)
      (make-safe-result :value 0 :ok-p nil)
      (make-safe-result :value (- a) :ok-p t)))

;;; Clamp value to range
(defun clamp (value min-val max-val)
  "Clamp VALUE to be within [MIN-VAL, MAX-VAL]."
  (cond
    ((< value min-val) min-val)
    ((> value max-val) max-val)
    (t value)))

;;; Check if value is in range
(defun in-range-p (value min-val max-val)
  "Return T if VALUE is within [MIN-VAL, MAX-VAL]."
  (and (>= value min-val) (<= value max-val)))
