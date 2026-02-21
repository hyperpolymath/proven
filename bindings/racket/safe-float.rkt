#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeFloat - Safe floating-point operations for Racket
;;

(require racket/math
         racket/format)

(provide
 ;; Safe result type
 (struct-out float-result)

 ;; Validation
 finite?
 valid-float?

 ;; Safe arithmetic
 safe-float-add
 safe-float-sub
 safe-float-mul
 safe-float-div
 safe-float-pow
 safe-float-sqrt
 safe-float-log
 safe-float-exp

 ;; Comparison with epsilon
 float-equal?
 float-less?
 float-greater?
 float-zero?

 ;; Rounding
 round-to-decimals
 round-to-significant
 truncate-decimals

 ;; Range operations
 float-clamp
 float-lerp
 float-normalize

 ;; Constants
 EPSILON
 MAX-FLOAT
 MIN-FLOAT
 POSITIVE-INFINITY
 NEGATIVE-INFINITY)

;; Result struct for safe operations
(struct float-result (value ok? error) #:transparent)

;; Constants
(define EPSILON 1e-10)
(define MAX-FLOAT 1.7976931348623157e+308)
(define MIN-FLOAT 2.2250738585072014e-308)
(define POSITIVE-INFINITY +inf.0)
(define NEGATIVE-INFINITY -inf.0)

;; Check if a number is finite
(define (finite? x)
  (and (real? x)
       (not (infinite? x))
       (not (nan? x))))

;; Check if valid float
(define (valid-float? x)
  (and (real? x) (not (nan? x))))

;; Safe addition
(define (safe-float-add a b)
  (let ([result (+ a b)])
    (if (finite? result)
        (float-result result #t #f)
        (float-result +inf.0 #f "Overflow in addition"))))

;; Safe subtraction
(define (safe-float-sub a b)
  (let ([result (- a b)])
    (if (finite? result)
        (float-result result #t #f)
        (float-result +inf.0 #f "Overflow in subtraction"))))

;; Safe multiplication
(define (safe-float-mul a b)
  (let ([result (* a b)])
    (if (finite? result)
        (float-result result #t #f)
        (float-result +inf.0 #f "Overflow in multiplication"))))

;; Safe division
(define (safe-float-div a b)
  (cond
    [(= b 0) (float-result 0 #f "Division by zero")]
    [else
     (let ([result (/ a b)])
       (if (finite? result)
           (float-result result #t #f)
           (float-result +inf.0 #f "Overflow in division")))]))

;; Safe power
(define (safe-float-pow base exponent)
  (cond
    [(and (= base 0) (< exponent 0))
     (float-result 0 #f "Zero raised to negative power")]
    [(and (< base 0) (not (integer? exponent)))
     (float-result 0 #f "Negative base with non-integer exponent")]
    [else
     (let ([result (expt base exponent)])
       (if (and (real? result) (finite? result))
           (float-result result #t #f)
           (float-result +inf.0 #f "Overflow in exponentiation")))]))

;; Safe square root
(define (safe-float-sqrt x)
  (if (< x 0)
      (float-result 0 #f "Square root of negative number")
      (float-result (sqrt x) #t #f)))

;; Safe logarithm
(define (safe-float-log x [base (exp 1)])
  (cond
    [(<= x 0) (float-result 0 #f "Logarithm of non-positive number")]
    [(<= base 0) (float-result 0 #f "Logarithm with non-positive base")]
    [(= base 1) (float-result 0 #f "Logarithm with base 1")]
    [else (float-result (/ (log x) (log base)) #t #f)]))

;; Safe exponential
(define (safe-float-exp x)
  (let ([result (exp x)])
    (if (finite? result)
        (float-result result #t #f)
        (float-result +inf.0 #f "Overflow in exponential"))))

;; Compare with epsilon tolerance
(define (float-equal? a b [epsilon EPSILON])
  (<= (abs (- a b)) epsilon))

(define (float-less? a b [epsilon EPSILON])
  (< a (- b epsilon)))

(define (float-greater? a b [epsilon EPSILON])
  (> a (+ b epsilon)))

(define (float-zero? x [epsilon EPSILON])
  (<= (abs x) epsilon))

;; Round to specified decimal places
(define (round-to-decimals x decimals)
  (let ([factor (expt 10 decimals)])
    (/ (round (* x factor)) factor)))

;; Round to significant figures
(define (round-to-significant x figures)
  (if (= x 0)
      0
      (let* ([magnitude (floor (log (abs x) 10))]
             [factor (expt 10 (- figures magnitude 1))])
        (/ (round (* x factor)) factor))))

;; Truncate to decimal places
(define (truncate-decimals x decimals)
  (let ([factor (expt 10 decimals)])
    (/ (truncate (* x factor)) factor)))

;; Clamp float to range
(define (float-clamp x min-val max-val)
  (cond
    [(< x min-val) min-val]
    [(> x max-val) max-val]
    [else x]))

;; Linear interpolation
(define (float-lerp a b t)
  (+ a (* (- b a) (float-clamp t 0.0 1.0))))

;; Normalize value to 0-1 range
(define (float-normalize x min-val max-val)
  (if (= min-val max-val)
      0.0
      (float-clamp (/ (- x min-val) (- max-val min-val)) 0.0 1.0)))
