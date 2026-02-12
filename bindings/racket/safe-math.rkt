#lang racket/base

;; SPDX-License-Identifier: Apache-2.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeMath - Overflow-checked arithmetic for Racket
;;

(require racket/contract)

(provide
 ;; Result struct
 (struct-out safe-result)

 ;; Safe operations
 safe-add
 safe-sub
 safe-mul
 safe-div
 safe-mod
 safe-abs
 safe-negate

 ;; Range operations
 clamp
 in-range?

 ;; Constants
 MAX-INT64
 MIN-INT64)

;; Maximum and minimum values for 64-bit integers
(define MAX-INT64 9223372036854775807)
(define MIN-INT64 -9223372036854775808)

;; Result struct for safe operations
(struct safe-result (value ok?) #:transparent)

;; Safe addition with overflow checking
(define (safe-add a b)
  (cond
    ;; Check for positive overflow: b > 0 and a > MAX - b
    [(and (> b 0) (> a (- MAX-INT64 b)))
     (safe-result 0 #f)]
    ;; Check for negative overflow: b < 0 and a < MIN - b
    [(and (< b 0) (< a (- MIN-INT64 b)))
     (safe-result 0 #f)]
    ;; Safe to add
    [else (safe-result (+ a b) #t)]))

;; Safe subtraction with overflow checking
(define (safe-sub a b)
  (cond
    ;; Check for positive overflow: b < 0 and a > MAX + b
    [(and (< b 0) (> a (+ MAX-INT64 b)))
     (safe-result 0 #f)]
    ;; Check for negative overflow: b > 0 and a < MIN + b
    [(and (> b 0) (< a (+ MIN-INT64 b)))
     (safe-result 0 #f)]
    ;; Safe to subtract
    [else (safe-result (- a b) #t)]))

;; Safe multiplication with overflow checking
(define (safe-mul a b)
  (cond
    ;; Handle zero cases
    [(or (= a 0) (= b 0))
     (safe-result 0 #t)]
    ;; Compute and verify
    [else
     (let ([result (* a b)])
       (if (and (not (= a 0)) (not (= (quotient result a) b)))
           (safe-result 0 #f)
           (safe-result result #t)))]))

;; Safe division with zero check
(define (safe-div a b)
  (cond
    ;; Division by zero
    [(= b 0)
     (safe-result 0 #f)]
    ;; MIN / -1 overflow
    [(and (= a MIN-INT64) (= b -1))
     (safe-result 0 #f)]
    ;; Safe to divide
    [else (safe-result (quotient a b) #t)]))

;; Safe modulo with zero check
(define (safe-mod a b)
  (if (= b 0)
      (safe-result 0 #f)
      (safe-result (remainder a b) #t)))

;; Safe absolute value
(define (safe-abs a)
  (if (= a MIN-INT64)
      (safe-result 0 #f)
      (safe-result (abs a) #t)))

;; Safe negate
(define (safe-negate a)
  (if (= a MIN-INT64)
      (safe-result 0 #f)
      (safe-result (- a) #t)))

;; Clamp value to range
(define (clamp value min-val max-val)
  (cond
    [(< value min-val) min-val]
    [(> value max-val) max-val]
    [else value]))

;; Check if value is in range
(define (in-range? value min-val max-val)
  (and (>= value min-val) (<= value max-val)))
