;;; SPDX-License-Identifier: PMPL-1.0
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeMath - Overflow-checked arithmetic for Guile Scheme
;;;
;;; All operations return a result alist with 'value and 'ok keys.
;;; Never throws, never crashes.

(define-module (proven safe-math)
  #:export (MAX-INT64
            MIN-INT64
            make-result
            result-value
            result-ok?
            safe-add
            safe-sub
            safe-mul
            safe-div
            safe-mod
            safe-abs
            safe-negate
            clamp
            in-range?))

;;; Maximum and minimum values for 64-bit integers
(define MAX-INT64 9223372036854775807)
(define MIN-INT64 -9223372036854775808)

;;; Result constructor
(define (make-result value ok)
  `((value . ,value) (ok . ,ok)))

;;; Result accessors
(define (result-value result)
  (assoc-ref result 'value))

(define (result-ok? result)
  (assoc-ref result 'ok))

;;; Safe addition with overflow checking
(define (safe-add a b)
  (cond
   ;; Check for positive overflow: b > 0 and a > MAX - b
   ((and (> b 0) (> a (- MAX-INT64 b)))
    (make-result 0 #f))
   ;; Check for negative overflow: b < 0 and a < MIN - b
   ((and (< b 0) (< a (- MIN-INT64 b)))
    (make-result 0 #f))
   ;; Safe to add
   (else (make-result (+ a b) #t))))

;;; Safe subtraction with overflow checking
(define (safe-sub a b)
  (cond
   ;; Check for positive overflow: b < 0 and a > MAX + b
   ((and (< b 0) (> a (+ MAX-INT64 b)))
    (make-result 0 #f))
   ;; Check for negative overflow: b > 0 and a < MIN + b
   ((and (> b 0) (< a (+ MIN-INT64 b)))
    (make-result 0 #f))
   ;; Safe to subtract
   (else (make-result (- a b) #t))))

;;; Safe multiplication with overflow checking
(define (safe-mul a b)
  (cond
   ;; Handle zero cases
   ((or (= a 0) (= b 0))
    (make-result 0 #t))
   ;; Compute and verify
   (else
    (let ((result (* a b)))
      (if (and (not (= a 0)) (not (= (quotient result a) b)))
          (make-result 0 #f)
          (make-result result #t))))))

;;; Safe division with zero check
(define (safe-div a b)
  (cond
   ;; Division by zero
   ((= b 0)
    (make-result 0 #f))
   ;; MIN / -1 overflow
   ((and (= a MIN-INT64) (= b -1))
    (make-result 0 #f))
   ;; Safe to divide
   (else (make-result (quotient a b) #t))))

;;; Safe modulo with zero check
(define (safe-mod a b)
  (if (= b 0)
      (make-result 0 #f)
      (make-result (remainder a b) #t)))

;;; Safe absolute value
(define (safe-abs a)
  (if (= a MIN-INT64)
      (make-result 0 #f)
      (make-result (abs a) #t)))

;;; Safe negate
(define (safe-negate a)
  (if (= a MIN-INT64)
      (make-result 0 #f)
      (make-result (- a) #t)))

;;; Clamp value to range
(define (clamp value min-val max-val)
  (cond
   ((< value min-val) min-val)
   ((> value max-val) max-val)
   (else value)))

;;; Check if value is in range
(define (in-range? value min-val max-val)
  (and (>= value min-val) (<= value max-val)))
