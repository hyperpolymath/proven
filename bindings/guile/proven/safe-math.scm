;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeMath - FFI bindings to libproven overflow-checked arithmetic
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-math)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (safe-add
            safe-sub
            safe-mul
            safe-div
            safe-mod
            safe-abs
            safe-negate
            clamp
            in-range?
            safe-pow
            MAX-INT64
            MIN-INT64
            make-result
            result-value
            result-ok?))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings (Zig FFI C ABI)
;;; IntResult is { i32 status, i64 value } = 16 bytes

(define ffi-math-add
  (pointer->procedure '* (dynamic-func "proven_math_add_checked" libproven)
                      (list int64 int64)))

(define ffi-math-sub
  (pointer->procedure '* (dynamic-func "proven_math_sub_checked" libproven)
                      (list int64 int64)))

(define ffi-math-mul
  (pointer->procedure '* (dynamic-func "proven_math_mul_checked" libproven)
                      (list int64 int64)))

(define ffi-math-div
  (pointer->procedure '* (dynamic-func "proven_math_div" libproven)
                      (list int64 int64)))

(define ffi-math-mod
  (pointer->procedure '* (dynamic-func "proven_math_mod" libproven)
                      (list int64 int64)))

(define ffi-math-abs
  (pointer->procedure '* (dynamic-func "proven_math_abs_safe" libproven)
                      (list int64)))

(define ffi-math-clamp
  (pointer->procedure int64 (dynamic-func "proven_math_clamp" libproven)
                      (list int64 int64 int64)))

(define ffi-math-pow
  (pointer->procedure '* (dynamic-func "proven_math_pow_checked" libproven)
                      (list int64 uint32)))

;;; Constants
(define MAX-INT64 9223372036854775807)
(define MIN-INT64 -9223372036854775808)

;;; Result constructor (mirrors IntResult from FFI)
(define (make-result value ok)
  `((value . ,value) (ok . ,ok)))

(define (result-value result)
  (assoc-ref result 'value))

(define (result-ok? result)
  (assoc-ref result 'ok))

;;; Helper: parse IntResult struct from FFI return (status: i32, value: i64)
(define (parse-int-result ptr)
  (let* ((bv (pointer->bytevector ptr 16))
         (status (bytevector-s32-native-ref bv 0))
         (value (bytevector-s64-native-ref bv 8)))
    (if (= status 0)
        (make-result value #t)
        (make-result 0 #f))))

;;; Safe addition with overflow checking (delegates to Idris 2)
(define (safe-add a b)
  (parse-int-result (ffi-math-add a b)))

;;; Safe subtraction with overflow checking (delegates to Idris 2)
(define (safe-sub a b)
  (parse-int-result (ffi-math-sub a b)))

;;; Safe multiplication with overflow checking (delegates to Idris 2)
(define (safe-mul a b)
  (parse-int-result (ffi-math-mul a b)))

;;; Safe division with zero check (delegates to Idris 2)
(define (safe-div a b)
  (parse-int-result (ffi-math-div a b)))

;;; Safe modulo with zero check (delegates to Idris 2)
(define (safe-mod a b)
  (parse-int-result (ffi-math-mod a b)))

;;; Safe absolute value (delegates to Idris 2)
(define (safe-abs a)
  (parse-int-result (ffi-math-abs a)))

;;; Safe negate -- negate via subtraction from zero (delegates to Idris 2)
(define (safe-negate a)
  (parse-int-result (ffi-math-sub 0 a)))

;;; Clamp value to range (delegates to Idris 2)
(define (clamp value min-val max-val)
  (ffi-math-clamp min-val max-val value))

;;; Check if value is in range
(define (in-range? value min-val max-val)
  (= value (clamp value min-val max-val)))

;;; Safe exponentiation (delegates to Idris 2)
(define (safe-pow base exp)
  (parse-int-result (ffi-math-pow base exp)))
