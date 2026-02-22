#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeMath - FFI bindings to libproven overflow-checked arithmetic
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide safe-add safe-sub safe-mul safe-div safe-mod
         safe-abs safe-negate safe-pow clamp in-range?
         (struct-out safe-result) MAX-INT64 MIN-INT64)

;; Load libproven
(define libproven (ffi-lib "libproven"))

;; IntResult C struct: { i32 status, i64 value }
(define-cstruct _IntResult
  ([status _int32]
   [value  _int64]))

;; Constants
(define MAX-INT64 9223372036854775807)
(define MIN-INT64 -9223372036854775808)

;; Result struct for Racket consumers
(struct safe-result (value ok?) #:transparent)

;; FFI bindings
(define ffi-math-add
  (get-ffi-obj "proven_math_add_checked" libproven (_fun _int64 _int64 -> _IntResult)))
(define ffi-math-sub
  (get-ffi-obj "proven_math_sub_checked" libproven (_fun _int64 _int64 -> _IntResult)))
(define ffi-math-mul
  (get-ffi-obj "proven_math_mul_checked" libproven (_fun _int64 _int64 -> _IntResult)))
(define ffi-math-div
  (get-ffi-obj "proven_math_div" libproven (_fun _int64 _int64 -> _IntResult)))
(define ffi-math-mod
  (get-ffi-obj "proven_math_mod" libproven (_fun _int64 _int64 -> _IntResult)))
(define ffi-math-abs
  (get-ffi-obj "proven_math_abs_safe" libproven (_fun _int64 -> _IntResult)))
(define ffi-math-clamp
  (get-ffi-obj "proven_math_clamp" libproven (_fun _int64 _int64 _int64 -> _int64)))
(define ffi-math-pow
  (get-ffi-obj "proven_math_pow_checked" libproven (_fun _int64 _uint32 -> _IntResult)))

;; Helper: convert IntResult to safe-result
(define (int-result->safe-result ir)
  (if (= (IntResult-status ir) 0)
      (safe-result (IntResult-value ir) #t)
      (safe-result 0 #f)))

;; Public API (all delegate to Idris 2)
(define (safe-add a b) (int-result->safe-result (ffi-math-add a b)))
(define (safe-sub a b) (int-result->safe-result (ffi-math-sub a b)))
(define (safe-mul a b) (int-result->safe-result (ffi-math-mul a b)))
(define (safe-div a b) (int-result->safe-result (ffi-math-div a b)))
(define (safe-mod a b) (int-result->safe-result (ffi-math-mod a b)))
(define (safe-abs a)   (int-result->safe-result (ffi-math-abs a)))
(define (safe-negate a) (int-result->safe-result (ffi-math-sub 0 a)))
(define (safe-pow base exp) (int-result->safe-result (ffi-math-pow base exp)))
(define (clamp value min-val max-val) (ffi-math-clamp min-val max-val value))
(define (in-range? value min-val max-val) (= value (clamp value min-val max-val)))
