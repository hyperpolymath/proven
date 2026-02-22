#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeFloat - FFI bindings to libproven float operations
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide safe-float-div float-is-finite? float-is-nan?
         safe-float-sqrt safe-float-ln
         (struct-out float-result))

(define libproven (ffi-lib "libproven"))

;; FloatResult C struct: { i32 status, f64 value }
(define-cstruct _FloatResult
  ([status _int32]
   [value  _double]))

(struct float-result (value ok? error) #:transparent)

(define ffi-float-div
  (get-ffi-obj "proven_float_div" libproven (_fun _double _double -> _FloatResult)))
(define ffi-float-is-finite
  (get-ffi-obj "proven_float_is_finite" libproven (_fun _double -> _bool)))
(define ffi-float-is-nan
  (get-ffi-obj "proven_float_is_nan" libproven (_fun _double -> _bool)))
(define ffi-float-sqrt
  (get-ffi-obj "proven_float_sqrt" libproven (_fun _double -> _FloatResult)))
(define ffi-float-ln
  (get-ffi-obj "proven_float_ln" libproven (_fun _double -> _FloatResult)))

;; Helper: convert FloatResult to float-result
(define (ffi-float->result fr)
  (if (= (FloatResult-status fr) 0)
      (float-result (FloatResult-value fr) #t #f)
      (float-result 0.0 #f "FFI error")))

;; Safe float division (delegates to Idris 2)
(define (safe-float-div a b) (ffi-float->result (ffi-float-div a b)))

;; Check if finite (delegates to Idris 2)
(define (float-is-finite? x) (ffi-float-is-finite x))

;; Check if NaN (delegates to Idris 2)
(define (float-is-nan? x) (ffi-float-is-nan x))

;; Safe square root (delegates to Idris 2)
(define (safe-float-sqrt x) (ffi-float->result (ffi-float-sqrt x)))

;; Safe natural logarithm (delegates to Idris 2)
(define (safe-float-ln x) (ffi-float->result (ffi-float-ln x)))
