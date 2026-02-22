#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeUnit - FFI bindings to libproven unit conversion
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide convert-length convert-temperature
         (struct-out unit-result))

(define libproven (ffi-lib "libproven"))

;; FloatResult C struct: { i32 status, f64 value }
(define-cstruct _FloatResult
  ([status _int32]
   [value  _double]))

(struct unit-result (value ok?) #:transparent)

;; LengthUnit and TempUnit are i32 enums in the Zig FFI
(define ffi-convert-length
  (get-ffi-obj "proven_unit_convert_length" libproven
               (_fun _double _int32 _int32 -> _FloatResult)))

(define ffi-convert-temp
  (get-ffi-obj "proven_unit_convert_temp" libproven
               (_fun _double _int32 _int32 -> _FloatResult)))

;; Helper
(define (ffi-float->unit-result fr)
  (if (= (FloatResult-status fr) 0)
      (unit-result (FloatResult-value fr) #t)
      (unit-result 0.0 #f)))

;; Convert length (delegates to Idris 2)
;; from-unit/to-unit are integer enum values matching LengthUnit
(define (convert-length value from-unit to-unit)
  (ffi-float->unit-result (ffi-convert-length value from-unit to-unit)))

;; Convert temperature (delegates to Idris 2)
;; from-unit/to-unit are integer enum values matching TempUnit
(define (convert-temperature value from-unit to-unit)
  (ffi-float->unit-result (ffi-convert-temp value from-unit to-unit)))
