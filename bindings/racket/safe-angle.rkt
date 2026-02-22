#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeAngle - FFI bindings to libproven angle operations
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide degrees->radians radians->degrees
         normalize-degrees normalize-radians)

(define libproven (ffi-lib "libproven"))

(define ffi-deg-to-rad
  (get-ffi-obj "proven_angle_deg_to_rad" libproven (_fun _double -> _double)))
(define ffi-rad-to-deg
  (get-ffi-obj "proven_angle_rad_to_deg" libproven (_fun _double -> _double)))
(define ffi-normalize-deg
  (get-ffi-obj "proven_angle_normalize_degrees" libproven (_fun _double -> _double)))
(define ffi-normalize-rad
  (get-ffi-obj "proven_angle_normalize_radians" libproven (_fun _double -> _double)))

;; All delegate to Idris 2
(define (degrees->radians deg) (ffi-deg-to-rad deg))
(define (radians->degrees rad) (ffi-rad-to-deg rad))
(define (normalize-degrees deg) (ffi-normalize-deg deg))
(define (normalize-radians rad) (ffi-normalize-rad rad))
