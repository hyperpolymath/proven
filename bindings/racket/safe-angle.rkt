#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeAngle - Safe angle operations for Racket
;;

(require racket/math)

(provide
 ;; Constants
 PI
 TWO-PI
 HALF-PI
 DEGREES-TO-RADIANS
 RADIANS-TO-DEGREES

 ;; Conversion
 degrees->radians
 radians->degrees

 ;; Normalization
 normalize-degrees
 normalize-radians

 ;; Trigonometry (with normalization)
 safe-sin
 safe-cos
 safe-tan
 safe-asin
 safe-acos
 safe-atan
 safe-atan2

 ;; Angular operations
 angle-difference
 angle-midpoint
 angle-lerp

 ;; Validation
 valid-angle-degrees?
 valid-angle-radians?

 ;; Common angles
 ANGLE-0
 ANGLE-90
 ANGLE-180
 ANGLE-270
 ANGLE-360)

;; Constants
(define PI pi)
(define TWO-PI (* 2 pi))
(define HALF-PI (/ pi 2))
(define DEGREES-TO-RADIANS (/ pi 180))
(define RADIANS-TO-DEGREES (/ 180 pi))

;; Common angle values in degrees
(define ANGLE-0 0)
(define ANGLE-90 90)
(define ANGLE-180 180)
(define ANGLE-270 270)
(define ANGLE-360 360)

;; Validate degrees value
(define (valid-angle-degrees? deg)
  (and (real? deg) (not (infinite? deg)) (not (nan? deg))))

;; Validate radians value
(define (valid-angle-radians? rad)
  (and (real? rad) (not (infinite? rad)) (not (nan? rad))))

;; Convert degrees to radians
(define (degrees->radians deg)
  (* deg DEGREES-TO-RADIANS))

;; Convert radians to degrees
(define (radians->degrees rad)
  (* rad RADIANS-TO-DEGREES))

;; Normalize degrees to [0, 360)
(define (normalize-degrees deg)
  (let ([result (modulo deg 360)])
    (if (< result 0)
        (+ result 360)
        result)))

;; Normalize radians to [0, 2*pi)
(define (normalize-radians rad)
  (let ([result (modulo rad TWO-PI)])
    (if (< result 0)
        (+ result TWO-PI)
        result)))

;; Safe sine (normalizes input)
(define (safe-sin angle-rad)
  (sin (normalize-radians angle-rad)))

;; Safe cosine (normalizes input)
(define (safe-cos angle-rad)
  (cos (normalize-radians angle-rad)))

;; Safe tangent (handles undefined points)
(define (safe-tan angle-rad)
  (let* ([normalized (normalize-radians angle-rad)]
         [cos-val (cos normalized)])
    (if (< (abs cos-val) 1e-10)
        +inf.0  ; Return infinity for undefined points
        (/ (sin normalized) cos-val))))

;; Safe arcsine (clamps input to [-1, 1])
(define (safe-asin x)
  (asin (max -1.0 (min 1.0 x))))

;; Safe arccosine (clamps input to [-1, 1])
(define (safe-acos x)
  (acos (max -1.0 (min 1.0 x))))

;; Safe arctangent
(define (safe-atan x)
  (atan x))

;; Safe atan2 (handles special cases)
(define (safe-atan2 y x)
  (cond
    [(and (= x 0) (= y 0)) 0]
    [(and (= x 0) (> y 0)) HALF-PI]
    [(and (= x 0) (< y 0)) (- HALF-PI)]
    [else (atan y x)]))

;; Calculate shortest angle difference between two angles (in radians)
(define (angle-difference from to)
  (let* ([diff (- (normalize-radians to) (normalize-radians from))])
    (cond
      [(> diff PI) (- diff TWO-PI)]
      [(< diff (- PI)) (+ diff TWO-PI)]
      [else diff])))

;; Find midpoint between two angles (shortest path)
(define (angle-midpoint a b)
  (let ([diff (angle-difference a b)])
    (normalize-radians (+ a (/ diff 2)))))

;; Linear interpolation between two angles (shortest path)
(define (angle-lerp from to t)
  (let* ([clamped-t (max 0.0 (min 1.0 t))]
         [diff (angle-difference from to)])
    (normalize-radians (+ from (* diff clamped-t)))))
