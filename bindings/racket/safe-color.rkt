#lang racket/base

;; SPDX-License-Identifier: Apache-2.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeColor - Safe color operations for Racket
;;

(require racket/string
         racket/match
         racket/format
         racket/math)

(provide
 ;; Color structs
 (struct-out rgb-color)
 (struct-out hsl-color)
 (struct-out hsv-color)

 ;; Parsing
 parse-hex
 parse-rgb
 parse-hsl

 ;; Conversion
 rgb->hex
 rgb->hsl
 rgb->hsv
 hsl->rgb
 hsv->rgb

 ;; Color operations
 lighten
 darken
 saturate
 desaturate
 invert
 grayscale
 mix-colors

 ;; Validation
 valid-hex?
 valid-rgb?
 valid-hsl?

 ;; Utilities
 luminance
 contrast-ratio
 wcag-aa?
 wcag-aaa?)

;; Color structures
(struct rgb-color (red green blue alpha) #:transparent)
(struct hsl-color (hue saturation lightness alpha) #:transparent)
(struct hsv-color (hue saturation value alpha) #:transparent)

;; Result struct
(struct color-result (value ok? error) #:transparent)

;; Clamp value to range
(define (clamp-byte v)
  (max 0 (min 255 (round v))))

(define (clamp-unit v)
  (max 0.0 (min 1.0 v)))

(define (clamp-degrees v)
  (modulo v 360))

;; Validate hex color
(define (valid-hex? str)
  (and (string? str)
       (regexp-match? #rx"^#?([0-9a-fA-F]{3}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$" str)))

;; Validate RGB values
(define (valid-rgb? r g b [a 1.0])
  (and (integer? r) (integer? g) (integer? b)
       (<= 0 r 255) (<= 0 g 255) (<= 0 b 255)
       (real? a) (<= 0 a 1)))

;; Validate HSL values
(define (valid-hsl? h s l [a 1.0])
  (and (real? h) (real? s) (real? l)
       (<= 0 h 360) (<= 0 s 1) (<= 0 l 1)
       (real? a) (<= 0 a 1)))

;; Parse hex color string to RGB
(define (parse-hex str)
  (let* ([clean (if (string-prefix? str "#")
                    (substring str 1)
                    str)]
         [len (string-length clean)])
    (cond
      [(not (valid-hex? str))
       (color-result #f #f "Invalid hex color")]
      [(= len 3)
       (let* ([r (string->number (make-string 2 (string-ref clean 0)) 16)]
              [g (string->number (make-string 2 (string-ref clean 1)) 16)]
              [b (string->number (make-string 2 (string-ref clean 2)) 16)])
         (color-result (rgb-color r g b 1.0) #t #f))]
      [(= len 6)
       (let* ([r (string->number (substring clean 0 2) 16)]
              [g (string->number (substring clean 2 4) 16)]
              [b (string->number (substring clean 4 6) 16)])
         (color-result (rgb-color r g b 1.0) #t #f))]
      [(= len 8)
       (let* ([r (string->number (substring clean 0 2) 16)]
              [g (string->number (substring clean 2 4) 16)]
              [b (string->number (substring clean 4 6) 16)]
              [a (/ (string->number (substring clean 6 8) 16) 255.0)])
         (color-result (rgb-color r g b a) #t #f))]
      [else (color-result #f #f "Invalid hex length")])))

;; Parse rgb/rgba string
(define (parse-rgb str)
  (let ([match (regexp-match
                #rx"^rgba?\\s*\\(\\s*([0-9]+)\\s*,\\s*([0-9]+)\\s*,\\s*([0-9]+)\\s*(,\\s*([0-9.]+))?\\s*\\)$"
                str)])
    (if match
        (let* ([r (string->number (list-ref match 1))]
               [g (string->number (list-ref match 2))]
               [b (string->number (list-ref match 3))]
               [a (if (list-ref match 5)
                      (string->number (list-ref match 5))
                      1.0)])
          (if (valid-rgb? r g b a)
              (color-result (rgb-color r g b a) #t #f)
              (color-result #f #f "RGB values out of range")))
        (color-result #f #f "Invalid RGB format"))))

;; Parse hsl/hsla string
(define (parse-hsl str)
  (let ([match (regexp-match
                #rx"^hsla?\\s*\\(\\s*([0-9.]+)\\s*,\\s*([0-9.]+)%?\\s*,\\s*([0-9.]+)%?\\s*(,\\s*([0-9.]+))?\\s*\\)$"
                str)])
    (if match
        (let* ([h (string->number (list-ref match 1))]
               [s (/ (string->number (list-ref match 2)) 100.0)]
               [l (/ (string->number (list-ref match 3)) 100.0)]
               [a (if (list-ref match 5)
                      (string->number (list-ref match 5))
                      1.0)])
          (if (valid-hsl? h s l a)
              (color-result (hsl-color h s l a) #t #f)
              (color-result #f #f "HSL values out of range")))
        (color-result #f #f "Invalid HSL format"))))

;; Convert RGB to hex string
(define (rgb->hex color)
  (format "#~a~a~a"
          (let ([hex (number->string (rgb-color-red color) 16)])
            (if (= (string-length hex) 1) (string-append "0" hex) hex))
          (let ([hex (number->string (rgb-color-green color) 16)])
            (if (= (string-length hex) 1) (string-append "0" hex) hex))
          (let ([hex (number->string (rgb-color-blue color) 16)])
            (if (= (string-length hex) 1) (string-append "0" hex) hex))))

;; Convert RGB to HSL
(define (rgb->hsl color)
  (let* ([r (/ (rgb-color-red color) 255.0)]
         [g (/ (rgb-color-green color) 255.0)]
         [b (/ (rgb-color-blue color) 255.0)]
         [max-c (max r g b)]
         [min-c (min r g b)]
         [l (/ (+ max-c min-c) 2.0)]
         [delta (- max-c min-c)])
    (if (= delta 0)
        (hsl-color 0 0 l (rgb-color-alpha color))
        (let* ([s (if (> l 0.5)
                      (/ delta (- 2.0 max-c min-c))
                      (/ delta (+ max-c min-c)))]
               [h (cond
                    [(= max-c r) (modulo (* 60 (/ (- g b) delta)) 360)]
                    [(= max-c g) (+ 120 (* 60 (/ (- b r) delta)))]
                    [else (+ 240 (* 60 (/ (- r g) delta)))])])
          (hsl-color h s l (rgb-color-alpha color))))))

;; Convert RGB to HSV
(define (rgb->hsv color)
  (let* ([r (/ (rgb-color-red color) 255.0)]
         [g (/ (rgb-color-green color) 255.0)]
         [b (/ (rgb-color-blue color) 255.0)]
         [max-c (max r g b)]
         [min-c (min r g b)]
         [delta (- max-c min-c)]
         [v max-c]
         [s (if (= max-c 0) 0 (/ delta max-c))])
    (if (= delta 0)
        (hsv-color 0 s v (rgb-color-alpha color))
        (let ([h (cond
                   [(= max-c r) (modulo (* 60 (/ (- g b) delta)) 360)]
                   [(= max-c g) (+ 120 (* 60 (/ (- b r) delta)))]
                   [else (+ 240 (* 60 (/ (- r g) delta)))])])
          (hsv-color h s v (rgb-color-alpha color))))))

;; Helper for HSL to RGB
(define (hue-to-rgb p q t)
  (let ([t (cond [(< t 0) (+ t 1)]
                 [(> t 1) (- t 1)]
                 [else t])])
    (cond
      [(< t (/ 1.0 6)) (+ p (* (- q p) 6 t))]
      [(< t 0.5) q]
      [(< t (/ 2.0 3)) (+ p (* (- q p) (- (/ 2.0 3) t) 6))]
      [else p])))

;; Convert HSL to RGB
(define (hsl->rgb color)
  (let* ([h (/ (hsl-color-hue color) 360.0)]
         [s (hsl-color-saturation color)]
         [l (hsl-color-lightness color)])
    (if (= s 0)
        (let ([gray (clamp-byte (* l 255))])
          (rgb-color gray gray gray (hsl-color-alpha color)))
        (let* ([q (if (< l 0.5)
                      (* l (+ 1 s))
                      (- (+ l s) (* l s)))]
               [p (- (* 2 l) q)]
               [r (clamp-byte (* 255 (hue-to-rgb p q (+ h (/ 1.0 3)))))]
               [g (clamp-byte (* 255 (hue-to-rgb p q h)))]
               [b (clamp-byte (* 255 (hue-to-rgb p q (- h (/ 1.0 3)))))])
          (rgb-color r g b (hsl-color-alpha color))))))

;; Convert HSV to RGB
(define (hsv->rgb color)
  (let* ([h (/ (hsv-color-hue color) 60.0)]
         [s (hsv-color-saturation color)]
         [v (hsv-color-value color)]
         [c (* v s)]
         [x (* c (- 1 (abs (- (modulo h 2) 1))))]
         [m (- v c)])
    (let-values ([(r1 g1 b1)
                  (cond
                    [(< h 1) (values c x 0)]
                    [(< h 2) (values x c 0)]
                    [(< h 3) (values 0 c x)]
                    [(< h 4) (values 0 x c)]
                    [(< h 5) (values x 0 c)]
                    [else (values c 0 x)])])
      (rgb-color (clamp-byte (* 255 (+ r1 m)))
                 (clamp-byte (* 255 (+ g1 m)))
                 (clamp-byte (* 255 (+ b1 m)))
                 (hsv-color-alpha color)))))

;; Lighten color by amount (0-1)
(define (lighten color amount)
  (let ([hsl (rgb->hsl color)])
    (hsl->rgb (hsl-color (hsl-color-hue hsl)
                         (hsl-color-saturation hsl)
                         (clamp-unit (+ (hsl-color-lightness hsl) amount))
                         (hsl-color-alpha hsl)))))

;; Darken color by amount (0-1)
(define (darken color amount)
  (lighten color (- amount)))

;; Saturate color by amount (0-1)
(define (saturate color amount)
  (let ([hsl (rgb->hsl color)])
    (hsl->rgb (hsl-color (hsl-color-hue hsl)
                         (clamp-unit (+ (hsl-color-saturation hsl) amount))
                         (hsl-color-lightness hsl)
                         (hsl-color-alpha hsl)))))

;; Desaturate color by amount (0-1)
(define (desaturate color amount)
  (saturate color (- amount)))

;; Invert color
(define (invert color)
  (rgb-color (- 255 (rgb-color-red color))
             (- 255 (rgb-color-green color))
             (- 255 (rgb-color-blue color))
             (rgb-color-alpha color)))

;; Convert to grayscale
(define (grayscale color)
  (let ([gray (clamp-byte
               (+ (* 0.2126 (rgb-color-red color))
                  (* 0.7152 (rgb-color-green color))
                  (* 0.0722 (rgb-color-blue color))))])
    (rgb-color gray gray gray (rgb-color-alpha color))))

;; Mix two colors
(define (mix-colors c1 c2 [weight 0.5])
  (let ([w (clamp-unit weight)])
    (rgb-color (clamp-byte (+ (* (rgb-color-red c1) (- 1 w))
                               (* (rgb-color-red c2) w)))
               (clamp-byte (+ (* (rgb-color-green c1) (- 1 w))
                               (* (rgb-color-green c2) w)))
               (clamp-byte (+ (* (rgb-color-blue c1) (- 1 w))
                               (* (rgb-color-blue c2) w)))
               (+ (* (rgb-color-alpha c1) (- 1 w))
                  (* (rgb-color-alpha c2) w)))))

;; Calculate relative luminance
(define (luminance color)
  (define (channel-lum c)
    (let ([v (/ c 255.0)])
      (if (<= v 0.03928)
          (/ v 12.92)
          (expt (/ (+ v 0.055) 1.055) 2.4))))
  (+ (* 0.2126 (channel-lum (rgb-color-red color)))
     (* 0.7152 (channel-lum (rgb-color-green color)))
     (* 0.0722 (channel-lum (rgb-color-blue color)))))

;; Calculate contrast ratio
(define (contrast-ratio c1 c2)
  (let* ([l1 (luminance c1)]
         [l2 (luminance c2)]
         [lighter (max l1 l2)]
         [darker (min l1 l2)])
    (/ (+ lighter 0.05) (+ darker 0.05))))

;; WCAG AA compliance (4.5:1 for normal text)
(define (wcag-aa? c1 c2)
  (>= (contrast-ratio c1 c2) 4.5))

;; WCAG AAA compliance (7:1 for normal text)
(define (wcag-aaa? c1 c2)
  (>= (contrast-ratio c1 c2) 7.0))
