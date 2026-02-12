#lang racket

;; SPDX-License-Identifier: Apache-2.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeHex - Hexadecimal encoding/decoding for Racket
;;

(require racket/contract
         racket/format)

(provide
 ;; Result struct with contract
 (contract-out
  [struct hex-result ([value (or/c bytes? string? #f)]
                      [ok? boolean?]
                      [error (or/c string? #f)])])

 ;; Encode functions
 hex-encode
 hex-encode-upper
 hex-encode-string
 bytes->hex-string

 ;; Decode functions
 hex-decode
 hex-decode-string
 hex-string->bytes

 ;; Validation predicates
 valid-hex-string?
 valid-hex-char?
 hex-length-valid?

 ;; Constant-time comparison (security-critical)
 constant-time-equal?
 hex-constant-time-equal?

 ;; Utility functions
 hex-char->int
 int->hex-char
 normalize-hex
 strip-hex-prefix

 ;; Formatting
 format-hex
 format-hex-grouped)

;; ============================================================================
;; Constants
;; ============================================================================

(define HEX-CHARS-LOWER "0123456789abcdef")
(define HEX-CHARS-UPPER "0123456789ABCDEF")

;; ============================================================================
;; Result Struct
;; ============================================================================

;; Result struct for operations that may fail
(struct hex-result (value ok? error) #:transparent)

;; ============================================================================
;; Character Validation and Conversion
;; ============================================================================

;; Check if character is a valid hex digit
(define (valid-hex-char? char)
  (or (char<=? #\0 char #\9)
      (char<=? #\a char #\f)
      (char<=? #\A char #\F)))

;; Convert hex character to integer (0-15)
(define (hex-char->int char)
  (cond
    [(char<=? #\0 char #\9) (- (char->integer char) (char->integer #\0))]
    [(char<=? #\a char #\f) (+ 10 (- (char->integer char) (char->integer #\a)))]
    [(char<=? #\A char #\F) (+ 10 (- (char->integer char) (char->integer #\A)))]
    [else -1]))

;; Convert integer (0-15) to hex character (lowercase)
(define (int->hex-char value [uppercase? #f])
  (cond
    [(and (>= value 0) (<= value 15))
     (string-ref (if uppercase? HEX-CHARS-UPPER HEX-CHARS-LOWER) value)]
    [else #f]))

;; ============================================================================
;; String Validation
;; ============================================================================

;; Check if string is valid hexadecimal
(define (valid-hex-string? hex-string)
  (and (string? hex-string)
       (even? (string-length hex-string))
       (for/and ([char (in-string hex-string)])
         (valid-hex-char? char))))

;; Check if hex length is valid (must be even)
(define (hex-length-valid? hex-string)
  (even? (string-length hex-string)))

;; Strip 0x or 0X prefix if present
(define (strip-hex-prefix hex-string)
  (cond
    [(string-prefix? hex-string "0x") (substring hex-string 2)]
    [(string-prefix? hex-string "0X") (substring hex-string 2)]
    [else hex-string]))

;; Normalize hex string (lowercase, strip prefix)
(define (normalize-hex hex-string)
  (string-downcase (strip-hex-prefix hex-string)))

;; ============================================================================
;; Encoding Functions
;; ============================================================================

;; Encode bytes to lowercase hex string
(define (hex-encode data)
  (cond
    [(bytes? data)
     (hex-result (bytes->hex-string data) #t #f)]
    [(string? data)
     (hex-result (bytes->hex-string (string->bytes/utf-8 data)) #t #f)]
    [else
     (hex-result #f #f "Input must be bytes or string")]))

;; Encode bytes to uppercase hex string
(define (hex-encode-upper data)
  (cond
    [(bytes? data)
     (hex-result (bytes->hex-string-case data #t) #t #f)]
    [(string? data)
     (hex-result (bytes->hex-string-case (string->bytes/utf-8 data) #t) #t #f)]
    [else
     (hex-result #f #f "Input must be bytes or string")]))

;; Encode string to hex (convenience wrapper)
(define (hex-encode-string str)
  (hex-encode (string->bytes/utf-8 str)))

;; Convert bytes to hex string (lowercase, direct function)
(define (bytes->hex-string bstr)
  (bytes->hex-string-case bstr #f))

;; Convert bytes to hex string with case option
(define (bytes->hex-string-case bstr uppercase?)
  (define hex-chars (if uppercase? HEX-CHARS-UPPER HEX-CHARS-LOWER))
  (define len (bytes-length bstr))
  (define result (make-string (* len 2)))

  (for ([i (in-range len)])
    (define byte-val (bytes-ref bstr i))
    (define high-nibble (arithmetic-shift byte-val -4))
    (define low-nibble (bitwise-and byte-val #x0F))
    (string-set! result (* i 2) (string-ref hex-chars high-nibble))
    (string-set! result (+ (* i 2) 1) (string-ref hex-chars low-nibble)))

  result)

;; ============================================================================
;; Decoding Functions
;; ============================================================================

;; Decode hex string to bytes
(define (hex-decode hex-string)
  (define normalized (strip-hex-prefix hex-string))

  (cond
    ;; Empty string
    [(= (string-length normalized) 0)
     (hex-result #"" #t #f)]

    ;; Odd length
    [(odd? (string-length normalized))
     (hex-result #f #f "Hex string must have even length")]

    ;; Check for invalid characters
    [(not (for/and ([char (in-string normalized)])
            (valid-hex-char? char)))
     (hex-result #f #f "Invalid hex character in input")]

    ;; Valid hex - decode
    [else
     (define byte-len (quotient (string-length normalized) 2))
     (define result (make-bytes byte-len))

     (for ([i (in-range byte-len)])
       (define high-char (string-ref normalized (* i 2)))
       (define low-char (string-ref normalized (+ (* i 2) 1)))
       (define high-nibble (hex-char->int high-char))
       (define low-nibble (hex-char->int low-char))
       (bytes-set! result i (+ (* high-nibble 16) low-nibble)))

     (hex-result result #t #f)]))

;; Decode hex string to UTF-8 string
(define (hex-decode-string hex-string)
  (define decoded (hex-decode hex-string))
  (cond
    [(not (hex-result-ok? decoded))
     decoded]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (hex-result #f #f "Decoded bytes are not valid UTF-8"))])
       (hex-result (bytes->string/utf-8 (hex-result-value decoded)) #t #f))]))

;; Direct function: hex string to bytes (returns #f on error)
(define (hex-string->bytes hex-string)
  (define result (hex-decode hex-string))
  (and (hex-result-ok? result) (hex-result-value result)))

;; ============================================================================
;; Constant-Time Comparison (Security-Critical)
;; ============================================================================

;; Constant-time byte comparison to prevent timing attacks
;; IMPORTANT: Both byte strings should be the same length for security
(define (constant-time-equal? a b)
  (cond
    ;; Handle bytes
    [(and (bytes? a) (bytes? b))
     (constant-time-bytes-equal? a b)]

    ;; Handle strings
    [(and (string? a) (string? b))
     (constant-time-bytes-equal?
      (string->bytes/utf-8 a)
      (string->bytes/utf-8 b))]

    ;; Type mismatch
    [else #f]))

;; Internal: constant-time bytes comparison
(define (constant-time-bytes-equal? a b)
  (define len-a (bytes-length a))
  (define len-b (bytes-length b))

  ;; Length check (still reveals length difference, but this is standard practice)
  (cond
    [(not (= len-a len-b)) #f]
    [else
     ;; XOR all bytes and accumulate differences
     ;; This ensures we always compare ALL bytes regardless of early differences
     (define diff
       (for/fold ([accumulated-diff 0])
                 ([byte-a (in-bytes a)]
                  [byte-b (in-bytes b)])
         (bitwise-ior accumulated-diff
                      (bitwise-xor byte-a byte-b))))
     (= diff 0)]))

;; Compare two hex strings in constant time
(define (hex-constant-time-equal? hex-a hex-b)
  (define decoded-a (hex-decode hex-a))
  (define decoded-b (hex-decode hex-b))

  (cond
    [(not (hex-result-ok? decoded-a)) #f]
    [(not (hex-result-ok? decoded-b)) #f]
    [else
     (constant-time-bytes-equal?
      (hex-result-value decoded-a)
      (hex-result-value decoded-b))]))

;; ============================================================================
;; Formatting Functions
;; ============================================================================

;; Format hex string with optional prefix
(define (format-hex hex-string
                    #:prefix [prefix ""]
                    #:uppercase? [uppercase? #f])
  (define normalized (strip-hex-prefix hex-string))
  (define cased (if uppercase?
                    (string-upcase normalized)
                    (string-downcase normalized)))
  (string-append prefix cased))

;; Format hex string with grouping (e.g., "aa bb cc dd")
(define (format-hex-grouped hex-string
                            #:group-size [group-size 2]
                            #:separator [separator " "]
                            #:uppercase? [uppercase? #f])
  (define normalized (strip-hex-prefix hex-string))

  (cond
    [(= (string-length normalized) 0) ""]
    [else
     (define cased (if uppercase?
                       (string-upcase normalized)
                       (string-downcase normalized)))

     ;; Split into groups
     (define groups
       (for/list ([i (in-range 0 (string-length cased) group-size)])
         (substring cased i (min (+ i group-size) (string-length cased)))))

     (string-join groups separator)]))
