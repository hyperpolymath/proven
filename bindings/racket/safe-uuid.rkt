#lang racket

;; SPDX-License-Identifier: Apache-2.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeUUID - UUID validation and generation for Racket
;;

(require racket/contract
         racket/random
         racket/format)

(provide
 ;; Struct with contract
 (contract-out
  [struct uuid-value ([bytes bytes?]
                      [version uuid-version?]
                      [variant uuid-variant?])])

 ;; Version and variant symbols
 uuid-version?
 uuid-variant?

 ;; Parse and format functions
 parse-uuid
 format-uuid
 uuid->string

 ;; Validation predicates
 valid-uuid-string?
 nil-uuid?
 max-uuid?

 ;; Generation functions
 generate-uuid-v4
 generate-nil-uuid
 generate-max-uuid

 ;; Extraction functions
 uuid-time-low
 uuid-time-mid
 uuid-time-hi-and-version
 uuid-clock-seq
 uuid-node

 ;; Comparison
 uuid=?
 uuid<?
 uuid-compare)

;; ============================================================================
;; UUID Version Symbols
;; ============================================================================

(define uuid-versions
  '(unknown v1 v2 v3 v4 v5 v6 v7 v8))

(define (uuid-version? symbol)
  (and (symbol? symbol)
       (member symbol uuid-versions)
       #t))

;; ============================================================================
;; UUID Variant Symbols
;; ============================================================================

(define uuid-variants
  '(ncs rfc4122 microsoft future reserved))

(define (uuid-variant? symbol)
  (and (symbol? symbol)
       (member symbol uuid-variants)
       #t))

;; ============================================================================
;; UUID Struct
;; ============================================================================

;; UUID value struct - stores 16 bytes, version, and variant
(struct uuid-value (bytes version variant) #:transparent)

;; ============================================================================
;; Constants
;; ============================================================================

(define UUID-BYTE-LENGTH 16)
(define UUID-STRING-LENGTH 36)
(define HEX-CHARS "0123456789abcdef")

;; Nil UUID (all zeros)
(define NIL-UUID-BYTES (make-bytes UUID-BYTE-LENGTH 0))

;; Max UUID (all ones)
(define MAX-UUID-BYTES (make-bytes UUID-BYTE-LENGTH 255))

;; ============================================================================
;; Parsing Functions
;; ============================================================================

;; Check if a character is a valid hex digit
(define (hex-char? char)
  (or (char<=? #\0 char #\9)
      (char<=? #\a char #\f)
      (char<=? #\A char #\F)))

;; Convert hex character to integer
(define (hex-char->int char)
  (cond
    [(char<=? #\0 char #\9) (- (char->integer char) (char->integer #\0))]
    [(char<=? #\a char #\f) (+ 10 (- (char->integer char) (char->integer #\a)))]
    [(char<=? #\A char #\F) (+ 10 (- (char->integer char) (char->integer #\A)))]
    [else 0]))

;; Validate UUID string format (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx)
(define (valid-uuid-string? uuid-string)
  (and (string? uuid-string)
       (= (string-length uuid-string) UUID-STRING-LENGTH)
       (char=? (string-ref uuid-string 8) #\-)
       (char=? (string-ref uuid-string 13) #\-)
       (char=? (string-ref uuid-string 18) #\-)
       (char=? (string-ref uuid-string 23) #\-)
       (for/and ([i (in-range UUID-STRING-LENGTH)])
         (define char (string-ref uuid-string i))
         (or (member i '(8 13 18 23))
             (hex-char? char)))))

;; Parse hex pair at position in string
(define (parse-hex-pair uuid-string position)
  (define high-nibble (hex-char->int (string-ref uuid-string position)))
  (define low-nibble (hex-char->int (string-ref uuid-string (+ position 1))))
  (+ (* high-nibble 16) low-nibble))

;; Extract version from version byte
(define (extract-version version-byte)
  (define version-num (arithmetic-shift version-byte -4))
  (case version-num
    [(1) 'v1]
    [(2) 'v2]
    [(3) 'v3]
    [(4) 'v4]
    [(5) 'v5]
    [(6) 'v6]
    [(7) 'v7]
    [(8) 'v8]
    [else 'unknown]))

;; Extract variant from clock-seq-hi byte
(define (extract-variant clock-seq-hi-byte)
  (cond
    [(= (bitwise-and clock-seq-hi-byte #x80) #x00) 'ncs]
    [(= (bitwise-and clock-seq-hi-byte #xC0) #x80) 'rfc4122]
    [(= (bitwise-and clock-seq-hi-byte #xE0) #xC0) 'microsoft]
    [(= (bitwise-and clock-seq-hi-byte #xE0) #xE0) 'future]
    [else 'reserved]))

;; Parse UUID string into uuid-value struct
(define (parse-uuid uuid-string)
  (cond
    [(not (valid-uuid-string? uuid-string))
     #f]
    [else
     ;; Parse hex characters to bytes (skipping dashes)
     (define uuid-bytes (make-bytes UUID-BYTE-LENGTH))
     (define positions
       '(0 2 4 6            ; time-low (bytes 0-3)
         9 11               ; time-mid (bytes 4-5)
         14 16              ; time-hi-and-version (bytes 6-7)
         19 21              ; clock-seq (bytes 8-9)
         24 26 28 30 32 34)) ; node (bytes 10-15)

     (for ([byte-idx (in-range UUID-BYTE-LENGTH)]
           [str-pos (in-list positions)])
       (bytes-set! uuid-bytes byte-idx (parse-hex-pair uuid-string str-pos)))

     ;; Extract version and variant
     (define version-byte (bytes-ref uuid-bytes 6))
     (define clock-seq-hi-byte (bytes-ref uuid-bytes 8))

     (uuid-value uuid-bytes
                 (extract-version version-byte)
                 (extract-variant clock-seq-hi-byte))]))

;; ============================================================================
;; Formatting Functions
;; ============================================================================

;; Convert byte to two hex characters
(define (byte->hex-chars byte)
  (string (string-ref HEX-CHARS (arithmetic-shift byte -4))
          (string-ref HEX-CHARS (bitwise-and byte #x0F))))

;; Format UUID bytes as standard UUID string
(define (format-uuid uuid)
  (define uuid-bytes (uuid-value-bytes uuid))
  (string-append
   ;; time-low (bytes 0-3)
   (byte->hex-chars (bytes-ref uuid-bytes 0))
   (byte->hex-chars (bytes-ref uuid-bytes 1))
   (byte->hex-chars (bytes-ref uuid-bytes 2))
   (byte->hex-chars (bytes-ref uuid-bytes 3))
   "-"
   ;; time-mid (bytes 4-5)
   (byte->hex-chars (bytes-ref uuid-bytes 4))
   (byte->hex-chars (bytes-ref uuid-bytes 5))
   "-"
   ;; time-hi-and-version (bytes 6-7)
   (byte->hex-chars (bytes-ref uuid-bytes 6))
   (byte->hex-chars (bytes-ref uuid-bytes 7))
   "-"
   ;; clock-seq (bytes 8-9)
   (byte->hex-chars (bytes-ref uuid-bytes 8))
   (byte->hex-chars (bytes-ref uuid-bytes 9))
   "-"
   ;; node (bytes 10-15)
   (byte->hex-chars (bytes-ref uuid-bytes 10))
   (byte->hex-chars (bytes-ref uuid-bytes 11))
   (byte->hex-chars (bytes-ref uuid-bytes 12))
   (byte->hex-chars (bytes-ref uuid-bytes 13))
   (byte->hex-chars (bytes-ref uuid-bytes 14))
   (byte->hex-chars (bytes-ref uuid-bytes 15))))

;; Alias for format-uuid
(define uuid->string format-uuid)

;; ============================================================================
;; Validation Predicates
;; ============================================================================

;; Check if UUID is the nil UUID (all zeros)
(define (nil-uuid? uuid)
  (bytes=? (uuid-value-bytes uuid) NIL-UUID-BYTES))

;; Check if UUID is the max UUID (all ones)
(define (max-uuid? uuid)
  (bytes=? (uuid-value-bytes uuid) MAX-UUID-BYTES))

;; ============================================================================
;; Generation Functions
;; ============================================================================

;; Generate a random version 4 UUID
(define (generate-uuid-v4)
  (define uuid-bytes (crypto-random-bytes UUID-BYTE-LENGTH))

  ;; Set version to 4 (0100 in high nibble of byte 6)
  (bytes-set! uuid-bytes 6
              (bitwise-ior #x40
                           (bitwise-and (bytes-ref uuid-bytes 6) #x0F)))

  ;; Set variant to RFC 4122 (10xx in high bits of byte 8)
  (bytes-set! uuid-bytes 8
              (bitwise-ior #x80
                           (bitwise-and (bytes-ref uuid-bytes 8) #x3F)))

  (uuid-value uuid-bytes 'v4 'rfc4122))

;; Generate nil UUID
(define (generate-nil-uuid)
  (uuid-value (bytes-copy NIL-UUID-BYTES) 'unknown 'ncs))

;; Generate max UUID
(define (generate-max-uuid)
  (uuid-value (bytes-copy MAX-UUID-BYTES) 'unknown 'future))

;; ============================================================================
;; Extraction Functions
;; ============================================================================

;; Extract time-low field (first 4 bytes as 32-bit integer)
(define (uuid-time-low uuid)
  (define uuid-bytes (uuid-value-bytes uuid))
  (+ (arithmetic-shift (bytes-ref uuid-bytes 0) 24)
     (arithmetic-shift (bytes-ref uuid-bytes 1) 16)
     (arithmetic-shift (bytes-ref uuid-bytes 2) 8)
     (bytes-ref uuid-bytes 3)))

;; Extract time-mid field (bytes 4-5 as 16-bit integer)
(define (uuid-time-mid uuid)
  (define uuid-bytes (uuid-value-bytes uuid))
  (+ (arithmetic-shift (bytes-ref uuid-bytes 4) 8)
     (bytes-ref uuid-bytes 5)))

;; Extract time-hi-and-version field (bytes 6-7 as 16-bit integer)
(define (uuid-time-hi-and-version uuid)
  (define uuid-bytes (uuid-value-bytes uuid))
  (+ (arithmetic-shift (bytes-ref uuid-bytes 6) 8)
     (bytes-ref uuid-bytes 7)))

;; Extract clock-seq field (bytes 8-9 as 16-bit integer)
(define (uuid-clock-seq uuid)
  (define uuid-bytes (uuid-value-bytes uuid))
  (+ (arithmetic-shift (bytes-ref uuid-bytes 8) 8)
     (bytes-ref uuid-bytes 9)))

;; Extract node field (bytes 10-15 as 48-bit integer)
(define (uuid-node uuid)
  (define uuid-bytes (uuid-value-bytes uuid))
  (+ (arithmetic-shift (bytes-ref uuid-bytes 10) 40)
     (arithmetic-shift (bytes-ref uuid-bytes 11) 32)
     (arithmetic-shift (bytes-ref uuid-bytes 12) 24)
     (arithmetic-shift (bytes-ref uuid-bytes 13) 16)
     (arithmetic-shift (bytes-ref uuid-bytes 14) 8)
     (bytes-ref uuid-bytes 15)))

;; ============================================================================
;; Comparison Functions
;; ============================================================================

;; Check UUID equality
(define (uuid=? uuid-a uuid-b)
  (bytes=? (uuid-value-bytes uuid-a) (uuid-value-bytes uuid-b)))

;; Check if uuid-a is less than uuid-b (lexicographic byte comparison)
(define (uuid<? uuid-a uuid-b)
  (bytes<? (uuid-value-bytes uuid-a) (uuid-value-bytes uuid-b)))

;; Compare two UUIDs: returns -1, 0, or 1
(define (uuid-compare uuid-a uuid-b)
  (cond
    [(uuid=? uuid-a uuid-b) 0]
    [(uuid<? uuid-a uuid-b) -1]
    [else 1]))
