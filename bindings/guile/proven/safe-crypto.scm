;;; SPDX-License-Identifier: MPL-2.0-or-later
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeCrypto - Safe cryptographic validation for Guile Scheme
;;;
;;; Provides hash validation, key format checking, and safe comparisons.
;;; All operations are pure and cannot crash.

(define-module (proven safe-crypto)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (;; Hash validation
            hash-valid-md5?
            hash-valid-sha1?
            hash-valid-sha256?
            hash-valid-sha384?
            hash-valid-sha512?
            hash-valid-blake2b?
            hash-valid-blake2s?
            hash-detect-algorithm

            ;; Key format validation
            key-valid-base64?
            key-valid-hex?
            key-valid-pem?
            key-valid-jwk?
            key-length-ok?

            ;; Safe comparison
            constant-time-equals?

            ;; Hash utilities
            hash-normalize
            hash-format

            ;; Algorithm constants
            *md5-length*
            *sha1-length*
            *sha256-length*
            *sha384-length*
            *sha512-length*
            *blake2b-length*
            *blake2s-length*))

;;; Hash length constants (in hex characters)
(define *md5-length* 32)
(define *sha1-length* 40)
(define *sha256-length* 64)
(define *sha384-length* 96)
(define *sha512-length* 128)
(define *blake2b-length* 128)
(define *blake2s-length* 64)

;;; Check if character is valid hex
(define (hex-char? char-code)
  (or (and (>= char-code 48) (<= char-code 57))   ; 0-9
      (and (>= char-code 65) (<= char-code 70))   ; A-F
      (and (>= char-code 97) (<= char-code 102)))) ; a-f

;;; Check if string is valid hex
(define (valid-hex-string? str)
  (and (string? str)
       (> (string-length str) 0)
       (every (lambda (c) (hex-char? (char->integer c)))
              (string->list str))))

;;; Check if string is valid base64
(define (base64-char? char-code)
  (or (and (>= char-code 65) (<= char-code 90))   ; A-Z
      (and (>= char-code 97) (<= char-code 122))  ; a-z
      (and (>= char-code 48) (<= char-code 57))   ; 0-9
      (= char-code 43)   ; +
      (= char-code 47)   ; /
      (= char-code 61))) ; =

;;; Validate MD5 hash
(define (hash-valid-md5? hash-string)
  (and (string? hash-string)
       (= (string-length hash-string) *md5-length*)
       (valid-hex-string? hash-string)))

;;; Validate SHA1 hash
(define (hash-valid-sha1? hash-string)
  (and (string? hash-string)
       (= (string-length hash-string) *sha1-length*)
       (valid-hex-string? hash-string)))

;;; Validate SHA256 hash
(define (hash-valid-sha256? hash-string)
  (and (string? hash-string)
       (= (string-length hash-string) *sha256-length*)
       (valid-hex-string? hash-string)))

;;; Validate SHA384 hash
(define (hash-valid-sha384? hash-string)
  (and (string? hash-string)
       (= (string-length hash-string) *sha384-length*)
       (valid-hex-string? hash-string)))

;;; Validate SHA512 hash
(define (hash-valid-sha512? hash-string)
  (and (string? hash-string)
       (= (string-length hash-string) *sha512-length*)
       (valid-hex-string? hash-string)))

;;; Validate BLAKE2b hash
(define (hash-valid-blake2b? hash-string)
  (and (string? hash-string)
       (= (string-length hash-string) *blake2b-length*)
       (valid-hex-string? hash-string)))

;;; Validate BLAKE2s hash
(define (hash-valid-blake2s? hash-string)
  (and (string? hash-string)
       (= (string-length hash-string) *blake2s-length*)
       (valid-hex-string? hash-string)))

;;; Detect hash algorithm based on length
(define (hash-detect-algorithm hash-string)
  (if (not (valid-hex-string? hash-string))
      #f
      (case (string-length hash-string)
        ((32) 'md5)
        ((40) 'sha1)
        ((64) 'sha256-or-blake2s)
        ((96) 'sha384)
        ((128) 'sha512-or-blake2b)
        (else 'unknown))))

;;; Check if string is valid base64
(define (key-valid-base64? key-string)
  (and (string? key-string)
       (> (string-length key-string) 0)
       ;; Length must be multiple of 4
       (= (remainder (string-length key-string) 4) 0)
       ;; All chars valid
       (every (lambda (c) (base64-char? (char->integer c)))
              (string->list key-string))
       ;; Padding at end only
       (let ((padding-start (string-index key-string #\=)))
         (or (not padding-start)
             (every (lambda (c) (char=? c #\=))
                    (string->list (substring key-string padding-start)))))))

;;; Check if string is valid hex key
(define (key-valid-hex? key-string)
  (and (string? key-string)
       (> (string-length key-string) 0)
       (= (remainder (string-length key-string) 2) 0)
       (valid-hex-string? key-string)))

;;; Check if string looks like PEM format
(define (key-valid-pem? key-string)
  (and (string? key-string)
       (or (string-prefix? "-----BEGIN" key-string)
           (string-contains key-string "-----BEGIN"))))

;;; Check if string looks like JWK format
(define (key-valid-jwk? key-string)
  (and (string? key-string)
       (string-prefix? "{" (string-trim key-string))
       (string-contains key-string "\"kty\"")))

;;; Check if key has acceptable length
(define (key-length-ok? key-string min-bits max-bits)
  (and (string? key-string)
       (let ((key-bits (* (string-length key-string) 4))) ; Assume hex
         (and (>= key-bits min-bits)
              (<= key-bits max-bits)))))

;;; Constant-time string comparison to prevent timing attacks
(define (constant-time-equals? str1 str2)
  (cond
   ((not (and (string? str1) (string? str2))) #f)
   ((not (= (string-length str1) (string-length str2))) #f)
   (else
    (let loop ((chars1 (string->list str1))
               (chars2 (string->list str2))
               (result 0))
      (if (null? chars1)
          (= result 0)
          (loop (cdr chars1)
                (cdr chars2)
                (logior result
                        (logxor (char->integer (car chars1))
                               (char->integer (car chars2))))))))))

;;; Normalize hash to lowercase
(define (hash-normalize hash-string)
  (if (valid-hex-string? hash-string)
      (string-downcase hash-string)
      hash-string))

;;; Format hash with algorithm prefix
(define (hash-format algorithm hash-string)
  (if (valid-hex-string? hash-string)
      (string-append (symbol->string algorithm) ":" (hash-normalize hash-string))
      #f))
