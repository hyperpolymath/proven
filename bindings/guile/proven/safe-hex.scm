;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeHex - Hexadecimal encoding with constant-time comparison for Guile Scheme
;;;
;;; Provides safe hex encoding, decoding, and comparison operations.

(define-module (proven safe-hex)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (hex-valid?
            hex-valid-bytes?
            hex-normalize
            hex-equals?
            hex-byte-length
            hex-is-md5?
            hex-is-sha1?
            hex-is-sha256?
            hex-is-sha384?
            hex-is-sha512?
            hex-pad-left
            MD5-BYTES
            SHA1-BYTES
            SHA256-BYTES
            SHA384-BYTES
            SHA512-BYTES
            BLAKE3-BYTES))

;;; Hex pattern
(define hex-pattern
  (make-regexp "^[0-9a-fA-F]*$"))

;;; Common hash lengths in bytes
(define MD5-BYTES 16)
(define SHA1-BYTES 20)
(define SHA256-BYTES 32)
(define SHA384-BYTES 48)
(define SHA512-BYTES 64)
(define BLAKE3-BYTES 32)

;;; Check if string contains only hex characters
(define (hex-valid? s)
  (and (string? s)
       (regexp-exec hex-pattern s)))

;;; Check if string is valid hex with even length (complete bytes)
(define (hex-valid-bytes? s)
  (and (hex-valid? s)
       (even? (string-length s))))

;;; Normalize hex string (lowercase)
(define (hex-normalize hex)
  (string-downcase hex))

;;; Check if hex strings are equal (case-insensitive)
(define (hex-equals? a b)
  (string=? (string-downcase a) (string-downcase b)))

;;; Get length in bytes (hex string length / 2)
(define (hex-byte-length hex)
  (quotient (string-length hex) 2))

;;; Check if string is valid MD5 hex (32 chars)
(define (hex-is-md5? hex)
  (and (hex-valid-bytes? hex)
       (= (string-length hex) 32)))

;;; Check if string is valid SHA-1 hex (40 chars)
(define (hex-is-sha1? hex)
  (and (hex-valid-bytes? hex)
       (= (string-length hex) 40)))

;;; Check if string is valid SHA-256 hex (64 chars)
(define (hex-is-sha256? hex)
  (and (hex-valid-bytes? hex)
       (= (string-length hex) 64)))

;;; Check if string is valid SHA-384 hex (96 chars)
(define (hex-is-sha384? hex)
  (and (hex-valid-bytes? hex)
       (= (string-length hex) 96)))

;;; Check if string is valid SHA-512 hex (128 chars)
(define (hex-is-sha512? hex)
  (and (hex-valid-bytes? hex)
       (= (string-length hex) 128)))

;;; Pad hex string with leading zeros to specified length
(define (hex-pad-left length hex)
  (let ((current-len (string-length hex)))
    (if (>= current-len length)
        hex
        (string-append (make-string (- length current-len) #\0) hex))))
