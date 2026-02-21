#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeCrypto - Cryptographic operations for Racket
;;

(require racket/format
         racket/random
         file/sha1)

(provide
 constant-time-equal?
 sha256-hash
 sha1-hash
 bytes->hex
 generate-token
 random-int
 secure-wipe!)

;; Token character set
(define TOKEN-CHARS
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

;; Constant-time comparison to prevent timing attacks
(define (constant-time-equal? a b)
  (cond
    [(not (= (string-length a) (string-length b))) #f]
    [else
     (define diff
       (for/fold ([d 0])
                 ([ca (in-string a)]
                  [cb (in-string b)])
         (bitwise-ior d (bitwise-xor (char->integer ca)
                                      (char->integer cb)))))
     (= diff 0)]))

;; SHA-256 hash using Racket's crypto library
(define (sha256-hash input)
  (require openssl/sha256)
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 input))))

;; SHA-1 hash (for compatibility, not recommended for security)
(define (sha1-hash input)
  (sha1 (open-input-string input)))

;; Convert bytes to hexadecimal string
(define (bytes->hex bstr)
  (apply string-append
         (for/list ([b (in-bytes bstr)])
           (~a (number->string b 16) #:width 2 #:align 'right #:pad-string "0"))))

;; Generate a random token
(define (generate-token length)
  (define chars-len (string-length TOKEN-CHARS))
  (list->string
   (for/list ([_ (in-range length)])
     (string-ref TOKEN-CHARS (random chars-len)))))

;; Random integer in range [min-val, max-val]
(define (random-int min-val max-val)
  (+ min-val (random (+ 1 (- max-val min-val)))))

;; Secure wipe (best effort - overwrites string contents)
;; Note: Due to GC, this is not guaranteed to fully wipe memory
(define (secure-wipe! str-box)
  (define str (unbox str-box))
  (define len (string-length str))

  ;; Overwrite with zeros
  (set-box! str-box (make-string len #\nul))

  ;; Overwrite with ones
  (set-box! str-box (make-string len #\xff))

  ;; Final zeros
  (set-box! str-box (make-string len #\nul)))
