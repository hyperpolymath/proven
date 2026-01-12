#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeEmail - Email validation for Racket
;;

(require racket/string
         racket/list)

(provide
 (struct-out email-result)
 valid-email?
 parse-email
 disposable-email?
 normalize-email)

;; Constants
(define MAX-EMAIL-LENGTH 254)
(define MAX-LOCAL-LENGTH 64)
(define MAX-DOMAIN-LENGTH 255)

;; Known disposable email domains
(define DISPOSABLE-DOMAINS
  '("mailinator.com"
    "guerrillamail.com"
    "10minutemail.com"
    "tempmail.com"
    "throwaway.email"
    "fakeinbox.com"
    "trashmail.com"
    "maildrop.cc"))

;; Email result struct
(struct email-result (local-part domain error ok?) #:transparent)

;; Basic email validation
(define (valid-email? email)
  (define email-len (string-length email))

  ;; Check length bounds
  (when (or (= email-len 0) (> email-len MAX-EMAIL-LENGTH))
    (return #f))

  ;; Find @ symbol
  (define at-pos (string-index email #\@))
  (unless at-pos (return #f))

  ;; Count @ symbols (must be exactly 1)
  (define at-count (count (lambda (c) (char=? c #\@)) (string->list email)))
  (unless (= at-count 1) (return #f))

  ;; @ cannot be first or last
  (when (or (= at-pos 0) (= at-pos (- email-len 1)))
    (return #f))

  ;; Check local part length
  (when (> at-pos MAX-LOCAL-LENGTH)
    (return #f))

  ;; Domain must have at least one dot
  (define domain (substring email (+ at-pos 1)))
  (unless (string-contains? domain ".")
    (return #f))

  #t)

;; Helper: Find character index in string
(define (string-index str char)
  (for/first ([i (in-naturals)]
              [c (in-string str)]
              #:when (char=? c char))
    i))

;; Helper: Return macro for early exit
(define-syntax-rule (return v) v)

;; Parse email into local part and domain
(define (parse-email email)
  (define email-len (string-length email))

  (cond
    [(= email-len 0)
     (email-result "" "" "Email is empty" #f)]

    [(> email-len MAX-EMAIL-LENGTH)
     (email-result "" "" "Email exceeds maximum length" #f)]

    [else
     (define at-pos (string-index email #\@))
     (cond
       [(not at-pos)
        (email-result "" "" "Missing @ symbol" #f)]

       [(= at-pos 0)
        (email-result "" "" "Local part is empty" #f)]

       [(> at-pos MAX-LOCAL-LENGTH)
        (email-result "" "" "Local part exceeds maximum length" #f)]

       [else
        (define local-part (substring email 0 at-pos))
        (define domain (substring email (+ at-pos 1)))

        (cond
          [(= (string-length domain) 0)
           (email-result "" "" "Domain is empty" #f)]

          [(not (string-contains? domain "."))
           (email-result "" "" "Domain must contain a dot" #f)]

          [else
           (email-result local-part domain "" #t)])])]))

;; Check for disposable email domain
(define (disposable-email? domain)
  (define lower-domain (string-downcase (string-trim domain)))
  (member lower-domain DISPOSABLE-DOMAINS))

;; Normalize email (lowercase domain)
(define (normalize-email email)
  (define at-pos (string-index email #\@))
  (if (not at-pos)
      email
      (let ([local-part (substring email 0 at-pos)]
            [domain (string-downcase (substring email (+ at-pos 1)))])
        (string-append local-part "@" domain))))
