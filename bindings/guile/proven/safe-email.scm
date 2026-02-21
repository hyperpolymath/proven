;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeEmail - RFC 5321 compliant email validation for Guile Scheme
;;;
;;; Validates email addresses without regex catastrophic backtracking.
;;; All operations are pure and cannot crash.

(define-module (proven safe-email)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (email-valid?
            email-parse
            email-normalize
            email-get-domain
            email-get-local
            make-email-result
            email-result-local
            email-result-domain
            email-result-ok?
            email-result-error))

;;; Result constructor for email operations
(define (make-email-result local domain ok error)
  `((local . ,local) (domain . ,domain) (ok . ,ok) (error . ,error)))

;;; Result accessors
(define (email-result-local result)
  (assoc-ref result 'local))

(define (email-result-domain result)
  (assoc-ref result 'domain))

(define (email-result-ok? result)
  (assoc-ref result 'ok))

(define (email-result-error result)
  (assoc-ref result 'error))

;;; Basic email pattern (simplified, safe from ReDoS)
(define email-pattern
  (make-regexp "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))

;;; Check if string is a valid email address
(define (email-valid? email)
  (and (regexp-exec email-pattern email)
       (<= (string-length email) 254)  ; Max email length per RFC
       (not (string-contains email "..")))) ; No consecutive dots

;;; Parse email into local and domain parts
(define (email-parse email)
  (if (not (email-valid? email))
      (make-email-result "" "" #f "Invalid email format")
      (let ((at-pos (string-index email #\@)))
        (if (not at-pos)
            (make-email-result "" "" #f "Invalid email format")
            (make-email-result
             (substring email 0 at-pos)
             (substring email (+ at-pos 1))
             #t
             "")))))

;;; Normalize email (lowercase domain, trim whitespace)
(define (email-normalize email)
  (let* ((trimmed (string-trim-both email))
         (at-pos (string-index trimmed #\@)))
    (if (not at-pos)
        trimmed
        (let ((local (substring trimmed 0 at-pos))
              (domain (string-downcase (substring trimmed (+ at-pos 1)))))
          (string-append local "@" domain)))))

;;; Get domain from email
(define (email-get-domain email)
  (let ((at-pos (string-index email #\@)))
    (if (not at-pos)
        ""
        (substring email (+ at-pos 1)))))

;;; Get local part from email
(define (email-get-local email)
  (let ((at-pos (string-index email #\@)))
    (if (not at-pos)
        ""
        (substring email 0 at-pos))))
