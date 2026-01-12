#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeString - XSS prevention for Racket
;;

(require racket/string
         racket/format)

(provide
 escape-html
 escape-sql
 escape-js
 sanitize-default
 url-encode
 slugify)

;; Escape HTML special characters
(define (escape-html input)
  (string-replace
   (string-replace
    (string-replace
     (string-replace
      (string-replace input "&" "&amp;")
      "<" "&lt;")
     ">" "&gt;")
    "\"" "&quot;")
   "'" "&#x27;"))

;; Escape SQL single quotes
(define (escape-sql input)
  (string-replace input "'" "''"))

;; Escape JavaScript special characters
(define (escape-js input)
  (define replacements
    '(("\\" . "\\\\")
      ("\"" . "\\\"")
      ("'" . "\\'")
      ("\n" . "\\n")
      ("\r" . "\\r")
      ("\t" . "\\t")
      ("<" . "\\u003C")
      (">" . "\\u003E")
      ("/" . "\\/")))
  (for/fold ([result input])
            ([pair replacements])
    (string-replace result (car pair) (cdr pair))))

;; Sanitize to alphanumeric + underscore + hyphen
(define (sanitize-default input)
  (list->string
   (filter (lambda (c)
             (or (char-alphabetic? c)
                 (char-numeric? c)
                 (char=? c #\_)
                 (char=? c #\-)))
           (string->list input))))

;; URL encode
(define (url-encode input)
  (define (encode-char c)
    (cond
      [(or (char-alphabetic? c)
           (char-numeric? c)
           (member c '(#\- #\_ #\. #\~)))
       (string c)]
      [else
       (format "%~a" (string-upcase
                      (number->string (char->integer c) 16)))]))
  (apply string-append (map encode-char (string->list input))))

;; Convert to URL-safe slug
(define (slugify input)
  (define lower (string-downcase input))
  (define cleaned
    (list->string
     (filter (lambda (c)
               (or (char-alphabetic? c)
                   (char-numeric? c)
                   (char=? c #\space)
                   (char=? c #\-)))
             (string->list lower))))
  ;; Replace spaces with hyphens and collapse multiple hyphens
  (regexp-replace* #rx"-+"
                   (string-replace cleaned " " "-")
                   "-"))
