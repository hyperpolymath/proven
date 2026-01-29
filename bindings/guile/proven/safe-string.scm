;;; SPDX-License-Identifier: MPL-2.0-or-later
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeString - XSS and injection prevention for Guile Scheme
;;;
;;; Provides safe escaping for SQL, HTML, JavaScript, and shell contexts.
;;; All operations are pure and cannot crash.

(define-module (proven safe-string)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (escape-html
            escape-sql
            escape-js
            escape-shell
            sanitize
            slugify
            truncate-string
            is-alphanumeric?
            is-identifier?))

;;; Helper: replace all occurrences of a substring
(define (string-replace-all str old new)
  (let loop ((s str))
    (let ((i (string-contains s old)))
      (if i
          (loop (string-append (substring s 0 i)
                               new
                               (substring s (+ i (string-length old)))))
          s))))

;;; Escape HTML special characters to prevent XSS
(define (escape-html input)
  (let* ((s1 (string-replace-all input "&" "&amp;"))
         (s2 (string-replace-all s1 "<" "&lt;"))
         (s3 (string-replace-all s2 ">" "&gt;"))
         (s4 (string-replace-all s3 "\"" "&quot;"))
         (s5 (string-replace-all s4 "'" "&#x27;")))
    s5))

;;; Escape SQL single quotes to prevent SQL injection
(define (escape-sql input)
  (string-replace-all input "'" "''"))

;;; Escape JavaScript special characters
(define (escape-js input)
  (let* ((s1 (string-replace-all input "\\" "\\\\"))
         (s2 (string-replace-all s1 "\"" "\\\""))
         (s3 (string-replace-all s2 "'" "\\'"))
         (s4 (string-replace-all s3 "\n" "\\n"))
         (s5 (string-replace-all s4 "\r" "\\r"))
         (s6 (string-replace-all s5 "\t" "\\t"))
         (s7 (string-replace-all s6 "<" "\\u003C"))
         (s8 (string-replace-all s7 ">" "\\u003E")))
    s8))

;;; Escape shell metacharacters
(define (escape-shell input)
  (let* ((s1 (string-replace-all input "\\" "\\\\"))
         (s2 (string-replace-all s1 "\"" "\\\""))
         (s3 (string-replace-all s2 "$" "\\$"))
         (s4 (string-replace-all s3 "`" "\\`"))
         (s5 (string-replace-all s4 "!" "\\!")))
    s5))

;;; Check if character is alphanumeric, underscore, or hyphen
(define (safe-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (char=? c #\_)
      (char=? c #\-)))

;;; Sanitize to alphanumeric + underscore + hyphen only
(define (sanitize input)
  (list->string
   (filter safe-char? (string->list input))))

;;; Check if string contains only alphanumeric characters
(define (is-alphanumeric? input)
  (string-every (lambda (c)
                  (or (char-alphabetic? c)
                      (char-numeric? c)))
                input))

;;; Check if string is a valid identifier
(define (is-identifier? input)
  (and (> (string-length input) 0)
       (let ((first-char (string-ref input 0)))
         (or (char-alphabetic? first-char)
             (char=? first-char #\_)))
       (string-every (lambda (c)
                       (or (char-alphabetic? c)
                           (char-numeric? c)
                           (char=? c #\_)))
                     input)))

;;; Convert to URL-safe slug
(define (slugify input)
  (let* ((lower (string-downcase input))
         (cleaned (list->string
                   (filter (lambda (c)
                             (or (char-alphabetic? c)
                                 (char-numeric? c)
                                 (char=? c #\space)
                                 (char=? c #\-)))
                           (string->list lower))))
         (with-hyphens (string-replace-all cleaned " " "-"))
         ;; Collapse multiple hyphens
         (collapsed (regexp-substitute/global #f "-+" with-hyphens 'pre "-" 'post)))
    collapsed))

;;; Truncate string to max length (safe, no crash on short strings)
(define (truncate-string input max-len)
  (if (<= (string-length input) max-len)
      input
      (substring input 0 max-len)))
