#lang racket/base

;; SPDX-License-Identifier: Apache-2.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafePath - Directory traversal prevention for Racket
;;

(require racket/string)

(provide
 (struct-out path-result)
 has-traversal?
 sanitize-filename
 safe-path-join)

;; Path result struct
(struct path-result (path error ok?) #:transparent)

;; Check for path traversal patterns
(define (has-traversal? path)
  (define lower-path (string-downcase path))
  (or (string-contains? path "..")
      (string-contains? path "./")
      (string-contains? lower-path "%2e%2e")
      (string-contains? lower-path "%00")))

;; Sanitize filename by removing dangerous characters
(define (sanitize-filename input)
  (list->string
   (map (lambda (c)
          (if (member c '(#\/ #\\ #\: #\* #\? #\" #\< #\> #\|))
              #\_
              c))
        (string->list input))))

;; Safely join paths
(define (safe-path-join base filename)
  (cond
    [(has-traversal? filename)
     (path-result "" "Path traversal detected" #f)]
    [else
     (define safe-name (sanitize-filename filename))
     (define joined
       (if (string-suffix? base "/")
           (string-append base safe-name)
           (string-append base "/" safe-name)))
     (path-result joined "" #t)]))
