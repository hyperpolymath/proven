#lang racket/base

;; SPDX-License-Identifier: Apache-2.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeJson - Safe JSON operations for Racket
;;

(require json
         racket/contract
         racket/match)

(provide
 ;; Safe parsing
 safe-json-parse
 safe-json-read

 ;; Safe serialization
 safe-json-write
 safe-json-string

 ;; Value extraction with defaults
 json-get
 json-get-string
 json-get-number
 json-get-boolean
 json-get-array
 json-get-object

 ;; Path navigation
 json-path

 ;; Validation
 valid-json-string?
 json-type-of

 ;; Safe construction
 make-json-object
 make-json-array)

;; Result struct for safe operations
(struct json-result (value ok? error) #:transparent)

;; Safe JSON parsing from string
(define (safe-json-parse str [max-depth 100])
  (with-handlers ([exn:fail? (lambda (e)
                               (json-result #f #f (exn-message e)))])
    (if (> (string-length str) (* 10 1024 1024)) ; 10MB limit
        (json-result #f #f "JSON string too large")
        (let ([parsed (string->jsexpr str)])
          (json-result parsed #t #f)))))

;; Safe JSON parsing from port
(define (safe-json-read port)
  (with-handlers ([exn:fail? (lambda (e)
                               (json-result #f #f (exn-message e)))])
    (let ([parsed (read-json port)])
      (json-result parsed #t #f))))

;; Safe JSON serialization to port
(define (safe-json-write value port)
  (with-handlers ([exn:fail? (lambda (e)
                               (json-result #f #f (exn-message e)))])
    (write-json value port)
    (json-result #t #t #f)))

;; Safe JSON serialization to string
(define (safe-json-string value)
  (with-handlers ([exn:fail? (lambda (e)
                               (json-result #f #f (exn-message e)))])
    (json-result (jsexpr->string value) #t #f)))

;; Get value from JSON object with default
(define (json-get obj key [default #f])
  (if (hash? obj)
      (hash-ref obj (if (symbol? key) key (string->symbol key)) default)
      default))

;; Get string value with default
(define (json-get-string obj key [default ""])
  (let ([val (json-get obj key)])
    (if (string? val) val default)))

;; Get number value with default
(define (json-get-number obj key [default 0])
  (let ([val (json-get obj key)])
    (if (number? val) val default)))

;; Get boolean value with default
(define (json-get-boolean obj key [default #f])
  (let ([val (json-get obj key)])
    (if (boolean? val) val default)))

;; Get array value with default
(define (json-get-array obj key [default '()])
  (let ([val (json-get obj key)])
    (if (list? val) val default)))

;; Get object value with default
(define (json-get-object obj key [default (hasheq)])
  (let ([val (json-get obj key)])
    (if (hash? val) val default)))

;; Navigate JSON by path (list of keys/indices)
(define (json-path obj path [default #f])
  (if (null? path)
      obj
      (let ([key (car path)])
        (cond
          [(and (hash? obj) (or (string? key) (symbol? key)))
           (json-path (json-get obj key default) (cdr path) default)]
          [(and (list? obj) (exact-nonnegative-integer? key) (< key (length obj)))
           (json-path (list-ref obj key) (cdr path) default)]
          [else default]))))

;; Check if string is valid JSON
(define (valid-json-string? str)
  (json-result-ok? (safe-json-parse str)))

;; Get type of JSON value
(define (json-type-of value)
  (cond
    [(hash? value) 'object]
    [(list? value) 'array]
    [(string? value) 'string]
    [(number? value) 'number]
    [(boolean? value) 'boolean]
    [(eq? value 'null) 'null]
    [else 'unknown]))

;; Create JSON object from key-value pairs
(define (make-json-object . pairs)
  (for/hasheq ([pair (in-list pairs)])
    (values (if (symbol? (car pair))
                (car pair)
                (string->symbol (car pair)))
            (cdr pair))))

;; Create JSON array
(define (make-json-array . elements)
  elements)
