;;; SPDX-License-Identifier: MPL-2.0-or-later
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeJson - Safe JSON operations for Guile Scheme
;;;
;;; Provides safe JSON validation and path access.
;;; All operations are pure and cannot crash.

(define-module (proven safe-json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (json-valid?
            json-valid-string?
            json-type-of
            json-path-get
            json-path-exists?
            json-array?
            json-object?
            json-string?
            json-number?
            json-boolean?
            json-null?
            json-escape-string
            json-unescape-string
            json-stringify-simple
            json-depth
            json-key-count
            json-keys))

;;; Make a result record
(define (make-result value ok)
  `((value . ,value) (ok . ,ok)))

(define (result-value result)
  (assoc-ref result 'value))

(define (result-ok? result)
  (assoc-ref result 'ok))

;;; Check if value is JSON null
(define (json-null? value)
  (eq? value 'null))

;;; Check if value is JSON boolean
(define (json-boolean? value)
  (or (eq? value #t) (eq? value #f)))

;;; Check if value is JSON string
(define (json-string? value)
  (string? value))

;;; Check if value is JSON number
(define (json-number? value)
  (number? value))

;;; Check if value is JSON array
(define (json-array? value)
  (and (list? value)
       (or (null? value)
           (not (pair? (car value))))))

;;; Check if value is JSON object (alist)
(define (json-object? value)
  (and (list? value)
       (or (null? value)
           (and (pair? (car value))
                (string? (caar value))))))

;;; Get type of JSON value
(define (json-type-of value)
  (cond
   ((json-null? value) 'null)
   ((json-boolean? value) 'boolean)
   ((json-string? value) 'string)
   ((json-number? value) 'number)
   ((json-array? value) 'array)
   ((json-object? value) 'object)
   (else 'unknown)))

;;; Check for control characters
(define (has-control-char? str)
  (any (lambda (c)
         (let ((code (char->integer c)))
           (and (< code 32)
                (not (= code 9))   ; tab
                (not (= code 10))  ; newline
                (not (= code 13))))) ; carriage return
       (string->list str)))

;;; Simple JSON string validation
(define (json-valid-string? str)
  (and (string? str)
       (> (string-length str) 0)
       ;; Minimal validation - proper parsing would be more complex
       (or (string-prefix? "{" (string-trim str))
           (string-prefix? "[" (string-trim str))
           (string-prefix? "\"" (string-trim str))
           (string=? "true" (string-trim str))
           (string=? "false" (string-trim str))
           (string=? "null" (string-trim str))
           (string->number (string-trim str)))))

;;; Check if parsed value is valid JSON
(define (json-valid? value)
  (or (json-null? value)
      (json-boolean? value)
      (json-string? value)
      (json-number? value)
      (json-array? value)
      (json-object? value)))

;;; Get value at path (path is list of keys/indices)
(define (json-path-get value path)
  (if (null? path)
      (make-result value #t)
      (let ((key (car path))
            (rest (cdr path)))
        (cond
         ;; Array index
         ((and (json-array? value) (integer? key))
          (if (and (>= key 0) (< key (length value)))
              (json-path-get (list-ref value key) rest)
              (make-result #f #f)))
         ;; Object key
         ((and (json-object? value) (string? key))
          (let ((pair (assoc key value)))
            (if pair
                (json-path-get (cdr pair) rest)
                (make-result #f #f))))
         (else
          (make-result #f #f))))))

;;; Check if path exists
(define (json-path-exists? value path)
  (result-ok? (json-path-get value path)))

;;; Escape string for JSON
(define (json-escape-string str)
  (apply string-append
         (map (lambda (c)
                (case c
                  ((#\") "\\\"")
                  ((#\\) "\\\\")
                  ((#\backspace) "\\b")
                  ((#\page) "\\f")
                  ((#\newline) "\\n")
                  ((#\return) "\\r")
                  ((#\tab) "\\t")
                  (else
                   (let ((code (char->integer c)))
                     (if (< code 32)
                         (format #f "\\u~4,'0x" code)
                         (string c))))))
              (string->list str))))

;;; Unescape JSON string
(define (json-unescape-string str)
  (let loop ((chars (string->list str))
             (result '())
             (escape #f))
    (cond
     ((null? chars)
      (list->string (reverse result)))
     (escape
      (case (car chars)
        ((#\") (loop (cdr chars) (cons #\" result) #f))
        ((#\\) (loop (cdr chars) (cons #\\ result) #f))
        ((#\n) (loop (cdr chars) (cons #\newline result) #f))
        ((#\r) (loop (cdr chars) (cons #\return result) #f))
        ((#\t) (loop (cdr chars) (cons #\tab result) #f))
        ((#\b) (loop (cdr chars) (cons #\backspace result) #f))
        ((#\f) (loop (cdr chars) (cons #\page result) #f))
        (else (loop (cdr chars) (cons (car chars) result) #f))))
     ((char=? (car chars) #\\)
      (loop (cdr chars) result #t))
     (else
      (loop (cdr chars) (cons (car chars) result) #f)))))

;;; Simple JSON stringify (for basic values)
(define (json-stringify-simple value)
  (cond
   ((json-null? value) "null")
   ((eq? value #t) "true")
   ((eq? value #f) "false")
   ((string? value)
    (string-append "\"" (json-escape-string value) "\""))
   ((number? value)
    (number->string value))
   ((json-array? value)
    (string-append
     "["
     (string-join (map json-stringify-simple value) ",")
     "]"))
   ((json-object? value)
    (string-append
     "{"
     (string-join
      (map (lambda (pair)
             (string-append
              "\"" (json-escape-string (car pair)) "\":"
              (json-stringify-simple (cdr pair))))
           value)
      ",")
     "}"))
   (else "null")))

;;; Calculate depth of JSON structure
(define (json-depth value)
  (cond
   ((json-array? value)
    (if (null? value)
        1
        (+ 1 (apply max (map json-depth value)))))
   ((json-object? value)
    (if (null? value)
        1
        (+ 1 (apply max (map (lambda (p) (json-depth (cdr p))) value)))))
   (else 0)))

;;; Count keys in object
(define (json-key-count value)
  (if (json-object? value)
      (length value)
      0))

;;; Get all keys from object
(define (json-keys value)
  (if (json-object? value)
      (map car value)
      '()))
