;;; SPDX-License-Identifier: PMPL-1.0
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafePath - Path operations without traversal attacks for Guile Scheme
;;;
;;; Validates and sanitizes filesystem paths to prevent directory traversal,
;;; null byte injection, and other path-based attacks.

(define-module (proven safe-path)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (path-has-traversal?
            path-is-absolute?
            path-is-relative?
            sanitize-filename
            path-validate
            path-join-safe
            path-extension
            path-basename
            path-dirname
            make-path-result
            path-result-path
            path-result-ok?
            path-result-error))

;;; Result constructor for path operations
(define (make-path-result path ok error)
  `((path . ,path) (ok . ,ok) (error . ,error)))

;;; Result accessors
(define (path-result-path result)
  (assoc-ref result 'path))

(define (path-result-ok? result)
  (assoc-ref result 'ok))

(define (path-result-error result)
  (assoc-ref result 'error))

;;; Check if path contains directory traversal sequences
(define (path-has-traversal? path)
  (or (string-contains path "..")
      (string-contains path "\x00")))

;;; Check if path is absolute (starts with /)
(define (path-is-absolute? path)
  (and (> (string-length path) 0)
       (char=? (string-ref path 0) #\/)))

;;; Check if path is relative (does not start with /)
(define (path-is-relative? path)
  (not (path-is-absolute? path)))

;;; Sanitize filename (remove dangerous characters)
(define (sanitize-filename filename)
  (let* (;; Remove path separators and null bytes
         (s1 (regexp-substitute/global #f "[/\\\\\x00]" filename 'pre "" 'post))
         ;; Remove leading dots
         (s2 (regexp-substitute/global #f "^\\.+" s1 'pre "" 'post))
         ;; Trim whitespace
         (s3 (string-trim-both s2)))
    s3))

;;; Validate path is safe (no traversal, no null bytes)
(define (path-validate path)
  (cond
   ((= (string-length path) 0)
    (make-path-result "" #f "Empty path"))
   ((string-contains path "\x00")
    (make-path-result "" #f "Null byte in path"))
   ((string-contains path "..")
    (make-path-result "" #f "Path traversal detected"))
   (else
    (make-path-result path #t ""))))

;;; Join paths safely (prevents traversal in child component)
(define (path-join-safe base child)
  (let ((validated (path-validate child)))
    (cond
     ((not (path-result-ok? validated))
      validated)
     ((path-is-absolute? child)
      (make-path-result "" #f "Child path must be relative"))
     (else
      (make-path-result (string-append base "/" child) #t "")))))

;;; Get file extension (returns empty string if none)
(define (path-extension path)
  (let ((parts (string-split path #\.)))
    (if (<= (length parts) 1)
        ""
        (last parts))))

;;; Get basename (filename without directory)
(define (path-basename path)
  (let ((parts (string-split path #\/)))
    (if (null? parts)
        path
        (last parts))))

;;; Get dirname (directory without filename)
(define (path-dirname path)
  (let ((parts (string-split path #\/)))
    (cond
     ((<= (length parts) 1)
      ".")
     (else
      (string-join (drop-right parts 1) "/")))))
