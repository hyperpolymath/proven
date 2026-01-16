;;; SPDX-License-Identifier: PMPL-1.0
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeUuid - UUID validation following RFC 4122 for Guile Scheme
;;;
;;; Validates and parses UUIDs without throwing exceptions.

(define-module (proven safe-uuid)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (UUID-NIL
            uuid-valid?
            uuid-valid-v4?
            uuid-nil?
            uuid-version
            uuid-normalize
            uuid-parse
            uuid-to-hex
            uuid-from-hex
            make-uuid-result
            make-uuid-parse-result))

;;; Nil UUID
(define UUID-NIL "00000000-0000-0000-0000-000000000000")

;;; UUID patterns
(define uuid-pattern
  (make-regexp "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"))

(define uuid-v4-pattern
  (make-regexp "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-4[0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$"))

;;; Result constructors
(define (make-uuid-result uuid ok)
  `((uuid . ,uuid) (ok . ,ok)))

(define (make-uuid-parse-result time-low time-mid time-hi clock-seq node ok)
  `((time-low . ,time-low)
    (time-mid . ,time-mid)
    (time-hi . ,time-hi)
    (clock-seq . ,clock-seq)
    (node . ,node)
    (ok . ,ok)))

;;; Check if string is a valid UUID (any version)
(define (uuid-valid? uuid)
  (and (string? uuid)
       (regexp-exec uuid-pattern uuid)))

;;; Check if string is a valid UUID v4
(define (uuid-valid-v4? uuid)
  (and (string? uuid)
       (regexp-exec uuid-v4-pattern uuid)))

;;; Check if UUID is nil
(define (uuid-nil? uuid)
  (string=? uuid UUID-NIL))

;;; Get UUID version (returns 0 if invalid)
(define (uuid-version uuid)
  (if (not (uuid-valid? uuid))
      0
      ;; Version is at position 14 (character after second hyphen)
      (let ((version-char (substring uuid 14 15)))
        (if (regexp-exec (make-regexp "[1-5]") version-char)
            (string->number version-char)
            0))))

;;; Normalize UUID to lowercase
(define (uuid-normalize uuid)
  (string-downcase uuid))

;;; Parse UUID into components
(define (uuid-parse uuid)
  (if (not (uuid-valid? uuid))
      (make-uuid-parse-result "" "" "" "" "" #f)
      (let ((parts (string-split uuid #\-)))
        (make-uuid-parse-result
         (list-ref parts 0)
         (list-ref parts 1)
         (list-ref parts 2)
         (list-ref parts 3)
         (list-ref parts 4)
         #t))))

;;; Remove hyphens from UUID
(define (uuid-to-hex uuid)
  (string-filter (lambda (c) (not (char=? c #\-))) uuid))

;;; Format 32 hex chars as UUID
(define (uuid-from-hex hex)
  (if (not (= (string-length hex) 32))
      (make-uuid-result "" #f)
      (if (not (regexp-exec (make-regexp "^[0-9a-fA-F]{32}$") hex))
          (make-uuid-result "" #f)
          (let ((formatted (string-append
                            (substring hex 0 8) "-"
                            (substring hex 8 12) "-"
                            (substring hex 12 16) "-"
                            (substring hex 16 20) "-"
                            (substring hex 20 32))))
            (make-uuid-result formatted #t)))))
