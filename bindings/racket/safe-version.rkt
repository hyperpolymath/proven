#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeVersion - Semantic versioning for Racket
;;

(require racket/string
         racket/match
         racket/format)

(provide
 ;; Version struct
 (struct-out semver)

 ;; Parsing
 parse-semver
 valid-semver?

 ;; Comparison
 semver-compare
 semver<?
 semver>?
 semver=?
 semver<=?
 semver>=?

 ;; Formatting
 semver->string

 ;; Operations
 bump-major
 bump-minor
 bump-patch
 with-prerelease
 with-build-metadata

 ;; Range checking
 satisfies-range?

 ;; Utilities
 parse-version-range)

;; Semantic version structure
(struct semver (major minor patch prerelease build-metadata) #:transparent)

;; Result struct
(struct version-result (value ok? error) #:transparent)

;; Parse pre-release identifiers
(define (parse-prerelease str)
  (if (or (not str) (string=? str ""))
      '()
      (string-split str ".")))

;; Parse semantic version string
(define (parse-semver str)
  (let ([match (regexp-match
                #rx"^[vV]?([0-9]+)\\.([0-9]+)\\.([0-9]+)(-([a-zA-Z0-9.-]+))?(\\+([a-zA-Z0-9.-]+))?$"
                str)])
    (if match
        (let* ([major (string->number (list-ref match 1))]
               [minor (string->number (list-ref match 2))]
               [patch (string->number (list-ref match 3))]
               [prerelease (list-ref match 5)]
               [build-meta (list-ref match 7)])
          (version-result
           (semver major minor patch
                   (parse-prerelease prerelease)
                   (or build-meta ""))
           #t #f))
        (version-result #f #f "Invalid semantic version format"))))

;; Check if string is valid semver
(define (valid-semver? str)
  (version-result-ok? (parse-semver str)))

;; Convert semver to string
(define (semver->string v)
  (let* ([base (format "~a.~a.~a"
                       (semver-major v)
                       (semver-minor v)
                       (semver-patch v))]
         [with-pre (if (null? (semver-prerelease v))
                       base
                       (format "~a-~a" base (string-join (semver-prerelease v) ".")))]
         [with-build (if (string=? (semver-build-metadata v) "")
                         with-pre
                         (format "~a+~a" with-pre (semver-build-metadata v)))])
    with-build))

;; Compare pre-release identifiers
(define (compare-prerelease pre1 pre2)
  (cond
    ;; No prerelease > prerelease
    [(and (null? pre1) (not (null? pre2))) 1]
    [(and (not (null? pre1)) (null? pre2)) -1]
    [(and (null? pre1) (null? pre2)) 0]
    [else
     (let loop ([p1 pre1] [p2 pre2])
       (cond
         [(and (null? p1) (null? p2)) 0]
         [(null? p1) -1]
         [(null? p2) 1]
         [else
          (let* ([id1 (car p1)]
                 [id2 (car p2)]
                 [num1 (string->number id1)]
                 [num2 (string->number id2)])
            (cond
              [(and num1 num2)
               (cond [(< num1 num2) -1]
                     [(> num1 num2) 1]
                     [else (loop (cdr p1) (cdr p2))])]
              [(and num1 (not num2)) -1]
              [(and (not num1) num2) 1]
              [else
               (let ([cmp (string<? id1 id2)])
                 (cond [(string<? id1 id2) -1]
                       [(string>? id1 id2) 1]
                       [else (loop (cdr p1) (cdr p2))]))]))]))])))

;; Compare two semvers
(define (semver-compare v1 v2)
  (cond
    [(< (semver-major v1) (semver-major v2)) -1]
    [(> (semver-major v1) (semver-major v2)) 1]
    [(< (semver-minor v1) (semver-minor v2)) -1]
    [(> (semver-minor v1) (semver-minor v2)) 1]
    [(< (semver-patch v1) (semver-patch v2)) -1]
    [(> (semver-patch v1) (semver-patch v2)) 1]
    [else (compare-prerelease (semver-prerelease v1) (semver-prerelease v2))]))

;; Comparison predicates
(define (semver<? v1 v2)
  (< (semver-compare v1 v2) 0))

(define (semver>? v1 v2)
  (> (semver-compare v1 v2) 0))

(define (semver=? v1 v2)
  (= (semver-compare v1 v2) 0))

(define (semver<=? v1 v2)
  (<= (semver-compare v1 v2) 0))

(define (semver>=? v1 v2)
  (>= (semver-compare v1 v2) 0))

;; Bump major version
(define (bump-major v)
  (semver (+ (semver-major v) 1) 0 0 '() ""))

;; Bump minor version
(define (bump-minor v)
  (semver (semver-major v) (+ (semver-minor v) 1) 0 '() ""))

;; Bump patch version
(define (bump-patch v)
  (semver (semver-major v) (semver-minor v) (+ (semver-patch v) 1) '() ""))

;; Set prerelease
(define (with-prerelease v prerelease)
  (semver (semver-major v) (semver-minor v) (semver-patch v)
          (if (string? prerelease) (string-split prerelease ".") prerelease)
          (semver-build-metadata v)))

;; Set build metadata
(define (with-build-metadata v metadata)
  (semver (semver-major v) (semver-minor v) (semver-patch v)
          (semver-prerelease v) metadata))

;; Parse version range (simplified: ^, ~, >=, <=, =)
(define (parse-version-range range-str)
  (let ([trimmed (string-trim range-str)])
    (cond
      [(string-prefix? trimmed "^")
       (let ([v (parse-semver (substring trimmed 1))])
         (if (version-result-ok? v)
             (list 'caret (version-result-value v))
             #f))]
      [(string-prefix? trimmed "~")
       (let ([v (parse-semver (substring trimmed 1))])
         (if (version-result-ok? v)
             (list 'tilde (version-result-value v))
             #f))]
      [(string-prefix? trimmed ">=")
       (let ([v (parse-semver (substring trimmed 2))])
         (if (version-result-ok? v)
             (list 'gte (version-result-value v))
             #f))]
      [(string-prefix? trimmed "<=")
       (let ([v (parse-semver (substring trimmed 2))])
         (if (version-result-ok? v)
             (list 'lte (version-result-value v))
             #f))]
      [(string-prefix? trimmed ">")
       (let ([v (parse-semver (substring trimmed 1))])
         (if (version-result-ok? v)
             (list 'gt (version-result-value v))
             #f))]
      [(string-prefix? trimmed "<")
       (let ([v (parse-semver (substring trimmed 1))])
         (if (version-result-ok? v)
             (list 'lt (version-result-value v))
             #f))]
      [(string-prefix? trimmed "=")
       (let ([v (parse-semver (substring trimmed 1))])
         (if (version-result-ok? v)
             (list 'eq (version-result-value v))
             #f))]
      [else
       (let ([v (parse-semver trimmed)])
         (if (version-result-ok? v)
             (list 'eq (version-result-value v))
             #f))])))

;; Check if version satisfies a range
(define (satisfies-range? version range)
  (match range
    [(list 'eq v) (semver=? version v)]
    [(list 'gt v) (semver>? version v)]
    [(list 'gte v) (semver>=? version v)]
    [(list 'lt v) (semver<? version v)]
    [(list 'lte v) (semver<=? version v)]
    [(list 'caret v)
     ;; ^1.2.3 means >=1.2.3 <2.0.0 (for major >= 1)
     (and (semver>=? version v)
          (< (semver-major version) (+ (semver-major v) 1)))]
    [(list 'tilde v)
     ;; ~1.2.3 means >=1.2.3 <1.3.0
     (and (semver>=? version v)
          (= (semver-major version) (semver-major v))
          (= (semver-minor version) (semver-minor v)))]
    [_ #f]))
