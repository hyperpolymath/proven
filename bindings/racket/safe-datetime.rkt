#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;
;; Proven SafeDatetime - Safe datetime operations for Racket
;;

(require racket/date
         racket/format
         racket/string
         racket/match)

(provide
 ;; Safe date/time creation
 safe-date
 safe-datetime

 ;; Validation
 valid-date?
 valid-time?
 valid-datetime?

 ;; Parsing
 parse-iso8601
 parse-date-string

 ;; Formatting
 format-iso8601
 format-date
 format-time

 ;; Comparison
 date-before?
 date-after?
 date-equal?
 datetime-compare

 ;; Arithmetic
 add-days
 add-months
 add-years
 add-hours
 add-minutes
 add-seconds
 date-diff-days

 ;; Utilities
 is-leap-year?
 days-in-month
 day-of-week

 ;; Timestamp operations
 current-timestamp
 timestamp->date
 date->timestamp)

;; Result structure
(struct datetime-result (value ok? error) #:transparent)

;; Check if year is a leap year
(define (is-leap-year? year)
  (or (and (= (modulo year 4) 0)
           (not (= (modulo year 100) 0)))
      (= (modulo year 400) 0)))

;; Days in a given month
(define (days-in-month year month)
  (cond
    [(member month '(1 3 5 7 8 10 12)) 31]
    [(member month '(4 6 9 11)) 30]
    [(= month 2) (if (is-leap-year? year) 29 28)]
    [else 0]))

;; Validate date components
(define (valid-date? year month day)
  (and (integer? year)
       (integer? month)
       (integer? day)
       (>= year 1)
       (<= year 9999)
       (>= month 1)
       (<= month 12)
       (>= day 1)
       (<= day (days-in-month year month))))

;; Validate time components
(define (valid-time? hour minute [second 0])
  (and (integer? hour)
       (integer? minute)
       (or (integer? second) (real? second))
       (>= hour 0)
       (<= hour 23)
       (>= minute 0)
       (<= minute 59)
       (>= second 0)
       (< second 60)))

;; Validate datetime
(define (valid-datetime? year month day hour minute [second 0])
  (and (valid-date? year month day)
       (valid-time? hour minute second)))

;; Create a safe date
(define (safe-date year month day)
  (if (valid-date? year month day)
      (datetime-result (list year month day) #t #f)
      (datetime-result #f #f "Invalid date")))

;; Create a safe datetime
(define (safe-datetime year month day hour minute [second 0])
  (if (valid-datetime? year month day hour minute second)
      (datetime-result (list year month day hour minute second) #t #f)
      (datetime-result #f #f "Invalid datetime")))

;; Parse ISO 8601 date/datetime string
(define (parse-iso8601 str)
  (with-handlers ([exn:fail? (lambda (e)
                               (datetime-result #f #f (exn-message e)))])
    (let ([date-match (regexp-match
                       #rx"^([0-9]{4})-([0-9]{2})-([0-9]{2})(T([0-9]{2}):([0-9]{2}):([0-9]{2})(\\.([0-9]+))?(Z|([+-])([0-9]{2}):([0-9]{2}))?)?$"
                       str)])
      (if date-match
          (let* ([year (string->number (list-ref date-match 1))]
                 [month (string->number (list-ref date-match 2))]
                 [day (string->number (list-ref date-match 3))]
                 [has-time (list-ref date-match 4)]
                 [hour (if has-time (string->number (list-ref date-match 5)) 0)]
                 [minute (if has-time (string->number (list-ref date-match 6)) 0)]
                 [second (if has-time (string->number (list-ref date-match 7)) 0)])
            (if (valid-datetime? year month day hour minute second)
                (datetime-result (list year month day hour minute second) #t #f)
                (datetime-result #f #f "Invalid datetime values")))
          (datetime-result #f #f "Invalid ISO 8601 format")))))

;; Parse simple date string (YYYY-MM-DD)
(define (parse-date-string str)
  (let ([match (regexp-match #rx"^([0-9]{4})-([0-9]{2})-([0-9]{2})$" str)])
    (if match
        (let* ([year (string->number (list-ref match 1))]
               [month (string->number (list-ref match 2))]
               [day (string->number (list-ref match 3))])
          (safe-date year month day))
        (datetime-result #f #f "Invalid date format"))))

;; Format as ISO 8601
(define (format-iso8601 dt)
  (match dt
    [(list year month day hour minute second)
     (format "~a-~a-~aT~a:~a:~aZ"
             (~r year #:min-width 4 #:pad-string "0")
             (~r month #:min-width 2 #:pad-string "0")
             (~r day #:min-width 2 #:pad-string "0")
             (~r hour #:min-width 2 #:pad-string "0")
             (~r minute #:min-width 2 #:pad-string "0")
             (~r (floor second) #:min-width 2 #:pad-string "0"))]
    [(list year month day)
     (format "~a-~a-~a"
             (~r year #:min-width 4 #:pad-string "0")
             (~r month #:min-width 2 #:pad-string "0")
             (~r day #:min-width 2 #:pad-string "0"))]
    [_ ""]))

;; Format date only
(define (format-date dt)
  (match dt
    [(list year month day _ ...)
     (format "~a-~a-~a"
             (~r year #:min-width 4 #:pad-string "0")
             (~r month #:min-width 2 #:pad-string "0")
             (~r day #:min-width 2 #:pad-string "0"))]
    [_ ""]))

;; Format time only
(define (format-time dt)
  (match dt
    [(list _ _ _ hour minute second)
     (format "~a:~a:~a"
             (~r hour #:min-width 2 #:pad-string "0")
             (~r minute #:min-width 2 #:pad-string "0")
             (~r (floor second) #:min-width 2 #:pad-string "0"))]
    [_ ""]))

;; Compare datetimes
(define (datetime-compare dt1 dt2)
  (let loop ([l1 dt1] [l2 dt2])
    (cond
      [(and (null? l1) (null? l2)) 0]
      [(null? l1) -1]
      [(null? l2) 1]
      [(< (car l1) (car l2)) -1]
      [(> (car l1) (car l2)) 1]
      [else (loop (cdr l1) (cdr l2))])))

;; Date comparisons
(define (date-before? dt1 dt2)
  (< (datetime-compare dt1 dt2) 0))

(define (date-after? dt1 dt2)
  (> (datetime-compare dt1 dt2) 0))

(define (date-equal? dt1 dt2)
  (= (datetime-compare dt1 dt2) 0))

;; Add days to date
(define (add-days dt days)
  (match dt
    [(list year month day rest ...)
     (let loop ([y year] [m month] [d (+ day days)])
       (cond
         [(> d (days-in-month y m))
          (loop y (+ m 1) (- d (days-in-month y m)))]
         [(< d 1)
          (let* ([new-m (- m 1)]
                 [actual-m (if (< new-m 1) 12 new-m)]
                 [actual-y (if (< new-m 1) (- y 1) y)])
            (loop actual-y actual-m (+ d (days-in-month actual-y actual-m))))]
         [(> m 12)
          (loop (+ y 1) (- m 12) d)]
         [(< m 1)
          (loop (- y 1) (+ m 12) d)]
         [else (append (list y m d) rest)]))]
    [_ dt]))

;; Add months to date
(define (add-months dt months)
  (match dt
    [(list year month day rest ...)
     (let* ([total-months (+ (* year 12) month -1 months)]
            [new-year (quotient total-months 12)]
            [new-month (+ 1 (modulo total-months 12))]
            [max-day (days-in-month new-year new-month)]
            [new-day (min day max-day)])
       (append (list new-year new-month new-day) rest))]
    [_ dt]))

;; Add years to date
(define (add-years dt years)
  (add-months dt (* years 12)))

;; Add hours
(define (add-hours dt hours)
  (match dt
    [(list year month day hour minute second)
     (let* ([total-hours (+ hour hours)]
            [extra-days (quotient total-hours 24)]
            [new-hour (modulo total-hours 24)]
            [new-date (add-days (list year month day) extra-days)])
       (append new-date (list new-hour minute second)))]
    [_ dt]))

;; Add minutes
(define (add-minutes dt mins)
  (match dt
    [(list year month day hour minute second)
     (let* ([total-mins (+ minute mins)]
            [extra-hours (quotient total-mins 60)]
            [new-min (modulo total-mins 60)])
       (add-hours (list year month day hour new-min second) extra-hours))]
    [_ dt]))

;; Add seconds
(define (add-seconds dt secs)
  (match dt
    [(list year month day hour minute second)
     (let* ([total-secs (+ second secs)]
            [extra-mins (quotient (floor total-secs) 60)]
            [new-sec (modulo total-secs 60)])
       (add-minutes (list year month day hour minute new-sec) extra-mins))]
    [_ dt]))

;; Calculate difference in days between two dates
(define (date-diff-days dt1 dt2)
  (- (date->timestamp dt1) (date->timestamp dt2)))

;; Day of week (0=Sunday, 1=Monday, ...)
(define (day-of-week year month day)
  (let* ([a (quotient (- 14 month) 12)]
         [y (- year a)]
         [m (+ month (* 12 a) -2)])
    (modulo (+ day y (quotient y 4) (- (quotient y 100)) (quotient y 400)
               (quotient (* 31 m) 12))
            7)))

;; Current Unix timestamp in seconds
(define (current-timestamp)
  (current-seconds))

;; Convert timestamp to datetime
(define (timestamp->date timestamp)
  (let ([d (seconds->date timestamp)])
    (list (date-year d) (date-month d) (date-day d)
          (date-hour d) (date-minute d) (date-second d))))

;; Convert datetime to timestamp (days since epoch for dates only)
(define (date->timestamp dt)
  (match dt
    [(list year month day _ ...)
     ;; Simplified: days since year 1
     (+ (* (- year 1) 365)
        (quotient (- year 1) 4)
        (- (quotient (- year 1) 100))
        (quotient (- year 1) 400)
        (for/sum ([m (in-range 1 month)])
          (days-in-month year m))
        day)]
    [_ 0]))
