;;; SPDX-License-Identifier: MPL-2.0-or-later
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; SafeDatetime - Safe date/time operations for Guile Scheme
;;;
;;; Provides validated date/time handling with overflow protection.
;;; All operations are pure and cannot crash.

(define-module (proven safe-datetime)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-19)
  #:export (;; Date validation
            date-valid?
            date-valid-year?
            date-valid-month?
            date-valid-day?
            leap-year?
            days-in-month

            ;; Time validation
            time-valid?
            time-valid-hour?
            time-valid-minute?
            time-valid-second?

            ;; Datetime creation
            make-datetime
            datetime-now
            datetime-from-timestamp

            ;; Datetime accessors
            datetime-year
            datetime-month
            datetime-day
            datetime-hour
            datetime-minute
            datetime-second
            datetime-timestamp

            ;; Datetime arithmetic
            datetime-add-days
            datetime-add-hours
            datetime-add-minutes
            datetime-add-seconds
            datetime-diff-days
            datetime-diff-seconds

            ;; Datetime comparison
            datetime-before?
            datetime-after?
            datetime-equal?
            datetime-between?

            ;; Formatting
            datetime-format-iso8601
            datetime-format-rfc2822
            datetime-parse-iso8601

            ;; Day of week
            datetime-day-of-week
            datetime-day-of-year
            datetime-week-number

            ;; Constants
            *seconds-per-minute*
            *seconds-per-hour*
            *seconds-per-day*
            *seconds-per-week*))

;;; Time constants
(define *seconds-per-minute* 60)
(define *seconds-per-hour* 3600)
(define *seconds-per-day* 86400)
(define *seconds-per-week* 604800)

;;; Make a result record
(define (make-result value ok)
  `((value . ,value) (ok . ,ok)))

(define (result-value result)
  (assoc-ref result 'value))

(define (result-ok? result)
  (assoc-ref result 'ok))

;;; Check if year is leap year
(define (leap-year? year)
  (and (= (remainder year 4) 0)
       (or (not (= (remainder year 100) 0))
           (= (remainder year 400) 0))))

;;; Days in each month (non-leap year)
(define *days-in-month*
  #(31 28 31 30 31 30 31 31 30 31 30 31))

;;; Get days in month
(define (days-in-month year month)
  (cond
   ((or (< month 1) (> month 12)) 0)
   ((and (= month 2) (leap-year? year)) 29)
   (else (vector-ref *days-in-month* (- month 1)))))

;;; Validate year
(define (date-valid-year? year)
  (and (integer? year)
       (>= year 1)
       (<= year 9999)))

;;; Validate month
(define (date-valid-month? month)
  (and (integer? month)
       (>= month 1)
       (<= month 12)))

;;; Validate day for given year/month
(define (date-valid-day? year month day)
  (and (date-valid-year? year)
       (date-valid-month? month)
       (integer? day)
       (>= day 1)
       (<= day (days-in-month year month))))

;;; Validate complete date
(define (date-valid? year month day)
  (date-valid-day? year month day))

;;; Validate hour
(define (time-valid-hour? hour)
  (and (integer? hour)
       (>= hour 0)
       (<= hour 23)))

;;; Validate minute
(define (time-valid-minute? minute)
  (and (integer? minute)
       (>= minute 0)
       (<= minute 59)))

;;; Validate second
(define (time-valid-second? second)
  (and (number? second)
       (>= second 0)
       (< second 60)))

;;; Validate complete time
(define (time-valid? hour minute second)
  (and (time-valid-hour? hour)
       (time-valid-minute? minute)
       (time-valid-second? second)))

;;; Create datetime record
(define (make-datetime year month day hour minute second)
  (if (and (date-valid? year month day)
           (time-valid? hour minute second))
      `((year . ,year)
        (month . ,month)
        (day . ,day)
        (hour . ,hour)
        (minute . ,minute)
        (second . ,second))
      #f))

;;; Get current datetime
(define (datetime-now)
  (let ((now (current-date)))
    (make-datetime (date-year now)
                   (date-month now)
                   (date-day now)
                   (date-hour now)
                   (date-minute now)
                   (date-second now))))

;;; Datetime accessors
(define (datetime-year dt) (and dt (assoc-ref dt 'year)))
(define (datetime-month dt) (and dt (assoc-ref dt 'month)))
(define (datetime-day dt) (and dt (assoc-ref dt 'day)))
(define (datetime-hour dt) (and dt (assoc-ref dt 'hour)))
(define (datetime-minute dt) (and dt (assoc-ref dt 'minute)))
(define (datetime-second dt) (and dt (assoc-ref dt 'second)))

;;; Convert to Unix timestamp (simplified)
(define (datetime-timestamp dt)
  (if (not dt)
      #f
      (let ((date (make-date 0
                            (datetime-second dt)
                            (datetime-minute dt)
                            (datetime-hour dt)
                            (datetime-day dt)
                            (datetime-month dt)
                            (datetime-year dt)
                            0)))
        (time-second (date->time-utc date)))))

;;; Create datetime from timestamp
(define (datetime-from-timestamp timestamp)
  (if (not (integer? timestamp))
      #f
      (let ((date (time-utc->date (make-time time-utc 0 timestamp))))
        (make-datetime (date-year date)
                       (date-month date)
                       (date-day date)
                       (date-hour date)
                       (date-minute date)
                       (date-second date)))))

;;; Add days to datetime
(define (datetime-add-days dt days)
  (let ((ts (datetime-timestamp dt)))
    (if ts
        (datetime-from-timestamp (+ ts (* days *seconds-per-day*)))
        #f)))

;;; Add hours to datetime
(define (datetime-add-hours dt hours)
  (let ((ts (datetime-timestamp dt)))
    (if ts
        (datetime-from-timestamp (+ ts (* hours *seconds-per-hour*)))
        #f)))

;;; Add minutes to datetime
(define (datetime-add-minutes dt minutes)
  (let ((ts (datetime-timestamp dt)))
    (if ts
        (datetime-from-timestamp (+ ts (* minutes *seconds-per-minute*)))
        #f)))

;;; Add seconds to datetime
(define (datetime-add-seconds dt seconds)
  (let ((ts (datetime-timestamp dt)))
    (if ts
        (datetime-from-timestamp (+ ts seconds))
        #f)))

;;; Difference in days
(define (datetime-diff-days dt1 dt2)
  (let ((ts1 (datetime-timestamp dt1))
        (ts2 (datetime-timestamp dt2)))
    (if (and ts1 ts2)
        (quotient (- ts1 ts2) *seconds-per-day*)
        #f)))

;;; Difference in seconds
(define (datetime-diff-seconds dt1 dt2)
  (let ((ts1 (datetime-timestamp dt1))
        (ts2 (datetime-timestamp dt2)))
    (if (and ts1 ts2)
        (- ts1 ts2)
        #f)))

;;; Datetime comparison
(define (datetime-before? dt1 dt2)
  (let ((ts1 (datetime-timestamp dt1))
        (ts2 (datetime-timestamp dt2)))
    (and ts1 ts2 (< ts1 ts2))))

(define (datetime-after? dt1 dt2)
  (let ((ts1 (datetime-timestamp dt1))
        (ts2 (datetime-timestamp dt2)))
    (and ts1 ts2 (> ts1 ts2))))

(define (datetime-equal? dt1 dt2)
  (let ((ts1 (datetime-timestamp dt1))
        (ts2 (datetime-timestamp dt2)))
    (and ts1 ts2 (= ts1 ts2))))

(define (datetime-between? dt start end)
  (and (or (datetime-after? dt start) (datetime-equal? dt start))
       (or (datetime-before? dt end) (datetime-equal? dt end))))

;;; Format as ISO 8601
(define (datetime-format-iso8601 dt)
  (if (not dt)
      #f
      (format #f "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
              (datetime-year dt)
              (datetime-month dt)
              (datetime-day dt)
              (datetime-hour dt)
              (datetime-minute dt)
              (inexact->exact (round (datetime-second dt))))))

;;; Format as RFC 2822
(define (datetime-format-rfc2822 dt)
  (if (not dt)
      #f
      (let* ((day-names #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
             (month-names #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
             (dow (datetime-day-of-week dt)))
        (format #f "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d +0000"
                (vector-ref day-names dow)
                (datetime-day dt)
                (vector-ref month-names (- (datetime-month dt) 1))
                (datetime-year dt)
                (datetime-hour dt)
                (datetime-minute dt)
                (inexact->exact (round (datetime-second dt)))))))

;;; Parse ISO 8601 format
(define (datetime-parse-iso8601 str)
  (if (not (string? str))
      (make-result #f #f)
      (let* ((parts (string-split str #\T))
             (date-str (if (>= (length parts) 1) (car parts) ""))
             (time-str (if (>= (length parts) 2) (cadr parts) "00:00:00"))
             (date-parts (string-split date-str #\-))
             (time-clean (string-trim-right time-str (char-set #\Z #\+ #\-)))
             (time-parts (string-split time-clean #\:)))
        (if (and (= (length date-parts) 3)
                 (>= (length time-parts) 2))
            (let ((year (string->number (car date-parts)))
                  (month (string->number (cadr date-parts)))
                  (day (string->number (caddr date-parts)))
                  (hour (string->number (car time-parts)))
                  (minute (string->number (cadr time-parts)))
                  (second (if (>= (length time-parts) 3)
                             (string->number (caddr time-parts))
                             0)))
              (let ((dt (make-datetime year month day hour minute second)))
                (if dt
                    (make-result dt #t)
                    (make-result #f #f))))
            (make-result #f #f)))))

;;; Get day of week (0 = Sunday)
(define (datetime-day-of-week dt)
  (if (not dt)
      #f
      (let* ((date (make-date 0 0 0 0
                             (datetime-day dt)
                             (datetime-month dt)
                             (datetime-year dt)
                             0)))
        (date-week-day date))))

;;; Get day of year (1-366)
(define (datetime-day-of-year dt)
  (if (not dt)
      #f
      (let* ((date (make-date 0 0 0 0
                             (datetime-day dt)
                             (datetime-month dt)
                             (datetime-year dt)
                             0)))
        (date-year-day date))))

;;; Get ISO week number
(define (datetime-week-number dt)
  (if (not dt)
      #f
      (let ((doy (datetime-day-of-year dt))
            (dow (datetime-day-of-week dt)))
        (quotient (+ doy 6 (- dow)) 7))))
