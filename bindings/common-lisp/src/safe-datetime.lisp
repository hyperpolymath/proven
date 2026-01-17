;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven SafeDatetime - Safe datetime parsing and manipulation for Common Lisp

(in-package #:proven)

;;; Constants
(defconstant +datetime-epoch+ 0
  "Unix epoch (January 1, 1970 00:00:00 UTC).")

;;; Datetime structure
(defstruct datetime
  "Represents a date and time with timezone."
  (year 1970 :type integer)
  (month 1 :type (integer 1 12))
  (day 1 :type (integer 1 31))
  (hour 0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 59))
  (millisecond 0 :type (integer 0 999))
  (timezone nil :type (or null string)))

;;; Datetime result structure
(defstruct datetime-result
  "Result of datetime parsing operation."
  (datetime nil :type (or null datetime))
  (error nil :type (or null string))
  (ok-p nil :type boolean))

;;; Validation helpers
(defun leap-year-p (year)
  "Check if YEAR is a leap year."
  (or (and (zerop (mod year 4))
           (not (zerop (mod year 100))))
      (zerop (mod year 400))))

(defun days-in-month (year month)
  "Return the number of days in MONTH of YEAR."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))
    (otherwise 0)))

(defun datetime-valid-p (datetime-obj)
  "Check if a datetime structure is valid."
  (and datetime-obj
       (>= (datetime-year datetime-obj) 1)
       (<= 1 (datetime-month datetime-obj) 12)
       (<= 1 (datetime-day datetime-obj) (days-in-month (datetime-year datetime-obj)
                                                         (datetime-month datetime-obj)))
       (<= 0 (datetime-hour datetime-obj) 23)
       (<= 0 (datetime-minute datetime-obj) 59)
       (<= 0 (datetime-second datetime-obj) 59)
       (<= 0 (datetime-millisecond datetime-obj) 999)))

;;; Day calculations
(defun day-of-year (year month day)
  "Calculate day of year (1-366) for given date."
  (let ((days-before-month #(0 31 59 90 120 151 181 212 243 273 304 334)))
    (+ (aref days-before-month (1- month))
       day
       (if (and (leap-year-p year) (> month 2)) 1 0))))

(defun day-of-week (year month day)
  "Calculate day of week (0=Sunday, 6=Saturday) using Zeller's formula."
  (when (< month 3)
    (decf month 12)
    (decf year))
  (let* ((q day)
         (m month)
         (k (mod year 100))
         (j (floor year 100)))
    (mod (+ q
            (floor (* 13 (1+ m)) 5)
            k
            (floor k 4)
            (floor j 4)
            (* -2 j))
         7)))

(defun week-of-year (year month day)
  "Calculate ISO week number (1-53)."
  (let* ((doy (day-of-year year month day))
         (dow (day-of-week year month day))
         ;; Adjust for Monday-based week
         (dow-monday (if (zerop dow) 6 (1- dow)))
         (jan1-dow (day-of-week year 1 1))
         (jan1-dow-monday (if (zerop jan1-dow) 6 (1- jan1-dow)))
         (week (ceiling (/ (+ doy (- 7 dow-monday) jan1-dow-monday) 7))))
    (max 1 (min week 53))))

;;; Unix timestamp conversion
(defun unix-timestamp (datetime-obj)
  "Convert datetime to Unix timestamp (seconds since epoch)."
  (let* ((year (datetime-year datetime-obj))
         (month (datetime-month datetime-obj))
         (day (datetime-day datetime-obj))
         (hour (datetime-hour datetime-obj))
         (minute (datetime-minute datetime-obj))
         (second (datetime-second datetime-obj))
         ;; Days from 1970 to year
         (years-passed (- year 1970))
         (leap-years (- (floor (1- year) 4) (floor 1969 4)
                        (- (floor (1- year) 100) (floor 1969 100))
                        (- (floor (1- year) 400) (floor 1969 400))))
         (days (+ (* years-passed 365) leap-years (1- (day-of-year year month day)))))
    (+ (* days 86400)
       (* hour 3600)
       (* minute 60)
       second)))

(defun from-unix-timestamp (timestamp)
  "Convert Unix timestamp to datetime."
  (multiple-value-bind (remaining-seconds second) (floor timestamp 60)
    (multiple-value-bind (remaining-minutes minute) (floor remaining-seconds 60)
      (multiple-value-bind (days hour) (floor remaining-minutes 24)
        (let* ((year 1970)
               (remaining-days days))
          ;; Find year
          (loop while (>= remaining-days (if (leap-year-p year) 366 365))
                do (decf remaining-days (if (leap-year-p year) 366 365))
                   (incf year))
          ;; Find month and day
          (let ((month 1))
            (loop while (> remaining-days (days-in-month year month))
                  do (decf remaining-days (days-in-month year month))
                     (incf month))
            (make-datetime :year year
                           :month month
                           :day (1+ remaining-days)
                           :hour hour
                           :minute minute
                           :second second)))))))

;;; Parsing
(defun parse-iso8601 (iso-string)
  "Parse an ISO 8601 datetime string.
   Returns a DATETIME-RESULT."
  (handler-case
      (let* ((working-string iso-string)
             (timezone nil)
             (millisecond 0))
        ;; Extract timezone
        (let ((tz-pos (or (position #\Z working-string)
                         (position #\+ working-string :start 10)
                         (position #\- working-string :start 10))))
          (when tz-pos
            (setf timezone (subseq working-string tz-pos))
            (setf working-string (subseq working-string 0 tz-pos))))
        ;; Extract milliseconds
        (let ((dot-pos (position #\. working-string)))
          (when dot-pos
            (let ((ms-string (subseq working-string (1+ dot-pos))))
              (setf millisecond (parse-integer (subseq ms-string 0 (min 3 (length ms-string)))))
              (setf working-string (subseq working-string 0 dot-pos)))))
        ;; Parse main datetime
        (let* ((parts (split-string working-string #\T))
               (date-part (first parts))
               (time-part (or (second parts) "00:00:00"))
               (date-components (split-string date-part #\-))
               (time-components (split-string time-part #\:)))
          (make-datetime-result
           :datetime (make-datetime
                      :year (parse-integer (first date-components))
                      :month (parse-integer (second date-components))
                      :day (parse-integer (third date-components))
                      :hour (parse-integer (first time-components))
                      :minute (parse-integer (or (second time-components) "0"))
                      :second (parse-integer (or (third time-components) "0"))
                      :millisecond millisecond
                      :timezone timezone)
           :ok-p t)))
    (error (condition-object)
      (make-datetime-result :error (format nil "Parse error: ~A" condition-object) :ok-p nil))))

(defun parse-datetime (datetime-string &optional (format-string nil))
  "Parse a datetime string. Uses ISO 8601 by default."
  (declare (ignore format-string))
  (parse-iso8601 datetime-string))

;;; Formatting
(defun format-iso8601 (datetime-obj)
  "Format datetime as ISO 8601 string."
  (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~@[.~3,'0D~]~@[~A~]"
          (datetime-year datetime-obj)
          (datetime-month datetime-obj)
          (datetime-day datetime-obj)
          (datetime-hour datetime-obj)
          (datetime-minute datetime-obj)
          (datetime-second datetime-obj)
          (when (plusp (datetime-millisecond datetime-obj))
            (datetime-millisecond datetime-obj))
          (datetime-timezone datetime-obj)))

(defun format-rfc2822 (datetime-obj)
  "Format datetime as RFC 2822 string."
  (let* ((day-names #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
         (month-names #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
         (dow (day-of-week (datetime-year datetime-obj)
                           (datetime-month datetime-obj)
                           (datetime-day datetime-obj))))
    (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D ~A"
            (aref day-names dow)
            (datetime-day datetime-obj)
            (aref month-names (1- (datetime-month datetime-obj)))
            (datetime-year datetime-obj)
            (datetime-hour datetime-obj)
            (datetime-minute datetime-obj)
            (datetime-second datetime-obj)
            (or (datetime-timezone datetime-obj) "+0000"))))

(defun format-datetime (datetime-obj &optional (format-string nil))
  "Format datetime as string. Uses ISO 8601 by default."
  (declare (ignore format-string))
  (format-iso8601 datetime-obj))

;;; Arithmetic
(defun datetime-add (datetime-obj &key (years 0) (months 0) (days 0)
                                       (hours 0) (minutes 0) (seconds 0))
  "Add duration to datetime and return new datetime."
  (let* ((new-dt (copy-datetime datetime-obj))
         (timestamp (unix-timestamp new-dt))
         (added-seconds (+ (* days 86400) (* hours 3600) (* minutes 60) seconds)))
    ;; Handle years and months separately
    (when (or (plusp years) (plusp months))
      (incf (datetime-year new-dt) years)
      (incf (datetime-month new-dt) months)
      (when (> (datetime-month new-dt) 12)
        (incf (datetime-year new-dt) (floor (1- (datetime-month new-dt)) 12))
        (setf (datetime-month new-dt) (1+ (mod (1- (datetime-month new-dt)) 12))))
      ;; Clamp day
      (setf (datetime-day new-dt)
            (min (datetime-day new-dt)
                 (days-in-month (datetime-year new-dt) (datetime-month new-dt))))
      (setf timestamp (unix-timestamp new-dt)))
    ;; Add seconds-based components
    (from-unix-timestamp (+ timestamp added-seconds))))

(defun datetime-subtract (datetime-obj &key (years 0) (months 0) (days 0)
                                             (hours 0) (minutes 0) (seconds 0))
  "Subtract duration from datetime and return new datetime."
  (datetime-add datetime-obj
                :years (- years)
                :months (- months)
                :days (- days)
                :hours (- hours)
                :minutes (- minutes)
                :seconds (- seconds)))

(defun datetime-diff (datetime-a datetime-b)
  "Calculate difference between two datetimes in seconds."
  (- (unix-timestamp datetime-a) (unix-timestamp datetime-b)))

(defun datetime-compare (datetime-a datetime-b)
  "Compare two datetimes. Returns -1, 0, or 1."
  (let ((diff (datetime-diff datetime-a datetime-b)))
    (cond
      ((< diff 0) -1)
      ((> diff 0) 1)
      (t 0))))
