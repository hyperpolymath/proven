;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeDatetime - Thin CFFI wrapper for libproven datetime operations.
;;;; All computation delegates to Idris 2 via the Zig FFI layer.

(in-package #:proven)

;;; ============================================================================
;;; C struct types matching the Zig FFI ABI
;;; ============================================================================

;;; DateTime: { year: i32, month: u8, day: u8, hour: u8, minute: u8,
;;;             second: u8, nanosecond: u32, tz_offset_minutes: i16 }
(defcstruct datetime-ffi
  (year :int32)
  (month :uint8)
  (day :uint8)
  (hour :uint8)
  (minute :uint8)
  (second :uint8)
  (nanosecond :uint32)
  (tz-offset-minutes :int16))

;;; DateTimeResult: { status: i32, datetime: DateTime }
(defcstruct datetime-result-ffi
  (status :int32)
  (datetime (:struct datetime-ffi)))

;;; ============================================================================
;;; FFI declarations
;;; ============================================================================

(defcfun ("proven_datetime_parse" %datetime-parse) (:struct datetime-result-ffi)
  (ptr :pointer) (len :size))

(defcfun ("proven_datetime_format_iso8601" %datetime-format-iso8601) (:struct string-result)
  (dt (:struct datetime-ffi)))

(defcfun ("proven_datetime_is_leap_year" %datetime-is-leap-year) :boolean
  (year :int32))

(defcfun ("proven_datetime_days_in_month" %datetime-days-in-month) :uint8
  (year :int32) (month :uint8))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(defun datetime-parse (str)
  "Parse an ISO 8601 datetime string via libproven.
   Returns (values plist ok-p) where plist has keys :YEAR :MONTH :DAY :HOUR
   :MINUTE :SECOND :NANOSECOND :TZ-OFFSET-MINUTES, or (values nil nil) on error."
  (with-foreign-string-buf (ptr len str)
    (let ((result (%datetime-parse ptr len)))
      (if (zerop (getf result 'status))
          (let ((dt (getf result 'datetime)))
            (values (list :year (getf dt 'year)
                          :month (getf dt 'month)
                          :day (getf dt 'day)
                          :hour (getf dt 'hour)
                          :minute (getf dt 'minute)
                          :second (getf dt 'second)
                          :nanosecond (getf dt 'nanosecond)
                          :tz-offset-minutes (getf dt 'tz-offset-minutes))
                    t))
          (values nil nil)))))

(defun datetime-format-iso8601 (datetime-plist)
  "Format a datetime plist as ISO 8601 string via libproven.
   DATETIME-PLIST should have keys :YEAR :MONTH :DAY :HOUR :MINUTE :SECOND
   :NANOSECOND :TZ-OFFSET-MINUTES.
   Returns (values iso-string ok-p) or (values nil nil) on error."
  (extract-string-result
   (%datetime-format-iso8601
    (list 'year (getf datetime-plist :year)
          'month (getf datetime-plist :month)
          'day (getf datetime-plist :day)
          'hour (getf datetime-plist :hour)
          'minute (getf datetime-plist :minute)
          'second (getf datetime-plist :second)
          'nanosecond (or (getf datetime-plist :nanosecond) 0)
          'tz-offset-minutes (or (getf datetime-plist :tz-offset-minutes) 0)))))

(defun datetime-is-leap-year (year)
  "Check if YEAR is a leap year via libproven. Returns a boolean."
  (%datetime-is-leap-year year))

(defun datetime-days-in-month (year month)
  "Return the number of days in MONTH of YEAR via libproven. Returns an integer."
  (%datetime-days-in-month year month))
