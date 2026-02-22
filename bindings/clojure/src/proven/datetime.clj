;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeDateTime - JNA wrapper for proven_datetime_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.datetime
  "Safe date/time operations via libproven JNA FFI."
  (:require [proven.native :as n]))

(defn leap-year?
  "Check if a year is a leap year."
  [^int year]
  (n/call-bool "proven_datetime_is_leap_year" (int year)))

(defn days-in-month
  "Get the number of days in a given month."
  [^int year ^byte month]
  (int (n/call-int "proven_datetime_days_in_month" (int year) (byte month))))
