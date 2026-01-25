;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.safe-datetime
  "Safe date and time operations."
  (:require [clojure.string :as str])
  (:import [java.time Instant LocalDate LocalDateTime ZonedDateTime Duration Period
            ZoneId ZoneOffset DateTimeException]
           [java.time.format DateTimeFormatter DateTimeParseException]
           [java.time.temporal ChronoUnit]))

(def iso8601-formatter DateTimeFormatter/ISO_INSTANT)
(def date-formatter DateTimeFormatter/ISO_LOCAL_DATE)
(def datetime-formatter DateTimeFormatter/ISO_LOCAL_DATE_TIME)

(defn parse-iso8601
  "Parse an ISO 8601 timestamp.
   Returns {:ok Instant} or {:error message}."
  [timestamp-string]
  (if (str/blank? timestamp-string)
    {:error "empty_timestamp"}
    (try
      {:ok (Instant/parse timestamp-string)}
      (catch DateTimeParseException _
        {:error "invalid_iso8601_format"}))))

(defn parse-date
  "Parse a date string (YYYY-MM-DD).
   Returns {:ok LocalDate} or {:error message}."
  [date-string]
  (if (str/blank? date-string)
    {:error "empty_date"}
    (try
      {:ok (LocalDate/parse date-string date-formatter)}
      (catch DateTimeParseException _
        {:error "invalid_date_format"}))))

(defn parse-datetime
  "Parse a datetime string (ISO local datetime).
   Returns {:ok LocalDateTime} or {:error message}."
  [datetime-string]
  (if (str/blank? datetime-string)
    {:error "empty_datetime"}
    (try
      {:ok (LocalDateTime/parse datetime-string datetime-formatter)}
      (catch DateTimeParseException _
        {:error "invalid_datetime_format"}))))

(defn format-iso8601
  "Format an Instant as ISO 8601 string."
  [instant]
  (.format iso8601-formatter instant))

(defn format-date
  "Format a LocalDate as YYYY-MM-DD string."
  [local-date]
  (.format date-formatter local-date))

(defn format-datetime
  "Format a LocalDateTime as ISO string."
  [local-datetime]
  (.format datetime-formatter local-datetime))

(defn now
  "Get current instant."
  []
  (Instant/now))

(defn today
  "Get today's date."
  []
  (LocalDate/now))

(defn now-local
  "Get current local datetime."
  []
  (LocalDateTime/now))

(defn epoch-millis
  "Get epoch milliseconds from an Instant."
  [instant]
  (.toEpochMilli instant))

(defn from-epoch-millis
  "Create Instant from epoch milliseconds."
  [millis]
  (Instant/ofEpochMilli millis))

(defn epoch-seconds
  "Get epoch seconds from an Instant."
  [instant]
  (.getEpochSecond instant))

(defn from-epoch-seconds
  "Create Instant from epoch seconds."
  [seconds]
  (Instant/ofEpochSecond seconds))

(defn add-duration
  "Add a duration to an Instant.
   Returns {:ok Instant} or {:error message}."
  [instant duration]
  (try
    {:ok (.plus instant duration)}
    (catch DateTimeException e
      {:error (str "duration_overflow: " (.getMessage e))})))

(defn add-days
  "Add days to a LocalDate."
  [local-date days]
  (.plusDays local-date days))

(defn add-months
  "Add months to a LocalDate."
  [local-date months]
  (.plusMonths local-date months))

(defn add-years
  "Add years to a LocalDate."
  [local-date years]
  (.plusYears local-date years))

(defn duration-between
  "Get duration between two Instants."
  [start-instant end-instant]
  (Duration/between start-instant end-instant))

(defn period-between
  "Get period between two LocalDates."
  [start-date end-date]
  (Period/between start-date end-date))

(defn days-between
  "Get number of days between two dates."
  [start-date end-date]
  (.until start-date end-date ChronoUnit/DAYS))

(defn before?
  "Check if first instant/date is before second."
  [a b]
  (.isBefore a b))

(defn after?
  "Check if first instant/date is after second."
  [a b]
  (.isAfter a b))

(defn between?
  "Check if instant/date is between start and end (inclusive)."
  [value start end]
  (and (not (.isBefore value start))
       (not (.isAfter value end))))

(defn is-leap-year?
  "Check if year is a leap year."
  [year]
  (or (and (zero? (mod year 4))
           (not (zero? (mod year 100))))
      (zero? (mod year 400))))

(defn days-in-month
  "Get number of days in a given month."
  [year month]
  (.lengthOfMonth (LocalDate/of year month 1)))

(defn start-of-day
  "Get start of day for a LocalDate."
  [local-date]
  (.atStartOfDay local-date))

(defn end-of-day
  "Get end of day for a LocalDate (23:59:59.999999999)."
  [local-date]
  (.atTime local-date 23 59 59 999999999))

(defn start-of-month
  "Get first day of the month."
  [local-date]
  (.withDayOfMonth local-date 1))

(defn end-of-month
  "Get last day of the month."
  [local-date]
  (.withDayOfMonth local-date (.lengthOfMonth local-date)))

(defn start-of-year
  "Get first day of the year."
  [local-date]
  (LocalDate/of (.getYear local-date) 1 1))

(defn end-of-year
  "Get last day of the year."
  [local-date]
  (LocalDate/of (.getYear local-date) 12 31))

(defn to-zone
  "Convert Instant to ZonedDateTime in a specific zone."
  [instant zone-id-string]
  (try
    {:ok (.atZone instant (ZoneId/of zone-id-string))}
    (catch Exception _
      {:error "invalid_zone_id"})))

(defn parse-duration
  "Parse an ISO 8601 duration string (e.g., PT1H30M).
   Returns {:ok Duration} or {:error message}."
  [duration-string]
  (try
    {:ok (Duration/parse duration-string)}
    (catch DateTimeParseException _
      {:error "invalid_duration_format"})))

(defn format-duration
  "Format a Duration as ISO 8601 string."
  [duration]
  (.toString duration))

(defn is-weekend?
  "Check if a date falls on a weekend."
  [local-date]
  (let [day-of-week (.getValue (.getDayOfWeek local-date))]
    (>= day-of-week 6)))

(defn is-weekday?
  "Check if a date falls on a weekday."
  [local-date]
  (not (is-weekend? local-date)))

(defn age-in-years
  "Calculate age in complete years from birth date to reference date."
  [birth-date reference-date]
  (.getYears (Period/between birth-date reference-date)))

(defn valid-date?
  "Check if year, month, day form a valid date."
  [year month day]
  (try
    (LocalDate/of year month day)
    true
    (catch DateTimeException _ false)))
