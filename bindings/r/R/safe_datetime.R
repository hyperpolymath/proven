# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe datetime parsing and validation.
#'
#' Provides date and time validation, parsing, and manipulation
#' with proper bounds checking and leap year handling.

#' Create a Date object.
#'
#' @param year Year (e.g., 2025)
#' @param month Month (1-12)
#' @param day Day (1-31)
#' @return SafeDate S3 object
#' @keywords internal
new_safe_date <- function(year, month, day) {
  structure(
    list(year = as.integer(year), month = as.integer(month), day = as.integer(day)),
    class = "SafeDate"
  )
}

#' Create a Time object.
#'
#' @param hour Hour (0-23)
#' @param minute Minute (0-59)
#' @param second Second (0-59)
#' @param nanos Nanoseconds (default 0)
#' @return SafeTime S3 object
#' @keywords internal
new_safe_time <- function(hour, minute, second, nanos = 0L) {
  structure(
    list(
      hour = as.integer(hour),
      minute = as.integer(minute),
      second = as.integer(second),
      nanos = as.integer(nanos)
    ),
    class = "SafeTime"
  )
}

#' Check if object is a SafeDate.
#'
#' @param x Object to check
#' @return TRUE if x is a SafeDate object
#' @export
is_safe_date <- function(x) {
  inherits(x, "SafeDate")
}

#' Check if object is a SafeTime.
#'
#' @param x Object to check
#' @return TRUE if x is a SafeTime object
#' @export
is_safe_time <- function(x) {
  inherits(x, "SafeTime")
}

#' Format SafeDate object.
#'
#' @param x SafeDate object
#' @param ... Additional arguments (ignored)
#' @return Formatted date string (YYYY-MM-DD)
#' @export
format.SafeDate <- function(x, ...) {
  sprintf("%04d-%02d-%02d", x$year, x$month, x$day)
}

#' Print SafeDate object.
#'
#' @param x SafeDate object
#' @param ... Additional arguments (ignored)
#' @export
print.SafeDate <- function(x, ...) {
  cat("SafeDate:", format(x), "\n")
  invisible(x)
}

#' Format SafeTime object.
#'
#' @param x SafeTime object
#' @param ... Additional arguments (ignored)
#' @return Formatted time string (HH:MM:SS)
#' @export
format.SafeTime <- function(x, ...) {
  sprintf("%02d:%02d:%02d", x$hour, x$minute, x$second)
}

#' Print SafeTime object.
#'
#' @param x SafeTime object
#' @param ... Additional arguments (ignored)
#' @export
print.SafeTime <- function(x, ...) {
  cat("SafeTime:", format(x), "\n")
  invisible(x)
}

#' Check if a year is a leap year.
#'
#' @param year Year to check
#' @return TRUE if leap year
#' @export
is_leap_year <- function(year) {
  if (is.na(year)) return(FALSE)
  (year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)
}

#' Get days in a month.
#'
#' @param year Year
#' @param month Month (1-12)
#' @return Number of days or NA if invalid
#' @export
days_in_month <- function(year, month) {
  if (is.na(year) || is.na(month)) return(NA_integer_)
  if (month < 1 || month > 12) return(NA_integer_)

  days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

  if (month == 2 && is_leap_year(year)) {
    return(29L)
  }

  days[month]
}

#' Validate a date.
#'
#' @param year Year
#' @param month Month (1-12)
#' @param day Day (1-31)
#' @return SafeDate object or NULL if invalid
#' @export
validate_date <- function(year, month, day) {
  if (is.na(year) || is.na(month) || is.na(day)) {
    return(NULL)
  }

  if (month < 1 || month > 12) {
    return(NULL)
  }

  max_day <- days_in_month(year, month)
  if (is.na(max_day) || day < 1 || day > max_day) {
    return(NULL)
  }

  new_safe_date(year, month, day)
}

#' Validate a time.
#'
#' @param hour Hour (0-23)
#' @param minute Minute (0-59)
#' @param second Second (0-59)
#' @return SafeTime object or NULL if invalid
#' @export
validate_time <- function(hour, minute, second) {
  if (is.na(hour) || is.na(minute) || is.na(second)) {
    return(NULL)
  }

  if (hour < 0 || hour > 23) {
    return(NULL)
  }

  if (minute < 0 || minute > 59) {
    return(NULL)
  }

  if (second < 0 || second > 59) {
    return(NULL)
  }

  new_safe_time(hour, minute, second)
}

#' Parse ISO 8601 date (YYYY-MM-DD).
#'
#' @param date_string Date string
#' @return SafeDate object or NULL on parse error
#' @export
parse_iso_date <- function(date_string) {
  if (is.na(date_string) || is.null(date_string)) {
    return(NULL)
  }

  parts <- strsplit(trimws(date_string), "-", fixed = TRUE)[[1]]
  if (length(parts) != 3) {
    return(NULL)
  }

  year <- suppressWarnings(as.integer(parts[1]))
  month <- suppressWarnings(as.integer(parts[2]))
  day <- suppressWarnings(as.integer(parts[3]))

  if (is.na(year) || is.na(month) || is.na(day)) {
    return(NULL)
  }

  validate_date(year, month, day)
}

#' Parse ISO 8601 time (HH:MM:SS or HH:MM).
#'
#' @param time_string Time string
#' @return SafeTime object or NULL on parse error
#' @export
parse_iso_time <- function(time_string) {
  if (is.na(time_string) || is.null(time_string)) {
    return(NULL)
  }

  # Remove fractional seconds and timezone
  time_clean <- sub("\\..*", "", trimws(time_string))
  time_clean <- sub("[Z+-].*", "", time_clean)

  parts <- strsplit(time_clean, ":", fixed = TRUE)[[1]]
  if (length(parts) < 2 || length(parts) > 3) {
    return(NULL)
  }

  hour <- suppressWarnings(as.integer(parts[1]))
  minute <- suppressWarnings(as.integer(parts[2]))
  second <- if (length(parts) > 2) suppressWarnings(as.integer(parts[3])) else 0L

  if (is.na(hour) || is.na(minute) || is.na(second)) {
    return(NULL)
  }

  validate_time(hour, minute, second)
}

#' Parse ISO 8601 datetime.
#'
#' @param datetime_string Datetime string
#' @return List with date and time components or NULL
#' @export
parse_iso_datetime <- function(datetime_string) {
  if (is.na(datetime_string) || is.null(datetime_string)) {
    return(NULL)
  }

  # Split on T or space
  parts <- strsplit(trimws(datetime_string), "[T ]")[[1]]
  if (length(parts) < 1 || length(parts) > 2) {
    return(NULL)
  }

  date_part <- parse_iso_date(parts[1])
  if (is.null(date_part)) {
    return(NULL)
  }

  time_part <- if (length(parts) > 1) parse_iso_time(parts[2]) else new_safe_time(0, 0, 0)
  if (is.null(time_part)) {
    return(NULL)
  }

  list(date = date_part, time = time_part)
}

#' Format date as ISO 8601.
#'
#' @param date SafeDate object
#' @return ISO 8601 date string or NA
#' @export
format_iso_date <- function(date) {
  if (is.null(date) || !is_safe_date(date)) {
    return(NA_character_)
  }

  sprintf("%04d-%02d-%02d", date$year, date$month, date$day)
}

#' Format time as ISO 8601.
#'
#' @param time SafeTime object
#' @return ISO 8601 time string or NA
#' @export
format_iso_time <- function(time) {
  if (is.null(time) || !is_safe_time(time)) {
    return(NA_character_)
  }

  sprintf("%02d:%02d:%02d", time$hour, time$minute, time$second)
}

#' Get day of week (1 = Monday, 7 = Sunday).
#'
#' Uses Zeller's formula.
#'
#' @param date SafeDate object or parsed from string
#' @return Day of week (1-7) or NA
#' @export
day_of_week <- function(date) {
  if (is.character(date)) {
    date <- parse_iso_date(date)
  }

  if (is.null(date) || !is_safe_date(date)) {
    return(NA_integer_)
  }

  y <- date$year
  m <- date$month
  d <- date$day

  # Adjust for January and February
  if (m < 3) {
    m <- m + 12
    y <- y - 1
  }

  k <- y %% 100
  j <- y %/% 100

  h <- (d + (13 * (m + 1)) %/% 5 + k + k %/% 4 + j %/% 4 - 2 * j) %% 7

  # Convert from Zeller's (0 = Saturday) to ISO (1 = Monday)
  ((h + 5) %% 7) + 1L
}

#' Get day name.
#'
#' @param dow Day of week (1-7)
#' @return Day name or NA
#' @export
day_name <- function(dow) {
  if (is.na(dow) || dow < 1 || dow > 7) {
    return(NA_character_)
  }

  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")[dow]
}

#' Get month name.
#'
#' @param month Month (1-12)
#' @return Month name or NA
#' @export
month_name <- function(month) {
  if (is.na(month) || month < 1 || month > 12) {
    return(NA_character_)
  }

  c("January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December")[month]
}

#' Check if date is weekend.
#'
#' @param date SafeDate object or date string
#' @return TRUE if Saturday or Sunday
#' @export
is_weekend <- function(date) {
  dow <- day_of_week(date)
  !is.na(dow) && dow >= 6
}

#' Check if date is weekday.
#'
#' @param date SafeDate object or date string
#' @return TRUE if Monday-Friday
#' @export
is_weekday <- function(date) {
  dow <- day_of_week(date)
  !is.na(dow) && dow <= 5
}

#' Add days to a date.
#'
#' @param date SafeDate object
#' @param days Number of days to add (can be negative)
#' @return New SafeDate object or NULL
#' @export
add_days <- function(date, days) {
  if (is.null(date) || !is_safe_date(date) || is.na(days)) {
    return(NULL)
  }

  # Convert to R Date, add, convert back
  r_date <- as.Date(sprintf("%04d-%02d-%02d", date$year, date$month, date$day))
  new_r_date <- r_date + days

  parts <- as.integer(strsplit(format(new_r_date, "%Y-%m-%d"), "-")[[1]])
  new_safe_date(parts[1], parts[2], parts[3])
}

#' Calculate days between two dates.
#'
#' @param date1 First SafeDate object
#' @param date2 Second SafeDate object
#' @return Number of days (date2 - date1) or NA
#' @export
days_between <- function(date1, date2) {
  if (is.null(date1) || !is_safe_date(date1) ||
      is.null(date2) || !is_safe_date(date2)) {
    return(NA_integer_)
  }

  r_date1 <- as.Date(sprintf("%04d-%02d-%02d", date1$year, date1$month, date1$day))
  r_date2 <- as.Date(sprintf("%04d-%02d-%02d", date2$year, date2$month, date2$day))

  as.integer(r_date2 - r_date1)
}

#' Compare two dates.
#'
#' @param date1 First SafeDate object
#' @param date2 Second SafeDate object
#' @return -1 if date1 < date2, 0 if equal, 1 if date1 > date2, NA if invalid
#' @export
compare_dates <- function(date1, date2) {
  diff <- days_between(date1, date2)
  if (is.na(diff)) {
    return(NA_integer_)
  }

  if (diff > 0) return(-1L)
  if (diff < 0) return(1L)
  0L
}

#' Get current date as SafeDate.
#'
#' @return SafeDate object
#' @export
current_date <- function() {
  today <- Sys.Date()
  parts <- as.integer(strsplit(format(today, "%Y-%m-%d"), "-")[[1]])
  new_safe_date(parts[1], parts[2], parts[3])
}

#' Get current time as SafeTime.
#'
#' @return SafeTime object
#' @export
current_time <- function() {
  now <- Sys.time()
  hour <- as.integer(format(now, "%H"))
  minute <- as.integer(format(now, "%M"))
  second <- as.integer(format(now, "%S"))
  new_safe_time(hour, minute, second)
}
