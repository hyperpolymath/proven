# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe currency operations with proper decimal handling.
#'
#' Provides currency code validation, Money S3 class with arithmetic operations,
#' and safe formatting for financial calculations.

#' Named vector of ISO 4217 currency codes with their decimal places.
#'
#' @export
currency_codes <- c(
  # Major currencies
  USD = 2L, EUR = 2L, GBP = 2L, JPY = 0L, CHF = 2L, AUD = 2L, CAD = 2L,
  NZD = 2L, CNY = 2L, HKD = 2L, SGD = 2L, SEK = 2L, NOK = 2L, DKK = 2L,
  # Additional currencies
  MXN = 2L, BRL = 2L, INR = 2L, RUB = 2L, ZAR = 2L, KRW = 0L, TWD = 2L,
  THB = 2L, MYR = 2L, IDR = 2L, PHP = 2L, VND = 0L, PLN = 2L, CZK = 2L,
  HUF = 2L, TRY = 2L, ILS = 2L, AED = 2L, SAR = 2L, EGP = 2L, NGN = 2L,
  KES = 2L, GHS = 2L, PKR = 2L, BDT = 2L, CLP = 0L, COP = 2L, PEN = 2L,
  ARS = 2L, VES = 2L, UAH = 2L, RON = 2L, BGN = 2L, HRK = 2L, ISK = 0L,
  # Zero-decimal currencies
  BIF = 0L, CLP = 0L, DJF = 0L, GNF = 0L, ISK = 0L, JPY = 0L, KMF = 0L,

KRW = 0L, PYG = 0L, RWF = 0L, UGX = 0L, VND = 0L, VUV = 0L, XAF = 0L,
  XOF = 0L, XPF = 0L,
  # Three-decimal currencies
  BHD = 3L, IQD = 3L, JOD = 3L, KWD = 3L, LYD = 3L, OMR = 3L, TND = 3L,
  # Precious metals and special
  XAU = 6L, XAG = 6L, XPT = 6L, XPD = 6L, BTC = 8L, ETH = 18L
)

#' Get decimal places for a currency code.
#'
#' @param currency_code ISO 4217 currency code
#' @return Number of decimal places or NA if unknown
#' @export
currency_decimals <- function(currency_code) {
  if (is.na(currency_code) || is.null(currency_code)) {
    return(NA_integer_)
  }

  code <- toupper(currency_code)
  if (code %in% names(currency_codes)) {
    currency_codes[[code]]
  } else {
    NA_integer_
  }
}

#' Check if currency code is valid.
#'
#' @param currency_code Currency code to check
#' @return TRUE if valid ISO 4217 code
#' @export
is_valid_currency <- function(currency_code) {
  if (is.na(currency_code) || is.null(currency_code)) {
    return(FALSE)
  }

  toupper(currency_code) %in% names(currency_codes)
}

#' Create a Money object.
#'
#' Money is stored internally as the smallest currency unit (e.g., cents for USD).
#' This avoids floating-point precision issues.
#'
#' @param amount Amount in major units (e.g., dollars)
#' @param currency ISO 4217 currency code
#' @return Money S3 object or NULL on invalid input
#' @export
money <- function(amount, currency) {
  if (is.na(amount) || is.null(amount)) {
    return(NULL)
  }

  currency <- toupper(currency)
  decimals <- currency_decimals(currency)

  if (is.na(decimals)) {
    return(NULL)
  }

  # Convert to minor units (cents, etc.)
  minor_units <- round(amount * (10 ^ decimals))

  structure(
    list(
      minor_units = as.integer(minor_units),
      currency = currency,
      decimals = decimals
    ),
    class = "Money"
  )
}

#' Create Money from minor units.
#'
#' @param minor_units Amount in smallest currency unit
#' @param currency ISO 4217 currency code
#' @return Money S3 object or NULL on invalid input
#' @export
money_from_minor <- function(minor_units, currency) {
  if (is.na(minor_units) || is.null(minor_units)) {
    return(NULL)
  }

  currency <- toupper(currency)
  decimals <- currency_decimals(currency)

  if (is.na(decimals)) {
    return(NULL)
  }

  structure(
    list(
      minor_units = as.integer(minor_units),
      currency = currency,
      decimals = decimals
    ),
    class = "Money"
  )
}

#' Check if object is Money.
#'
#' @param x Object to check
#' @return TRUE if x is a Money object
#' @export
is_money <- function(x) {
  inherits(x, "Money")
}

#' Format Money object.
#'
#' @param x Money object
#' @param ... Additional arguments (ignored)
#' @return Formatted string
#' @export
format.Money <- function(x, ...) {
  format_money(x)
}

#' Print Money object.
#'
#' @param x Money object
#' @param ... Additional arguments (ignored)
#' @export
print.Money <- function(x, ...) {
  cat(format_money(x), "\n")
  invisible(x)
}

#' Format a Money object as a string.
#'
#' @param money_obj Money object
#' @param symbol Currency symbol to use (NULL for code)
#' @param symbol_position "before" or "after" (default "before")
#' @return Formatted money string
#' @export
format_money <- function(money_obj, symbol = NULL, symbol_position = "before") {
  if (is.null(money_obj) || !is_money(money_obj)) {
    return(NA_character_)
  }

  # Convert minor units to major units
  major_value <- money_obj$minor_units / (10 ^ money_obj$decimals)

  # Format with proper decimal places
  formatted_value <- format(
    round(major_value, money_obj$decimals),
    nsmall = money_obj$decimals,
    big.mark = ",",
    scientific = FALSE
  )

  currency_indicator <- if (is.null(symbol)) money_obj$currency else symbol

  if (symbol_position == "after") {
    paste0(formatted_value, " ", currency_indicator)
  } else {
    paste0(currency_indicator, " ", formatted_value)
  }
}

#' Get amount in major units.
#'
#' @param money_obj Money object
#' @return Numeric amount in major units or NA
#' @export
money_amount <- function(money_obj) {
  if (is.null(money_obj) || !is_money(money_obj)) {
    return(NA_real_)
  }

  money_obj$minor_units / (10 ^ money_obj$decimals)
}

#' Get amount in minor units.
#'
#' @param money_obj Money object
#' @return Integer amount in minor units or NA
#' @export
money_minor_units <- function(money_obj) {
  if (is.null(money_obj) || !is_money(money_obj)) {
    return(NA_integer_)
  }

  money_obj$minor_units
}

#' Get currency code.
#'
#' @param money_obj Money object
#' @return Currency code or NA
#' @export
money_currency <- function(money_obj) {
  if (is.null(money_obj) || !is_money(money_obj)) {
    return(NA_character_)
  }

  money_obj$currency
}

#' Add two Money objects.
#'
#' @param a First Money object
#' @param b Second Money object
#' @return Money object or NULL if currencies differ
#' @export
money_add <- function(a, b) {
  if (!is_money(a) || !is_money(b)) {
    return(NULL)
  }

  if (a$currency != b$currency) {
    stop("Cannot add money with different currencies")
  }

  money_from_minor(a$minor_units + b$minor_units, a$currency)
}

#' Subtract two Money objects.
#'
#' @param a First Money object
#' @param b Second Money object
#' @return Money object or NULL if currencies differ
#' @export
money_subtract <- function(a, b) {
  if (!is_money(a) || !is_money(b)) {
    return(NULL)
  }

  if (a$currency != b$currency) {
    stop("Cannot subtract money with different currencies")
  }

  money_from_minor(a$minor_units - b$minor_units, a$currency)
}

#' Multiply Money by a scalar.
#'
#' @param money_obj Money object
#' @param multiplier Numeric multiplier
#' @return Money object
#' @export
money_multiply <- function(money_obj, multiplier) {
  if (!is_money(money_obj) || is.na(multiplier)) {
    return(NULL)
  }

  new_minor <- round(money_obj$minor_units * multiplier)
  money_from_minor(as.integer(new_minor), money_obj$currency)
}

#' Divide Money by a scalar.
#'
#' @param money_obj Money object
#' @param divisor Numeric divisor
#' @return Money object or NULL on division by zero
#' @export
money_divide <- function(money_obj, divisor) {
  if (!is_money(money_obj) || is.na(divisor) || divisor == 0) {
    return(NULL)
  }

  new_minor <- round(money_obj$minor_units / divisor)
  money_from_minor(as.integer(new_minor), money_obj$currency)
}

#' Addition operator for Money.
#'
#' @param e1 First Money object
#' @param e2 Second Money object
#' @return Money object
#' @export
`+.Money` <- function(e1, e2) {
  if (is.numeric(e2)) {
    # Adding a scalar (in major units)
    money_add(e1, money(e2, e1$currency))
  } else {
    money_add(e1, e2)
  }
}

#' Subtraction operator for Money.
#'
#' @param e1 First Money object
#' @param e2 Second Money object
#' @return Money object
#' @export
`-.Money` <- function(e1, e2) {
  if (is.numeric(e2)) {
    money_subtract(e1, money(e2, e1$currency))
  } else {
    money_subtract(e1, e2)
  }
}

#' Multiplication operator for Money.
#'
#' @param e1 Money object
#' @param e2 Numeric multiplier
#' @return Money object
#' @export
`*.Money` <- function(e1, e2) {
  if (is_money(e1) && is.numeric(e2)) {
    money_multiply(e1, e2)
  } else if (is.numeric(e1) && is_money(e2)) {
    money_multiply(e2, e1)
  } else {
    stop("Money can only be multiplied by a numeric value")
  }
}

#' Division operator for Money.
#'
#' @param e1 Money object
#' @param e2 Numeric divisor
#' @return Money object
#' @export
`/.Money` <- function(e1, e2) {
  if (!is.numeric(e2)) {
    stop("Money can only be divided by a numeric value")
  }
  money_divide(e1, e2)
}

#' Compare two Money objects for equality.
#'
#' @param e1 First Money object
#' @param e2 Second Money object
#' @return TRUE if equal
#' @export
`==.Money` <- function(e1, e2) {
  if (!is_money(e1) || !is_money(e2)) {
    return(FALSE)
  }
  e1$currency == e2$currency && e1$minor_units == e2$minor_units
}

#' Compare Money for less than.
#'
#' @param e1 First Money object
#' @param e2 Second Money object
#' @return TRUE if e1 < e2
#' @export
`<.Money` <- function(e1, e2) {
  if (!is_money(e1) || !is_money(e2)) {
    return(NA)
  }
  if (e1$currency != e2$currency) {
    stop("Cannot compare money with different currencies")
  }
  e1$minor_units < e2$minor_units
}

#' Compare Money for greater than.
#'
#' @param e1 First Money object
#' @param e2 Second Money object
#' @return TRUE if e1 > e2
#' @export
`>.Money` <- function(e1, e2) {
  if (!is_money(e1) || !is_money(e2)) {
    return(NA)
  }
  if (e1$currency != e2$currency) {
    stop("Cannot compare money with different currencies")
  }
  e1$minor_units > e2$minor_units
}

#' Compare Money for less than or equal.
#'
#' @param e1 First Money object
#' @param e2 Second Money object
#' @return TRUE if e1 <= e2
#' @export
`<=.Money` <- function(e1, e2) {
  e1 < e2 || e1 == e2
}

#' Compare Money for greater than or equal.
#'
#' @param e1 First Money object
#' @param e2 Second Money object
#' @return TRUE if e1 >= e2
#' @export
`>=.Money` <- function(e1, e2) {
  e1 > e2 || e1 == e2
}

#' Check if Money is negative.
#'
#' @param money_obj Money object
#' @return TRUE if negative
#' @export
money_is_negative <- function(money_obj) {
  if (!is_money(money_obj)) {
    return(NA)
  }
  money_obj$minor_units < 0
}

#' Check if Money is zero.
#'
#' @param money_obj Money object
#' @return TRUE if zero
#' @export
money_is_zero <- function(money_obj) {
  if (!is_money(money_obj)) {
    return(NA)
  }
  money_obj$minor_units == 0
}

#' Check if Money is positive.
#'
#' @param money_obj Money object
#' @return TRUE if positive
#' @export
money_is_positive <- function(money_obj) {
  if (!is_money(money_obj)) {
    return(NA)
  }
  money_obj$minor_units > 0
}

#' Get absolute value of Money.
#'
#' @param money_obj Money object
#' @return Money object with absolute value
#' @export
money_abs <- function(money_obj) {
  if (!is_money(money_obj)) {
    return(NULL)
  }
  money_from_minor(abs(money_obj$minor_units), money_obj$currency)
}

#' Negate Money.
#'
#' @param money_obj Money object
#' @return Negated Money object
#' @export
money_negate <- function(money_obj) {
  if (!is_money(money_obj)) {
    return(NULL)
  }
  money_from_minor(-money_obj$minor_units, money_obj$currency)
}

#' Allocate Money across multiple recipients.
#'
#' Uses largest remainder method to avoid losing pennies.
#'
#' @param money_obj Money object to allocate
#' @param ratios Numeric vector of allocation ratios
#' @return List of Money objects
#' @export
money_allocate <- function(money_obj, ratios) {
  if (!is_money(money_obj) || length(ratios) == 0) {
    return(NULL)
  }

  total_ratio <- sum(ratios)
  if (total_ratio == 0) {
    return(NULL)
  }

  total_minor <- money_obj$minor_units

  # Calculate base allocations
  allocations <- floor(ratios / total_ratio * total_minor)

  # Distribute remainder using largest remainder method
  remainder <- total_minor - sum(allocations)
  remainders <- (ratios / total_ratio * total_minor) - allocations

  while (remainder > 0) {
    max_idx <- which.max(remainders)
    allocations[max_idx] <- allocations[max_idx] + 1
    remainders[max_idx] <- -1  # Mark as used
    remainder <- remainder - 1
  }

  lapply(allocations, function(amt) {
    money_from_minor(as.integer(amt), money_obj$currency)
  })
}

#' Parse money from a string.
#'
#' @param money_string String like "USD 100.00" or "$100.00 USD"
#' @param default_currency Default currency if not specified
#' @return Money object or NULL on parse failure
#' @export
parse_money <- function(money_string, default_currency = "USD") {
  if (is.na(money_string) || is.null(money_string)) {
    return(NULL)
  }

  # Remove common currency symbols
  cleaned <- gsub("[$\u20ac\u00a3\u00a5]", "", money_string)

  # Remove commas and spaces used as thousand separators
  cleaned <- gsub(",", "", cleaned)
  cleaned <- trimws(cleaned)

  # Try to extract currency code and amount
  pattern <- "^([A-Z]{3})?\\s*([+-]?[0-9.]+)\\s*([A-Z]{3})?$"
  match_result <- regmatches(cleaned, regexec(pattern, cleaned, ignore.case = TRUE))[[1]]

  if (length(match_result) < 3) {
    return(NULL)
  }

  currency <- if (nchar(match_result[2]) > 0) {
    toupper(match_result[2])
  } else if (length(match_result) > 3 && nchar(match_result[4]) > 0) {
    toupper(match_result[4])
  } else {
    toupper(default_currency)
  }

  amount <- as.numeric(match_result[3])

  if (is.na(amount)) {
    return(NULL)
  }

  money(amount, currency)
}
