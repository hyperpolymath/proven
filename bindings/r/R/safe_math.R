# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe arithmetic operations with overflow checking.
#'
#' All functions return NA on overflow instead of wrapping.

# Maximum and minimum 64-bit integer values
.MAX_INT64 <- 9223372036854775807
.MIN_INT64 <- -9223372036854775808

#' Add two values with overflow checking.
#'
#' @param a First operand
#' @param b Second operand
#' @return Sum or NA on overflow
#' @export
safe_add <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA)

  # Check for potential overflow
  if (b > 0 && a > .MAX_INT64 - b) return(NA)
  if (b < 0 && a < .MIN_INT64 - b) return(NA)

  a + b
}

#' Subtract two values with overflow checking.
#'
#' @param a First operand
#' @param b Second operand
#' @return Difference or NA on overflow
#' @export
safe_sub <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA)

  # Check for potential overflow
  if (b < 0 && a > .MAX_INT64 + b) return(NA)
  if (b > 0 && a < .MIN_INT64 + b) return(NA)

  a - b
}

#' Multiply two values with overflow checking.
#'
#' @param a First operand
#' @param b Second operand
#' @return Product or NA on overflow
#' @export
safe_mul <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA)
  if (a == 0 || b == 0) return(0)

  result <- a * b

  # Check if overflow occurred
  if (a != 0 && result / a != b) return(NA)

  result
}

#' Divide two values safely.
#'
#' @param a Dividend
#' @param b Divisor
#' @return Quotient or NA on division by zero
#' @export
safe_div <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA)
  if (b == 0) return(NA)

  # Check for overflow (MIN_VALUE / -1)
  if (a == .MIN_INT64 && b == -1) return(NA)

  a %/% b
}

#' Modulo operation with safety checks.
#'
#' @param a Dividend
#' @param b Divisor
#' @return Remainder or NA on division by zero
#' @export
safe_mod <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA)
  if (b == 0) return(NA)

  a %% b
}

#' Absolute value with overflow checking.
#'
#' @param a Value
#' @return Absolute value or NA on overflow
#' @export
safe_abs <- function(a) {
  if (is.na(a)) return(NA)
  if (a == .MIN_INT64) return(NA)

  abs(a)
}

#' Negate with overflow checking.
#'
#' @param a Value
#' @return Negated value or NA on overflow
#' @export
safe_neg <- function(a) {
  if (is.na(a)) return(NA)
  if (a == .MIN_INT64) return(NA)

  -a
}

#' Power operation with overflow checking.
#'
#' @param base Base value
#' @param exp Exponent (must be non-negative)
#' @return Power or NA on overflow
#' @export
safe_pow <- function(base, exp) {
  if (is.na(base) || is.na(exp)) return(NA)
  if (exp < 0) return(NA)
  if (exp == 0) return(1)
  if (exp == 1) return(base)

  result <- 1
  b <- base
  e <- exp

  while (e > 0) {
    if (bitwAnd(e, 1) == 1) {
      new_result <- safe_mul(result, b)
      if (is.na(new_result)) return(NA)
      result <- new_result
    }
    e <- bitwShiftR(e, 1)
    if (e > 0) {
      new_b <- safe_mul(b, b)
      if (is.na(new_b)) return(NA)
      b <- new_b
    }
  }

  result
}

#' Safe sum of a vector.
#'
#' @param values Numeric vector
#' @return Sum or NA on overflow
#' @export
safe_sum <- function(values) {
  result <- 0
  for (v in values) {
    new_result <- safe_add(result, v)
    if (is.na(new_result)) return(NA)
    result <- new_result
  }
  result
}

#' Safe product of a vector.
#'
#' @param values Numeric vector
#' @return Product or NA on overflow
#' @export
safe_product <- function(values) {
  result <- 1
  for (v in values) {
    new_result <- safe_mul(result, v)
    if (is.na(new_result)) return(NA)
    result <- new_result
  }
  result
}

#' Clamp value to range.
#'
#' @param value Value to clamp
#' @param min_val Minimum value
#' @param max_val Maximum value
#' @return Clamped value
#' @export
clamp <- function(value, min_val, max_val) {
  if (value < min_val) return(min_val)
  if (value > max_val) return(max_val)
  value
}

#' Check if value is in range.
#'
#' @param value Value to check
#' @param min_val Minimum value
#' @param max_val Maximum value
#' @return TRUE if in range
#' @export
in_range <- function(value, min_val, max_val) {
  value >= min_val && value <= max_val
}
