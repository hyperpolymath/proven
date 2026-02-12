# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe floating-point operations with NaN/Infinity prevention.
#'
#' Provides mathematically verified safe operations that prevent
#' NaN, Infinity, or crashes in numerical computing.

#' Minimum positive value to consider non-zero.
#' @export
FLOAT_EPSILON <- 1e-10

#' Safe division with zero check.
#'
#' Returns NA if divisor is zero or too small.
#'
#' @param a Dividend
#' @param b Divisor
#' @return Quotient or NA on error
#' @export
safe_div <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA_real_)
  if (abs(b) < FLOAT_EPSILON) return(NA_real_)

  result <- a / b
  if (!is.finite(result)) return(NA_real_)

  result
}

#' Safe natural logarithm.
#'
#' Returns NA for non-positive values.
#'
#' @param x Value
#' @return Natural log or NA on error
#' @export
safe_ln <- function(x) {
  if (is.na(x) || x <= 0) return(NA_real_)
  log(x)
}

#' Safe log base 10.
#'
#' Returns NA for non-positive values.
#'
#' @param x Value
#' @return Log base 10 or NA on error
#' @export
safe_log10 <- function(x) {
  if (is.na(x) || x <= 0) return(NA_real_)
  log10(x)
}

#' Safe square root.
#'
#' Returns NA for negative values.
#'
#' @param x Value
#' @return Square root or NA on error
#' @export
safe_sqrt <- function(x) {
  if (is.na(x) || x < 0) return(NA_real_)
  sqrt(x)
}

#' Safe power operation.
#'
#' Returns NA if result is not finite.
#'
#' @param base Base value
#' @param exp Exponent
#' @return Power or NA on error
#' @export
safe_pow <- function(base, exp) {
  if (is.na(base) || is.na(exp)) return(NA_real_)

  result <- base ^ exp
  if (!is.finite(result)) return(NA_real_)

  result
}

#' Safe exponential (e^x).
#'
#' Returns NA if result overflows.
#'
#' @param x Exponent
#' @return Exponential or NA on overflow
#' @export
safe_exp <- function(x) {
  if (is.na(x)) return(NA_real_)

  result <- exp(x)
  if (!is.finite(result)) return(NA_real_)

  result
}

#' Compute vector magnitude (L2 norm).
#'
#' @param v Numeric vector
#' @return Magnitude
#' @export
float_magnitude <- function(v) {
  if (length(v) == 0) return(0)
  sqrt(sum(v^2))
}

#' Safe vector normalization (unit vector).
#'
#' Returns NA if vector has zero magnitude.
#'
#' @param v Numeric vector
#' @return Normalized vector or NA on error
#' @export
float_normalize <- function(v) {
  if (length(v) == 0) return(NA)

  mag <- float_magnitude(v)
  if (mag < FLOAT_EPSILON) return(NA)

  v / mag
}

#' Check if a float is finite (not NaN or Infinity).
#'
#' @param x Value to check
#' @return TRUE if finite
#' @export
float_is_finite <- function(x) {
  !is.na(x) && is.finite(x)
}

#' Check if a float is safe for division.
#'
#' @param x Value to check
#' @return TRUE if safe divisor
#' @export
float_is_safe_divisor <- function(x) {
  float_is_finite(x) && abs(x) >= FLOAT_EPSILON
}

#' Clamp a float to a range, handling NaN.
#'
#' Unlike base clamp, this safely handles NaN by returning min.
#'
#' @param value Value to clamp
#' @param min_val Minimum value
#' @param max_val Maximum value
#' @return Clamped value
#' @export
float_clamp <- function(value, min_val, max_val) {
  if (is.na(value) || !is.finite(value)) return(min_val)
  if (value < min_val) return(min_val)
  if (value > max_val) return(max_val)
  value
}

#' Safe reciprocal (1/x).
#'
#' @param x Value
#' @return Reciprocal or NA on error
#' @export
safe_reciprocal <- function(x) {
  safe_div(1, x)
}

#' Compute mean of a vector safely.
#'
#' Returns NA for empty vectors.
#'
#' @param v Numeric vector
#' @return Mean or NA on error
#' @export
safe_mean <- function(v) {
  if (length(v) == 0) return(NA_real_)
  if (any(is.na(v))) return(NA_real_)

  sum(v) / length(v)
}

#' Compute variance safely.
#'
#' @param v Numeric vector
#' @return Variance or NA on error
#' @export
safe_variance <- function(v) {
  mean_val <- safe_mean(v)
  if (is.na(mean_val)) return(NA_real_)

  sum_sq <- sum((v - mean_val)^2)
  safe_div(sum_sq, length(v))
}

#' Compute standard deviation safely.
#'
#' @param v Numeric vector
#' @return Standard deviation or NA on error
#' @export
safe_std_dev <- function(v) {
  var_val <- safe_variance(v)
  if (is.na(var_val)) return(NA_real_)

  safe_sqrt(var_val)
}

#' Safe sigmoid function.
#'
#' @param x Input value
#' @return Sigmoid output (0-1) or NA on error
#' @export
safe_sigmoid <- function(x) {
  if (is.na(x)) return(NA_real_)

  # Prevent overflow in exp
  if (x < -700) return(0)
  if (x > 700) return(1)

  1 / (1 + exp(-x))
}

#' Safe softmax function.
#'
#' @param v Numeric vector
#' @return Softmax probabilities or NA on error
#' @export
safe_softmax <- function(v) {
  if (length(v) == 0) return(NA)
  if (any(is.na(v))) return(NA)

  # Subtract max for numerical stability
  v_shifted <- v - max(v)
  exp_v <- exp(v_shifted)
  total <- sum(exp_v)

  if (total < FLOAT_EPSILON) return(NA)

  exp_v / total
}

#' Safe log-sum-exp.
#'
#' @param v Numeric vector
#' @return Log-sum-exp or NA on error
#' @export
safe_logsumexp <- function(v) {
  if (length(v) == 0) return(NA_real_)
  if (any(is.na(v))) return(NA_real_)

  max_v <- max(v)
  max_v + log(sum(exp(v - max_v)))
}

#' Compute dot product safely.
#'
#' @param a First vector
#' @param b Second vector
#' @return Dot product or NA if lengths differ
#' @export
safe_dot_product <- function(a, b) {
  if (length(a) != length(b)) return(NA_real_)
  if (length(a) == 0) return(0)
  if (any(is.na(a)) || any(is.na(b))) return(NA_real_)

  sum(a * b)
}

#' Compute cosine similarity safely.
#'
#' @param a First vector
#' @param b Second vector
#' @return Cosine similarity (-1 to 1) or NA on error
#' @export
safe_cosine_similarity <- function(a, b) {
  if (length(a) != length(b)) return(NA_real_)

  dot <- safe_dot_product(a, b)
  if (is.na(dot)) return(NA_real_)

  mag_a <- float_magnitude(a)
  mag_b <- float_magnitude(b)

  if (mag_a < FLOAT_EPSILON || mag_b < FLOAT_EPSILON) {
    return(NA_real_)
  }

  float_clamp(dot / (mag_a * mag_b), -1, 1)
}

#' Compute Euclidean distance safely.
#'
#' @param a First vector
#' @param b Second vector
#' @return Distance or NA on error
#' @export
safe_euclidean_distance <- function(a, b) {
  if (length(a) != length(b)) return(NA_real_)
  if (any(is.na(a)) || any(is.na(b))) return(NA_real_)

  sqrt(sum((a - b)^2))
}

#' Linear interpolation (lerp).
#'
#' @param a Start value
#' @param b End value
#' @param t Interpolation factor (0-1)
#' @return Interpolated value
#' @export
float_lerp <- function(a, b, t) {
  if (is.na(a) || is.na(b) || is.na(t)) return(NA_real_)
  t <- float_clamp(t, 0, 1)
  a + (b - a) * t
}

#' Check if two floats are approximately equal.
#'
#' @param a First value
#' @param b Second value
#' @param epsilon Tolerance (default FLOAT_EPSILON)
#' @return TRUE if approximately equal
#' @export
float_approx_equal <- function(a, b, epsilon = FLOAT_EPSILON) {
  if (is.na(a) || is.na(b)) return(FALSE)
  abs(a - b) < epsilon
}
