# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe color handling with validation and WCAG contrast calculations.
#'
#' Provides RGB/RGBA color types with safe conversions and
#' accessibility-focused contrast ratio calculations.

#' Create an RGB color object.
#'
#' @param r Red component (0-255)
#' @param g Green component (0-255)
#' @param b Blue component (0-255)
#' @return RGB S3 object or NULL if invalid
#' @export
rgb_color <- function(r, g, b) {
  if (is.na(r) || is.na(g) || is.na(b)) return(NULL)
  if (r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255) return(NULL)

  structure(
    list(r = as.integer(r), g = as.integer(g), b = as.integer(b)),
    class = "RGB"
  )
}

#' Create an RGBA color object.
#'
#' @param r Red component (0-255)
#' @param g Green component (0-255)
#' @param b Blue component (0-255)
#' @param a Alpha component (0-255)
#' @return RGBA S3 object or NULL if invalid
#' @export
rgba_color <- function(r, g, b, a) {
  if (is.na(r) || is.na(g) || is.na(b) || is.na(a)) return(NULL)
  if (r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 || a < 0 || a > 255) {
    return(NULL)
  }

  structure(
    list(r = as.integer(r), g = as.integer(g), b = as.integer(b), a = as.integer(a)),
    class = "RGBA"
  )
}

#' Check if object is an RGB color.
#'
#' @param x Object to check
#' @return TRUE if x is an RGB object
#' @export
is_rgb <- function(x) {
  inherits(x, "RGB")
}

#' Check if object is an RGBA color.
#'
#' @param x Object to check
#' @return TRUE if x is an RGBA object
#' @export
is_rgba <- function(x) {
  inherits(x, "RGBA")
}

#' Format RGB color.
#'
#' @param x RGB object
#' @param ... Additional arguments (ignored)
#' @return Hex color string
#' @export
format.RGB <- function(x, ...) {
  rgb_to_hex(x)
}

#' Print RGB color.
#'
#' @param x RGB object
#' @param ... Additional arguments (ignored)
#' @export
print.RGB <- function(x, ...) {
  cat("RGB:", rgb_to_hex(x), sprintf("(%d, %d, %d)\n", x$r, x$g, x$b))
  invisible(x)
}

#' Format RGBA color.
#'
#' @param x RGBA object
#' @param ... Additional arguments (ignored)
#' @return Hex color string with alpha
#' @export
format.RGBA <- function(x, ...) {
  sprintf("#%02X%02X%02X%02X", x$r, x$g, x$b, x$a)
}

#' Print RGBA color.
#'
#' @param x RGBA object
#' @param ... Additional arguments (ignored)
#' @export
print.RGBA <- function(x, ...) {
  cat("RGBA:", format(x), sprintf("(%d, %d, %d, %d)\n", x$r, x$g, x$b, x$a))
  invisible(x)
}

#' Create RGB from hex string.
#'
#' @param hex Hex color string (e.g., "#FF0000" or "FF0000")
#' @return RGB object or NULL on error
#' @export
rgb_from_hex <- function(hex) {
  if (is.na(hex) || is.null(hex)) return(NULL)

  clean <- gsub("^#", "", hex)
  if (nchar(clean) != 6) return(NULL)

  if (!grepl("^[0-9a-fA-F]{6}$", clean)) return(NULL)

  r <- strtoi(substr(clean, 1, 2), base = 16L)
  g <- strtoi(substr(clean, 3, 4), base = 16L)
  b <- strtoi(substr(clean, 5, 6), base = 16L)

  rgb_color(r, g, b)
}

#' Convert RGB to hex string.
#'
#' @param color RGB object
#' @return Hex string or NA
#' @export
rgb_to_hex <- function(color) {
  if (!is_rgb(color)) return(NA_character_)
  sprintf("#%02X%02X%02X", color$r, color$g, color$b)
}

#' Gamma correction for luminance calculation.
#'
#' @param value Linear value (0-1)
#' @return Gamma-corrected value
#' @keywords internal
gamma_correct <- function(value) {
  if (value <= 0.03928) {
    value / 12.92
  } else {
    ((value + 0.055) / 1.055) ^ 2.4
  }
}

#' Calculate relative luminance (WCAG formula).
#'
#' @param color RGB object
#' @return Luminance value (0-1) or NA
#' @export
color_luminance <- function(color) {
  if (!is_rgb(color)) return(NA_real_)

  r <- gamma_correct(color$r / 255)
  g <- gamma_correct(color$g / 255)
  b <- gamma_correct(color$b / 255)

  0.2126 * r + 0.7152 * g + 0.0722 * b
}

#' Calculate WCAG contrast ratio between two colors.
#'
#' @param color1 First RGB color
#' @param color2 Second RGB color
#' @return Contrast ratio (1-21) or NA
#' @export
contrast_ratio <- function(color1, color2) {
  if (!is_rgb(color1) || !is_rgb(color2)) return(NA_real_)

  l1 <- color_luminance(color1)
  l2 <- color_luminance(color2)

  lighter <- max(l1, l2)
  darker <- min(l1, l2)

  (lighter + 0.05) / (darker + 0.05)
}

#' Check if contrast meets WCAG AA standard (4.5:1 for normal text).
#'
#' @param color1 First RGB color
#' @param color2 Second RGB color
#' @return TRUE if meets AA
#' @export
meets_wcag_aa <- function(color1, color2) {
  ratio <- contrast_ratio(color1, color2)
  !is.na(ratio) && ratio >= 4.5
}

#' Check if contrast meets WCAG AAA standard (7:1 for normal text).
#'
#' @param color1 First RGB color
#' @param color2 Second RGB color
#' @return TRUE if meets AAA
#' @export
meets_wcag_aaa <- function(color1, color2) {
  ratio <- contrast_ratio(color1, color2)
  !is.na(ratio) && ratio >= 7.0
}

#' Blend foreground RGBA with background RGB.
#'
#' @param fg Foreground RGBA color
#' @param bg Background RGB color
#' @return Blended RGB color or NULL
#' @export
color_blend <- function(fg, bg) {
  if (!is_rgba(fg) || !is_rgb(bg)) return(NULL)

  alpha <- fg$a / 255
  inv_alpha <- 1 - alpha

  rgb_color(
    round(fg$r * alpha + bg$r * inv_alpha),
    round(fg$g * alpha + bg$g * inv_alpha),
    round(fg$b * alpha + bg$b * inv_alpha)
  )
}

#' Convert RGBA to RGB by removing alpha.
#'
#' @param color RGBA object
#' @return RGB object
#' @export
rgba_to_rgb <- function(color) {
  if (!is_rgba(color)) return(NULL)
  rgb_color(color$r, color$g, color$b)
}

#' Convert RGB to RGBA with alpha.
#'
#' @param color RGB object
#' @param alpha Alpha value (0-255)
#' @return RGBA object
#' @export
rgb_to_rgba <- function(color, alpha = 255) {
  if (!is_rgb(color)) return(NULL)
  rgba_color(color$r, color$g, color$b, alpha)
}

#' Lighten a color.
#'
#' @param color RGB object
#' @param amount Amount to lighten (0-1)
#' @return Lightened RGB color
#' @export
color_lighten <- function(color, amount) {
  if (!is_rgb(color) || is.na(amount)) return(NULL)
  amount <- max(0, min(1, amount))

  rgb_color(
    round(color$r + (255 - color$r) * amount),
    round(color$g + (255 - color$g) * amount),
    round(color$b + (255 - color$b) * amount)
  )
}

#' Darken a color.
#'
#' @param color RGB object
#' @param amount Amount to darken (0-1)
#' @return Darkened RGB color
#' @export
color_darken <- function(color, amount) {
  if (!is_rgb(color) || is.na(amount)) return(NULL)
  amount <- max(0, min(1, amount))

  rgb_color(
    round(color$r * (1 - amount)),
    round(color$g * (1 - amount)),
    round(color$b * (1 - amount))
  )
}

#' Invert a color.
#'
#' @param color RGB object
#' @return Inverted RGB color
#' @export
color_invert <- function(color) {
  if (!is_rgb(color)) return(NULL)
  rgb_color(255 - color$r, 255 - color$g, 255 - color$b)
}

#' Convert RGB to grayscale.
#'
#' @param color RGB object
#' @return Grayscale RGB color
#' @export
color_grayscale <- function(color) {
  if (!is_rgb(color)) return(NULL)

  gray <- round(0.299 * color$r + 0.587 * color$g + 0.114 * color$b)
  rgb_color(gray, gray, gray)
}

#' Common color constants.
#' @export
COLOR_BLACK <- rgb_color(0, 0, 0)
#' @export
COLOR_WHITE <- rgb_color(255, 255, 255)
#' @export
COLOR_RED <- rgb_color(255, 0, 0)
#' @export
COLOR_GREEN <- rgb_color(0, 255, 0)
#' @export
COLOR_BLUE <- rgb_color(0, 0, 255)
#' @export
COLOR_YELLOW <- rgb_color(255, 255, 0)
#' @export
COLOR_CYAN <- rgb_color(0, 255, 255)
#' @export
COLOR_MAGENTA <- rgb_color(255, 0, 255)
