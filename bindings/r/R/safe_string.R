# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe string operations for XSS prevention and sanitization.

#' Escape HTML special characters.
#'
#' @param input String to escape
#' @return Escaped string
#' @export
escape_html <- function(input) {
  if (is.na(input)) return(NA)

  result <- input
  result <- gsub("&", "&amp;", result, fixed = TRUE)
  result <- gsub("<", "&lt;", result, fixed = TRUE)
  result <- gsub(">", "&gt;", result, fixed = TRUE)
  result <- gsub('"', "&quot;", result, fixed = TRUE)
  result <- gsub("'", "&#x27;", result, fixed = TRUE)

  result
}

#' Escape for SQL (single quotes).
#'
#' @param input String to escape
#' @return Escaped string
#' @export
escape_sql <- function(input) {
  if (is.na(input)) return(NA)
  gsub("'", "''", input, fixed = TRUE)
}

#' Escape for JavaScript strings.
#'
#' @param input String to escape
#' @return Escaped string
#' @export
escape_js <- function(input) {
  if (is.na(input)) return(NA)

  result <- input
  result <- gsub("\\", "\\\\", result, fixed = TRUE)
  result <- gsub('"', '\\"', result, fixed = TRUE)
  result <- gsub("'", "\\'", result, fixed = TRUE)
  result <- gsub("\n", "\\n", result, fixed = TRUE)
  result <- gsub("\r", "\\r", result, fixed = TRUE)
  result <- gsub("\t", "\\t", result, fixed = TRUE)
  result <- gsub("<", "\\x3c", result, fixed = TRUE)
  result <- gsub(">", "\\x3e", result, fixed = TRUE)

  result
}

#' URL encode a string.
#'
#' @param input String to encode
#' @return Encoded string
#' @export
url_encode <- function(input) {
  if (is.na(input)) return(NA)
  utils::URLencode(input, reserved = TRUE)
}

#' URL decode a string.
#'
#' @param input String to decode
#' @return Decoded string or NA on error
#' @export
url_decode <- function(input) {
  if (is.na(input)) return(NA)
  tryCatch(
    utils::URLdecode(input),
    error = function(e) NA
  )
}

#' Sanitize string to only allow safe characters.
#'
#' @param input String to sanitize
#' @param allowed Pattern of allowed characters (regex character class)
#' @return Sanitized string
#' @export
sanitize <- function(input, allowed = "a-zA-Z0-9_-") {
  if (is.na(input)) return(NA)
  gsub(paste0("[^", allowed, "]"), "", input)
}

#' Default sanitization (alphanumeric + underscore + hyphen).
#'
#' @param input String to sanitize
#' @return Sanitized string
#' @export
sanitize_default <- function(input) {
  sanitize(input, "a-zA-Z0-9_-")
}

#' Convert to slug.
#'
#' @param input String to slugify
#' @return Slugified string
#' @export
slugify <- function(input) {
  if (is.na(input)) return(NA)

  result <- tolower(input)
  # Remove non-alphanumeric except spaces and hyphens
  result <- gsub("[^a-z0-9 -]", "", result)
  # Replace spaces with hyphens
  result <- gsub(" ", "-", result)
  # Collapse multiple hyphens
  result <- gsub("-+", "-", result)
  # Trim hyphens
  result <- gsub("^-+|-+$", "", result)

  result
}

#' Truncate string safely.
#'
#' @param input String to truncate
#' @param max_length Maximum length
#' @param suffix Suffix to add (default "...")
#' @return Truncated string
#' @export
truncate <- function(input, max_length, suffix = "...") {
  if (is.na(input)) return(NA)
  if (nchar(input) <= max_length) return(input)
  if (max_length <= nchar(suffix)) return(suffix)

  paste0(substr(input, 1, max_length - nchar(suffix)), suffix)
}

#' Remove control characters.
#'
#' @param input String to clean
#' @return Cleaned string
#' @export
strip_control_chars <- function(input) {
  if (is.na(input)) return(NA)
  gsub("[\\x00-\\x1f\\x7f]", "", input)
}

#' Normalize whitespace.
#'
#' @param input String to normalize
#' @return Normalized string
#' @export
normalize_whitespace <- function(input) {
  if (is.na(input)) return(NA)
  result <- gsub("\\s+", " ", input)
  trimws(result)
}

#' Check if string contains only ASCII.
#'
#' @param input String to check
#' @return TRUE if ASCII only
#' @export
is_ascii_only <- function(input) {
  if (is.na(input)) return(FALSE)
  !grepl("[^\x01-\x7f]", input)
}

#' Check if string contains only printable ASCII.
#'
#' @param input String to check
#' @return TRUE if printable ASCII only
#' @export
is_printable_ascii <- function(input) {
  if (is.na(input)) return(FALSE)
  !grepl("[^\x20-\x7e]", input)
}

#' Base64 encode.
#'
#' @param input String to encode
#' @return Encoded string
#' @export
base64_encode <- function(input) {
  if (is.na(input)) return(NA)
  base64enc::base64encode(charToRaw(input))
}

#' Base64 decode.
#'
#' @param input String to decode
#' @return Decoded string or NA on error
#' @export
base64_decode <- function(input) {
  if (is.na(input)) return(NA)
  tryCatch({
    rawToChar(base64enc::base64decode(input))
  }, error = function(e) NA)
}

#' Hex encode bytes.
#'
#' @param input Raw vector to encode
#' @return Hex string
#' @export
hex_encode <- function(input) {
  if (is.null(input)) return(NA)
  paste0(format(as.hexmode(as.integer(input)), width = 2), collapse = "")
}

#' Hex decode to bytes.
#'
#' @param input Hex string to decode
#' @return Raw vector or NA on error
#' @export
hex_decode <- function(input) {
  if (is.na(input)) return(NA)
  if (nchar(input) %% 2 != 0) return(NA)

  tryCatch({
    n <- nchar(input)
    bytes <- sapply(seq(1, n, by = 2), function(i) {
      strtoi(substr(input, i, i + 1), base = 16)
    })
    as.raw(bytes)
  }, error = function(e) NA)
}
