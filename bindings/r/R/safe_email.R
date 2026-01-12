# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe email validation and parsing.

# Common disposable email domains
.DISPOSABLE_DOMAINS <- c(
  "mailinator.com", "guerrillamail.com", "tempmail.com", "throwaway.email",
  "10minutemail.com", "fakeinbox.com", "trashmail.com", "maildrop.cc",
  "yopmail.com", "sharklasers.com"
)

#' Create a successful EmailResult.
#'
#' @param local_part Local part of email
#' @param domain Domain part of email
#' @return An EmailResult list
email_ok <- function(local_part, domain) {
  list(ok = TRUE, local_part = local_part, domain = domain, error = NULL)
}

#' Create an error EmailResult.
#'
#' @param message Error message
#' @return An EmailResult list
email_error <- function(message) {
  list(ok = FALSE, local_part = NULL, domain = NULL, error = message)
}

#' Check if email format is valid.
#'
#' @param email Email to validate
#' @return TRUE if valid
#' @export
is_valid_email <- function(email) {
  if (is.na(email) || nchar(email) == 0) return(FALSE)
  if (nchar(email) > 254) return(FALSE)

  # Must contain exactly one @
  at_count <- length(gregexpr("@", email, fixed = TRUE)[[1]])
  if (at_count != 1) return(FALSE)

  parts <- strsplit(email, "@", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(FALSE)

  local_part <- parts[1]
  domain <- parts[2]

  if (nchar(local_part) == 0 || nchar(local_part) > 64) return(FALSE)
  if (nchar(domain) == 0 || nchar(domain) > 253) return(FALSE)

  # Basic format check
  pattern <- "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)*$"
  grepl(pattern, email)
}

#' Parse and validate email address.
#'
#' @param email Email to parse
#' @return EmailResult list
#' @export
parse_email <- function(email) {
  if (is.na(email) || nchar(email) == 0) {
    return(email_error("Email is empty"))
  }
  if (nchar(email) > 254) {
    return(email_error("Email too long"))
  }

  at_pos <- regexpr("@", email, fixed = TRUE)
  if (at_pos == -1) {
    return(email_error("Missing @ symbol"))
  }

  local_part <- substr(email, 1, at_pos - 1)
  domain <- substr(email, at_pos + 1, nchar(email))

  if (nchar(local_part) == 0) {
    return(email_error("Local part is empty"))
  }
  if (nchar(local_part) > 64) {
    return(email_error("Local part too long"))
  }
  if (nchar(domain) == 0) {
    return(email_error("Domain is empty"))
  }
  if (nchar(domain) > 253) {
    return(email_error("Domain too long"))
  }

  if (!is_valid_email(email)) {
    return(email_error("Invalid email format"))
  }

  email_ok(local_part, domain)
}

#' Check if domain is a disposable email provider.
#'
#' @param email Email to check
#' @return TRUE if disposable
#' @export
is_disposable_email <- function(email) {
  if (is.na(email)) return(FALSE)

  at_pos <- regexpr("@", email, fixed = TRUE)
  if (at_pos == -1) return(FALSE)

  domain <- tolower(substr(email, at_pos + 1, nchar(email)))
  domain %in% .DISPOSABLE_DOMAINS
}

#' Normalize email address.
#'
#' @param email Email to normalize
#' @return Normalized email or NA
#' @export
normalize_email <- function(email) {
  result <- parse_email(email)
  if (!result$ok) return(NA)

  paste0(result$local_part, "@", tolower(result$domain))
}

#' Get domain from email.
#'
#' @param email Email address
#' @return Domain or NA
#' @export
get_email_domain <- function(email) {
  if (is.na(email)) return(NA)

  at_pos <- regexpr("@", email, fixed = TRUE)
  if (at_pos == -1) return(NA)

  substr(email, at_pos + 1, nchar(email))
}

#' Get local part from email.
#'
#' @param email Email address
#' @return Local part or NA
#' @export
get_local_part <- function(email) {
  if (is.na(email)) return(NA)

  at_pos <- regexpr("@", email, fixed = TRUE)
  if (at_pos == -1) return(NA)

  substr(email, 1, at_pos - 1)
}

#' Mask email for display.
#'
#' @param email Email to mask
#' @return Masked email or NA
#' @export
mask_email <- function(email) {
  result <- parse_email(email)
  if (!result$ok) return(NA)

  local <- result$local_part
  domain <- result$domain

  if (nchar(local) <= 1) {
    paste0(local, "***@", domain)
  } else {
    paste0(substr(local, 1, 1), "***@", domain)
  }
}

#' Check if two emails are equal (case-insensitive domain).
#'
#' @param email1 First email
#' @param email2 Second email
#' @return TRUE if equal
#' @export
emails_equal <- function(email1, email2) {
  norm1 <- normalize_email(email1)
  norm2 <- normalize_email(email2)

  if (is.na(norm1) || is.na(norm2)) return(FALSE)

  norm1 == norm2
}
